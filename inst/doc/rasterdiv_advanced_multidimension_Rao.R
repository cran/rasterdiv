## ----setup, include=FALSE ,results='hide'-------------------------------------
knitr::opts_chunk$set(fig.width=10, fig.height=10,fig.asp = 0.618, out.width = "95%", fig.align = "center", fig.dpi = 150, collapse = FALSE, comment = "#") 
#knitr::opts_chunk$set(dev = 'pdf')

## ----results='hide', message=FALSE, warning=FALSE-----------------------------
library(raster)
library(gstat)
library(rasterdiv)

## -----------------------------------------------------------------------------
gridDim <- 40 # 10000m/250 m = 40 columns and rows
xy <- expand.grid(x=1:gridDim, y=1:gridDim)

## -----------------------------------------------------------------------------
varioMod <- vgm(psill=0.005, range=100, model='Exp') # psill=partial sill=(sill-nugget)
# Set up an additional variable from simple kriging
zDummy <- gstat(formula=z~1, 
                locations = ~x+y, 
                dummy=TRUE,
                beta=200, 
                model=varioMod, 
                nmax=1)
# Generate a randomly autocorrelated predictor data field
set.seed(123)
xyz <- predict(zDummy, newdata=xy, nsim=2)

## -----------------------------------------------------------------------------
utm32N <- "+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
r <- raster(nrow=40, ncol=40, crs=utm32N, ext=extent(0,10000, 0,10000))
r1 <- raster(nrow=40, ncol=40, crs=utm32N, ext=extent(0,10000, 0,10000))
values(r)=xyz$sim1
values(r1)=xyz$sim2

## ----results='hide', message=FALSE, warning=FALSE-----------------------------
mRao <- paRao(x=list(r,r1), window=c(3,5), alpha=c(1,Inf), na.tolerance=1, method="multidimension",simplify=3, debugging=TRUE)

## -----------------------------------------------------------------------------
plot(stack(r,r1,stack(mRao[[1]]),stack(mRao[[2]])), nc = 2, nr = 3)

