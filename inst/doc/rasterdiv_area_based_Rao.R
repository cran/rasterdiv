## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(fig.width=10, fig.height=10,fig.asp = 0.618, out.width = "95%", fig.align = "center", fig.dpi = 150, collapse = FALSE, comment = "#") 
#knitr::opts_chunk$set(dev = 'pdf')

## ----results='hide', message=FALSE, warning=FALSE-----------------------------
require(rasterdiv)
require(rasterVis)
require(RColorBrewer)

## -----------------------------------------------------------------------------
copNDVI <- raster::reclassify(copNDVI, cbind(252,255, NA), right=TRUE)

## -----------------------------------------------------------------------------
#Resample using raster::aggregate and a linear factor of 10
copNDVIlr <- raster::aggregate(copNDVI, fact=10)
#Set float numbers as integers to further speed up the calculation
storage.mode(copNDVIlr[]) = "integer"

## ----echo = T, results = 'hide', warning=FALSE, message=FALSE-----------------
RaoC <- paRao(x=copNDVIlr, area=world, field='CONTINENT', alpha=c(1,2))

## ----fig01, warning=FALSE, message=FALSE--------------------------------------
#Plot classic area-based Rao's index
plot(RaoC, col=hcl(RaoC$alpha.1*10), main="Classic Rao's index per continent alpha 1")
text(RaoC, label=paste("Rao'Q =", round(RaoC$alpha.1,1)), col="black", family="Arial", halo=TRUE)

## ----echo = T, results = 'hide', warning=FALSE, message=FALSE-----------------
# The second layers is NDVI/100 (just as an example)
RaoMC <- paRao(x=stack(copNDVIlr,copNDVIlr/2), area=world, field='CONTINENT', alpha=5, method="multidimension", simplify=1)

## ----fig02, warning=FALSE, message=FALSE--------------------------------------
#Plot area-based RAo's index
plot(RaoMC, col=hcl(RaoMC$alpha.5*10000), main="Multidimension Rao's index per continent alpha 5")
text(RaoMC, label=paste("Rao'Q =", round(RaoMC$alpha.5,3)), col="black", family="Arial", halo=TRUE)

