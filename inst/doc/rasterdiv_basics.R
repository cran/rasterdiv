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
#Resample using raster::aggregate and a linear factor of 20
copNDVIlr <- raster::aggregate(copNDVI, fact=20)
#Set float numbers as integers to further speed up the calculation
storage.mode(copNDVIlr[]) = "integer"
#Cut on Africa and Australia
cont <- subset(world, CONTINENT=="Africa"|CONTINENT=="Oceania")
copNDVIcont <- crop(copNDVIlr, cont)

## ----fig01--------------------------------------------------------------------
levelplot(copNDVI,layout=c(0,1,1), main="NDVI 21st of June 1999-2017 - ~8km pixel resolution")
levelplot(copNDVIlr,layout=c(0,1,1), main="NDVI 21st of June 1999-2017 - ~150km pixel resolution")

## ----echo = T, results = 'hide', warning=FALSE, message=FALSE-----------------
RaoC <- paRao(x=copNDVIlr, area=cont, field='CONTINENT', alpha=c(1,2))

## ----fig02--------------------------------------------------------------------
#Shannon's Diversity
plot(RaoC, col=RaoC$alpha.1, main="Rao's index per continent alpha 1")
text(RaoC, label=paste("Rao =", round(RaoC$alpha.1,1)), col="black")

## ----echo = T, results = 'hide', warning=FALSE, message=FALSE-----------------
#Shannon's Diversity
sha <- Shannon(copNDVIcont,window=9,na.tolerance=0.1,np=1)

#Pielou's Evenness
pie <- Pielou(copNDVIcont,window=9,na.tolerance=0.1,np=1)

#Berger-Parker's Index
ber <- BergerParker(copNDVIcont,window=9,na.tolerance=0.1,np=1)

#Parametric Rao's quadratic entropy with alpha ranging from 1 to 5
prao <- paRao(copNDVIcont,window=9,alpha=1:5,na.tolerance=0.1,dist_m="euclidean",np=1)

#Cumulative Residual Entropy
cre <- CRE(copNDVIcont,window=9,na.tolerance=0.1,np=1)

#Hill's numbers
hil <- Hill(copNDVIcont,window=9,alpha=seq(0,2,0.5),na.tolerance=0.1,np=1)

#Rényi's Index
ren <- Renyi(copNDVIcont,window=9,alpha=seq(0,2,0.5),na.tolerance=0.1,np=1)

## ----fig03--------------------------------------------------------------------
#Shannon's Diversity
levelplot(sha,main="Shannon's entropy from Copernicus NDVI 5 km (9 px-side moving window)",as.table = T,layout=c(0,1,1), ylim=c(-60,75), margin = list(draw = TRUE))

## ----fig04--------------------------------------------------------------------
#Pielou's Evenness
levelplot(pie,main="Pielou's evenness from Copernicus NDVI 5 km (9 px-side moving window)",as.table = T,layout=c(0,1,1), ylim=c(-60,75), margin = list(draw = TRUE))

## ----fig05--------------------------------------------------------------------
#Berger-Parker' Index
levelplot(ber,main="Berger-Parker's index from Copernicus NDVI 5 km (9 px-side moving window)",as.table = T,layout=c(0,1,1), ylim=c(-60,75), margin = list(draw = TRUE))

## ----fig06--------------------------------------------------------------------
#Parametric Rao's quadratic Entropy
levelplot(stack(prao[[1]]),main="Parametric Rao's quadratic entropy from Copernicus NDVI 5 km (9 px-side moving window)",as.table = T,layout=c(0,5,1), ylim=c(-60,75), margin = list(draw = TRUE))

## ----fig07--------------------------------------------------------------------
#Cumulative Residual Entropy
levelplot(cre,main="Cumulative Residual Entropy from Copernicus NDVI 5 km (9 px-side moving window)",as.table = T,layout=c(0,1,1), ylim=c(-60,75), margin = list(draw = TRUE))

## ----fig08--------------------------------------------------------------------
#Hill's numbers (alpha=0, 0.5, 1, 1.5 and 2)
levelplot(stack(hil),main="Hill's numbers from Copernicus NDVI 5 km (9 px-side moving window)",as.table = T,layout=c(0,5,1), ylim=c(-60,75))

## ----fig09--------------------------------------------------------------------
#Renyi' Index (alpha=0, 1, 1.5 and 2)
levelplot(stack(ren),main="Renyi's entropy from Copernicus NDVI 5 km (9 px-side moving window)",as.table = T,layout=c(0,5,1),names.attr=paste("alpha",seq(0,2,0.5),sep=" "), ylim=c(-60,75), margin = list(draw = FALSE))

