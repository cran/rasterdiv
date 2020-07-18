## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(fig.width=10, fig.height=10,fig.asp = 0.618, out.width = "95%", fig.align = "center", fig.dpi = 150, collapse = FALSE, comment = "#") 
#knitr::opts_chunk$set(dev = 'pdf')

## ----results='hide', message=FALSE, warning=FALSE-----------------------------
#Required packages
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

## ----fig01--------------------------------------------------------------------
levelplot(copNDVI,layout=c(0,1,1), main="NDVI 21st of June 1999-2017 - ~8km pixel resolution")
levelplot(copNDVIlr,layout=c(0,1,1), main="NDVI 21st of June 1999-2017 - ~150km pixel resolution")

## ----echo = T, results = 'hide', warning=FALSE, message=FALSE-----------------
#Shannon's Diversity
sha <- Shannon(copNDVIlr,window=9,na.tolerance=0.1,np=1)

#Pielou's Evenness
pie <- Pielou(copNDVIlr,window=9,na.tolerance=0.1,np=1)

#Berger-Parker's Index
ber <- BergerParker(copNDVIlr,window=9,na.tolerance=0.1,np=1)

#Rao's quadratic Entropy
rao <- Rao(copNDVIlr,window=9,na.tolerance=0.1,dist_m="euclidean",shannon=FALSE,np=1)

#Parametric Rao's quadratic entropy with alpha ranging from 1 to 5
prao <- paRao(copNDVIlr,window=9,alpha=1:5,na.tolerance=0.1,dist_m="euclidean",np=1)

#Cumulative Residual Entropy
cre <- CRE(copNDVIlr,window=9,na.tolerance=0.1,np=1)

#Hill's numbers
hil <- Hill(copNDVIlr,window=9,alpha=seq(0,2,0.5),na.tolerance=0.1,np=1)

#Renyi's Index
ren <- Renyi(copNDVIlr,window=9,alpha=seq(0,2,0.5),na.tolerance=0.1,np=1)

## ----fig02--------------------------------------------------------------------
#Shannon's Diversity
levelplot(sha,main="Shannon's entropy from Copernicus NDVI 5 km (9 px-side moving window)",as.table = T,layout=c(0,1,1), ylim=c(-60,75), margin = list(draw = TRUE))

## ----fig03--------------------------------------------------------------------
#Pielou's Evenness
levelplot(pie,main="Pielou's evenness from Copernicus NDVI 5 km (9 px-side moving window)",as.table = T,layout=c(0,1,1), ylim=c(-60,75), margin = list(draw = TRUE))

## ----fig04--------------------------------------------------------------------
#Berger-Parker' Index
levelplot(ber,main="Berger-Parker's index from Copernicus NDVI 5 km (9 px-side moving window)",as.table = T,layout=c(0,1,1), ylim=c(-60,75), margin = list(draw = TRUE))

## ----fig05--------------------------------------------------------------------
#Rao's quadratic Entropy
levelplot(rao,main="Rao's quadratic entropy from Copernicus NDVI 5 km (9 px-side moving window)",as.table = T,layout=c(0,1,1), ylim=c(-60,75), margin = list(draw = TRUE))

## ----fig06--------------------------------------------------------------------
#Parametric Rao's quadratic Entropy
levelplot(stack(prao),main="Parametric Rao's quadratic entropy from Copernicus NDVI 5 km (9 px-side moving window)",as.table = T,layout=c(0,5,1), ylim=c(-60,75), margin = list(draw = TRUE))

## ----fig07--------------------------------------------------------------------
#Cumulative Residual Entropy
levelplot(cre,main="Cumulative Residual Entropy from Copernicus NDVI 5 km (9 px-side moving window)",as.table = T,layout=c(0,1,1), ylim=c(-60,75), margin = list(draw = TRUE))

## ----fig08--------------------------------------------------------------------
#Hill's numbers (alpha=0, 1, 1.5 and 2)
levelplot(stack(hil),main="Hill's numbers from Copernicus NDVI 5 km (9 px-side moving window)",as.table = T,layout=c(0,5,1),names.attr=paste("alpha",seq(0,2,0.5),sep=" "), ylim=c(-60,75))

## ----fig09--------------------------------------------------------------------
#Renyi' Index (alpha=0, 1, 1.5 and 2)
levelplot(stack(ren),main="Renyi's entropy from Copernicus NDVI 5 km (9 px-side moving window)",as.table = T,layout=c(0,5,1),names.attr=paste("alpha",seq(0,2,0.5),sep=" "), ylim=c(-60,75), margin = list(draw = FALSE))

