## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(fig.width=10, fig.height=10,fig.asp = 0.618, out.width = "95%", fig.align = "center") 
knitr::opts_chunk$set(dev = 'pdf')

## ----results='hide', message=FALSE, warning=FALSE-----------------------------
#Required packages
library(rasterdiv)
library(rasterVis)
library(RColorBrewer)

## -----------------------------------------------------------------------------
copNDVI <- reclassify(copNDVI, cbind(253, 255, NA), right=TRUE)

## -----------------------------------------------------------------------------
#Resample using raster::aggregate and a linear factor of 10
copNDVIlr <- raster::aggregate(copNDVI, fact=20)
#Set float numbers as integers to further speed up the calculation
storage.mode(copNDVIlr[]) = "integer"

## ----fig01--------------------------------------------------------------------
levelplot(copNDVI,layout=c(0,1,1), main="NDVI 21st of June 1999-2017 - ~8km pixel resolution")
levelplot(copNDVIlr,layout=c(0,1,1), main="NDVI 21st of June 1999-2017 - ~150km pixel resolution")

## ----echo = T, results = 'hide', warning=FALSE, message=FALSE-----------------
#Shannon's Diversity
sha <- Shannon(copNDVIlr,window=9,np=1,na.tolerance=0.1)

#Pielou's Evenness
pie <- Pielou(copNDVIlr,window=9,np=1,na.tolerance=0.1)

#Berger-Parker
ber <- BergerParker(copNDVIlr,window=9,np=1,na.tolerance=0.1)

#Rao's quadratic Entropy
rao <- Rao(copNDVIlr,window=9,np=1,na.tolerance=0.1,dist_m="euclidean",shannon=FALSE)

#Cumulative Residual Entropy
cre <- CRE(copNDVIlr,window=9,np=1,na.tolerance=0.1)

#Hill's numbers
hil <- Hill(copNDVIlr,window=9,np=1,na.tolerance=0.1,alpha=seq(0,2,0.5))

#Renyi
ren <- Renyi(copNDVIlr,window=9,np=1,na.tolerance=0.1,alpha=seq(0,2,0.5))

## -----------------------------------------------------------------------------
shara <- raster(sha,template=copNDVIlr)
piera <- raster(pie,template=copNDVIlr)
berra <- raster(ber,template=copNDVIlr)
raora <- raster(rao,template=copNDVIlr)
crera <- raster(cre,template=copNDVIlr)
hilra <- lapply(hil, function(x) raster(x,template=copNDVIlr))
renra <- lapply(ren, function(x) raster(x,template=copNDVIlr))

## ----fig02--------------------------------------------------------------------
#Shannon's Diversity
levelplot(shara,main="Shannon's entropy from Copernicus NDVI 5 km (9 px-side moving window)",as.table = T,layout=c(0,1,1), ylim=c(-60,75), margin = list(draw = TRUE))

## ----fig03--------------------------------------------------------------------
#Pielou's Evenness
levelplot(piera,main="Pielou's evenness from Copernicus NDVI 5 km (9 px-side moving window)",as.table = T,layout=c(0,1,1), ylim=c(-60,75), margin = list(draw = TRUE))

## ----fig04--------------------------------------------------------------------
#Berger-Parker' Index
levelplot(berra,main="Berger-Parker's index from Copernicus NDVI 5 km (9 px-side moving window)",as.table = T,layout=c(0,1,1), ylim=c(-60,75), margin = list(draw = TRUE))

## ----fig05--------------------------------------------------------------------
#Rao's quadratic Entropy
levelplot(crera,main="Rao's quadratic entropy from Copernicus NDVI 5 km (9 px-side moving window)",as.table = T,layout=c(0,1,1), ylim=c(-60,75), margin = list(draw = TRUE))

## ----fig06--------------------------------------------------------------------
#Cumulative Residual Entropy
levelplot(crera,main="Cumulative Resiudal Entropy from Copernicus NDVI 5 km (9 px-side moving window)",as.table = T,layout=c(0,1,1), ylim=c(-60,75), margin = list(draw = TRUE))

## ----fig07--------------------------------------------------------------------
#Hill's numbers (alpha=0, 1, 1.5 and 2)
levelplot(stack(hilra),main="Hill's numbers from Copernicus NDVI 5 km (9 px-side moving window)",as.table = T,layout=c(0,5,1),names.attr=paste("alpha",seq(0,2,0.5),sep=" "), ylim=c(-60,75))

## ----fig08--------------------------------------------------------------------
#Renyi' Index (alpha=0, 1, 1.5 and 2)
levelplot(stack(renra),main="Renyi's entropy from Copernicus NDVI 5 km (9 px-side moving window)",as.table = T,layout=c(0,5,1),names.attr=paste("alpha",seq(0,2,0.5),sep=" "), ylim=c(-60,75), margin = list(draw = FALSE))

