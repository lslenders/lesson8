## Team Puma
## Jason Davis & Lieven Slenders. 
## January 13-1-2015
# -------------------------------------------------------

library(sp)
library(raster)
load("AdvancedRasterAnalysis-gh-pages/data/GewataB1.rda")
load("AdvancedRasterAnalysis-gh-pages/data/GewataB2.rda")
load("AdvancedRasterAnalysis-gh-pages/data/GewataB3.rda")
load("AdvancedRasterAnalysis-gh-pages/data/GewataB4.rda")
load("AdvancedRasterAnalysis-gh-pages/data/GewataB5.rda")
load("AdvancedRasterAnalysis-gh-pages/data/GewataB7.rda")
load("AdvancedRasterAnalysis-gh-pages/data/vcfGewata.rda")

## Remove erroneous values from the vcfGewata band
vcfGewata[vcfGewata > 100] <- NA

## Calculate NDVI -----------------------------------------------
ndvi <- overlay(GewataB4, GewataB3, fun=function(x,y){(x-y)/(x+y)})

## build a brick containing all data
rasterbrick <- brick(GewataB1, GewataB2, GewataB3, GewataB4, GewataB5, GewataB7)
rasterbrick <- calc(rasterbrick, fun=function(x) x / 10000)
rasterbrick <- addLayer(rasterbrick, c(ndvi, vcfGewata))
names(rasterbrick) <- c("band1","band2", "band3", "band4", "band5", "band7","ndvi", "VCF")
head(rasterbrick)

## extract all data to a data.frame
rasterbrickData <- getValues(rasterbrick)
rasterbrickData <- as.data.frame(rasterbrick, na.rm=T)

##plot
pairs(rasterbrick)


## Run linear regression model
modelAll <- lm(VCF ~ band1+band2+band3+band4+band5+band7, rasterbrickData, na.action=na.omit)
summary(modelAll)
plot(modelAll)
  
  
  
  
# load the training polygons -------------------------------
load("AdvancedRasterAnalysis-gh-pages/data/trainingPoly.rda")

# we can convert to integer by using the as.numeric() function, 
# which takes the factor levels
trainingPoly@data$Code <- as.numeric(trainingPoly@data$Class)
trainingPoly@data

