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



## relationship between the Landsat bands and the VCF tree cover--------------
#plot pairs to observe correlations and to select bands for further analysis
pairs(brick(rasterbrick$band1,rasterbrick$band2,rasterbrick$band3,rasterbrick$VCF))
pairs(brick(rasterbrick$band4,rasterbrick$band5,rasterbrick$band7,rasterbrick$VCF))
pairs(brick(rasterbrick$band1,rasterbrick$band7,rasterbrick$ndvi,rasterbrick$VCF))

# Run linear regression model with selected bands
modelAll <- lm(VCF ~ band1+band2+band3+band4+band5+band7+ndvi, rasterbrickData, na.action=na.omit)
summary(modelAll)

# predictors (bands) probably most important 
model357ndvi <- lm(VCF ~ band3+band5+band7, rasterbrickData, na.action=na.omit)
summary(mode357ndvi)
# predictionraster & cleaning
predictVCF <- predict(rasterbrick, model= model357ndvi, na.rm = T)
predictVCF[ predictVCF < 0 ] <- NA


## plot the predicted tree cover raster ------------------------------------
# Comparing predicted and original VCF
# colorPal <- rev(colorRampPalette(c("darkgreen","yellow","brown"))(20)) # Create color palette
# plot1<-spplot(rasterbrick$VCF, main="Original VCF", col.regions = colorPal, 
#               sp.layout=list(list("SpatialPolygonsRescale", layout.north.arrow(), #Create north arrow
#                                   offset=c(850000, 845000), scale=7000, fill=c('white','black')), 
#                              list("SpatialPolygonsRescale", layout.scale.bar(), #Create scale bar
#                                   offset=c(842000, 820000), scale=10000, fill=c('white','black')),
#                              list("sp.text", c(842000, 821500), "0", font=2), #Add text to scale bar
#                              list("sp.text", c(852000, 821500), "10 km", font=2)))
# plot2<-spplot(VCFpredict,main="Predicted VCF", col.regions = colorPal, 
#               sp.layout=list(list("SpatialPolygonsRescale", layout.north.arrow(), 
#                                   offset=c(850000, 845000), scale=7000, fill=c('white','black')), 
#                              list("SpatialPolygonsRescale", layout.scale.bar(),
#                                   offset=c(842000, 820000), scale=10000, fill=c('white','black')),
#                              list("sp.text", c(842000, 821500), "0", font=2),
#                              list("sp.text", c(852000, 821500), "10 km", font=2)))



# compute RMSE -------------------------------------------------

source('R/calculateRMSE.R')
RMSEprediction <- calculateRMSE(rasterbrick$VCF, predictVCF)

## are the differences between the predicted and actual tree cover the same for all of ----------------
## the 3 classes we used for the random forest classfication?
covBrick <- brick(rasterbrick$VCF, predictVCF)
names(covBrick) <- c("VCF", "predictVCF")


# Using the training polygons from the random forest classification, --------------------------
# load the training polygons 
load("AdvancedRasterAnalysis-gh-pages/data/trainingPoly.rda")

#'relabel' our training classes
trainingPoly@data$Code <- as.numeric(trainingPoly@data$Class)

# assign 'Code' values to raster cells (where they overlap)
classes <- rasterize(trainingPoly, ndvi, field='Code')

## Mask band images by polygons
covmaskedVCF <- mask(covBrick, classes)

# combine this new brick with the classes layer for selection on class later
names(classes) <- "class"


## calculate zonal RMSE

differenceOfSquares <- function() {
  (covmaskedVCF$VCF - covmaskedVCF$predictVCF)^2 -> squaredDifferences
  averageSquaredDifferences <- zonal(squaredDifferences, classes, fun=mean, na.rm=T)
  RMSE <- sqrt(averageSquaredDifferences[, 2])
  return(RMSE)
}

differenceOfSquares()

