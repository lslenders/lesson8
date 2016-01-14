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
mode357ndvi <- lm(VCF ~ band3+band5+band7, rasterbrickData, na.action=na.omit)
summary(mode357ndvi)

## plot the predicted tree cover raster ------------------------------------
# load the training polygons 
load("AdvancedRasterAnalysis-gh-pages/data/trainingPoly.rda")
# superimpose training polygons onto ndvi plot
plot(ndvi)
plot(trainingPoly, add = TRUE)

#'relabel' our training classes
trainingPoly@data$Code <- as.numeric(trainingPoly@data$Class)

# assign 'Code' values to raster cells (where they overlap)
classes <- rasterize(trainingPoly, ndvi, field='Code')
# colors to classes
cols <- c("orange", "dark green", "light blue")

# plot without a legend
plot(classes, col=cols, legend=FALSE)

# add a customized legend
legend("topright", legend=c("cropland", "forest", "wetland"), fill=cols, bg="white")

##
covBrick <- brick(rasterbrick$band3, rasterbrick$band5, rasterbrick$band7, rasterbrick$ndvi, rasterbrick$VCF)
names(covBrick) <- c("band3", "band5", "band7", "ndvi", "VCF")
## Mask band images by polygons
covmasked <- mask(covBrick, classes)

# combine this new brick with the classes layer to make our input training dataset
names(classes) <- "class"
covBrick <- addLayer(covmasked, classes)
plot(covBrick)

# extract all values into a matrix and remove NA values from valuetable
valuetable <- getValues(covBrick)
valuetable <- na.omit(valuetable)

##Convert values to data frame
df_valuetable <- as.data.frame(valuetable)
range(df_valuetable$class)


df_valuetable$class <- factor(df_valuetable$class, levels = c(1:3)) ## must WHY ??? 

val_crop <- subset(df_valuetable, class == 1)
val_forest <- subset(df_valuetable, class == 2)
val_wetland <- subset(df_valuetable, class == 3)



# compare with the original VCF raster 
..




