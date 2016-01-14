
## Team Puma
## Jason Davis & Lieven Slenders. 
## January 13-1-2015
# -------------------------------------------------------




calculateRMSE <- function (x,y){
  # x is Original VCF layer, y is the Predicted VCF layer
  error <- x-y
  square <- error^2
  mean <- cellStats(square, 'mean')
  RMSE <- sqrt(mean)
  return(RMSE)
}