


calculateRMSEclass <- function(r1,r2,class){
  # r1 is the original VCF data in the training area, r2 is the Predicted VCF. 
  r1sub <- r1$VCF #select the VCF column
  r1sub[r1$Class != class] <- NA #all values in unwanted classes are set to NA
  r2sub <- r2$VCF
  r2sub[r2$Class != class] <- NA
  RMSEsub <- calculate.RMSE(r1sub,r2sub)
  return(RMSEsub)
}