## Team Puma
## Jason Davis & Lieven Slenders. 
## January 13-1-2015
# -------------------------------------------------------

## Lesson 7

## setting working directory
getwd()
# set working directory to you own specifications
setwd("D:/workspace/geoscripting/lesson6")
# 
dir.create('data/')
dir.create('R/')
dir.create('output/')
#---------------------------------------------------------------------------

#tutorial: 

install.packages('knitr', dependencies = TRUE)
# update all existing packages first
update.packages(ask = FALSE, repos = 'http://cran.rstudio.org')
install.packages('knitr', repos = c('http://yihui.name/xran', 'http://cran.rstudio.org'))


install.packages("rmarkdown")
