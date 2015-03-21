if (!require("foreach")) install.packages("foreach")
if (!require("doSNOW")) install.packages("doSNOW")
if (!require("randomForest")) install.packages("randomForest")
setwd("~/Documents/Cursos/DataScience/2ndTerm/HomeworkMachineLearning/Project/code/RandomForest")

#source("RF_XValidation.R")
source("RF_XValidationBuckets.R")


#Load data
rawdata <- read.csv("../../data/Kaggle_Covertype_training.csv")
numcol <- ncol(rawdata)
numrow <- nrow(rawdata)
#rawdata[,numcol] <- factor(rawdata[,numcol]) #Classes as factors

featuresRaw <- rawdata[,-c(1,numcol)]
featuresRaw <- featuresRaw[,-which(names(featuresRaw)=="soil_type_15")]
labels <- factor(rawdata[,numcol])

features <- featuresRaw
t1 <- tuneRF(features,labels,mtryStart=20, ntreeTry=50, stepFactor=1.2, improve=0.01)
# mtry = 20  OOB error = 11.49% 
# Searching left ...
# mtry = 17   OOB error = 11.66% 
# -0.01514624 0.01 
# Searching right ...
# mtry = 24 	OOB error = 11.22% 
# 0.02332869 0.01 
# mtry = 28 	OOB error = 11.1% 
# 0.01069519 0.01 
# mtry = 33 	OOB error = 10.97% 
# 0.01153153 0.01 
# mtry = 39 	OOB error = 10.92% 
# 0.004374772 0.01 
t2 <- tuneRF(features,labels,mtryStart=39, ntreeTry=100, stepFactor=1.2, improve=0.01)
# mtry = 39  OOB error = 10.42% 
# Searching left ...
# mtry = 33   OOB error = 10.44% 
# -0.001919017 0.01 
# Searching right ...
# mtry = 46 	OOB error = 10.51% 
# -0.008251775 0.01 

MError <- c()

for (j in 1:11){

  cat(j,"\n")
# Set Parameters
#MCiter <- 4
nBuckets <- 10
fraction <- 0.1
mtry <- 38
ntree <- round(10+190*(j-1)/10)
cores <- 4


# Parameters Optimization
#MCiterList <- split(rep(round(MCiter/cores), cores), seq(1,cores))
BucketsList <- split(seq(1,nBuckets), seq(1,cores))

cl <- makeCluster(cores, type="SOCK", outfile="") 
registerDoSNOW(cl)  

results<- foreach(Buckets = BucketsList, .combine = rbind) %dopar% {
  MisclassError <- XVal_RF(features, labels, 
                           Buckets=Buckets, nBuckets=nBuckets,
                           mtry=mtry, ntree=ntree)
  MisclassError
  
}

stopCluster(cl)

MError[j] <- colSums(results)
#For j From 1:11
#MError=
}
#####!!!!!!!!!!!!!!!!!!!!!! Check tuneRF function for initial search on mtry 