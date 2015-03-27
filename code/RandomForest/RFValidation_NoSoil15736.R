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
featuresRaw <- featuresRaw[,-which(names(featuresRaw)=="soil_type_7")]
featuresRaw <- featuresRaw[,-which(names(featuresRaw)=="soil_type_36")]
labels <- factor(rawdata[,numcol])

features <- featuresRaw
t1 <- tuneRF(features,labels,mtryStart=20, ntreeTry=50, stepFactor=1.2, improve=0.01)
# mtry = 20  OOB error = 11.35% 
# Searching left ...
# mtry = 17   OOB error = 11.58% 
# -0.02062401 0.01 
# Searching right ...
# mtry = 24 	OOB error = 11.27% 
# 0.006345849 0.01 
t2 <- tuneRF(features,labels,mtryStart=39, ntreeTry=100, stepFactor=1.2, improve=0.01)
# mtry = 39  OOB error = 10.45% 
# Searching left ...
# mtry = 33   OOB error = 10.36% 
# 0.008993494 0.01 
# Searching right ...
# mtry = 46 	OOB error = 10.42% 
# 0.002870264 0.01 
t3 <- tuneRF(features,labels,mtryStart=30, ntreeTry=100, stepFactor=1.2, improve=0.01)
# mtry = 30  OOB error = 10.46% 
# Searching left ...
# mtry = 25   OOB error = 10.57% 
# -0.01090283 0.01 
# Searching right ...
# mtry = 36 	OOB error = 10.42% 
# 0.003251721 0.01 

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