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

features <- rawdata[,-c(1,numcol)]
labels <- rawdata[,numcol]
labels[labels==5] <- 2
labels[labels==4] <- 3
labels <- factor(labels)

t1 <- tuneRF(features,labels,mtryStart=20, ntreeTry=50, stepFactor=1.2, improve=0.01)
# mtry = 20  OOB error = 10.47% 
# Searching left ...
# mtry = 17   OOB error = 10.58% 
# -0.01069723 0.01 
# Searching right ...
# mtry = 24 	OOB error = 10.34% 
# 0.01241643 0.01 
# mtry = 28 	OOB error = 10.24% 
# 0.00967118 0.01 
t2 <- tuneRF(features,labels,mtryStart=28, ntreeTry=100, stepFactor=1.2, improve=0.01)
# mtry = 28  OOB error = 9.65% 
# Searching left ...
# mtry = 24   OOB error = 9.74% 
# -0.009533679 0.01 
# Searching right ...
# mtry = 33 	OOB error = 9.6% 
# 0.005181347 0.01 
features <- features[,-which(names(features)=="soil_type_15")]
t3 <- tuneRF(features,labels,mtryStart=33, ntreeTry=100, stepFactor=1.2, improve=0.01)
# mtry = 33  OOB error = 9.64% 
# Searching left ...
# mtry = 28   OOB error = 9.65% 
# -0.001245072 0.01 
# Searching right ...
# mtry = 39 	OOB error = 9.43% 
# 0.02137373 0.01 
# mtry = 46 	OOB error = 9.69% 
# -0.02735369 0.01 
t4 <- tuneRF(features,labels,mtryStart=39, ntreeTry=100, stepFactor=1.05, improve=0.01)
# mtry = 39  OOB error = 9.61% 
# Searching left ...
# mtry = 38   OOB error = 9.47% 
# 0.01539422 0.01 
# mtry = 37 	OOB error = 9.55% 
# -0.009085147 0.01 
# Searching right ...
# mtry = 40 	OOB error = 9.74% 
# -0.0289457 0.01 

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