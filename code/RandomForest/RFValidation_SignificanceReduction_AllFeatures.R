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
rawdata[,numcol] <- factor(rawdata[,numcol]) #Classes as factors

features <- rawdata[,-c(1,numcol)]
labels <- rawdata[,numcol]

mtry <- 20
ntree <- 100
RFout <- randomForest(features,
                      labels,
                      mtry=mtry,
                      ntree=ntree)

RFoutImportanceSorted <- RFout$importance[order(RFout$importance[,1]),]
#FeaturesOut <- names(RFoutImportanceSorted[RFoutImportanceSorted<100])
FeaturesOut <- which(RFout$importance[,1]<100)
# Dimension Reduction
dataset <- rawdata[,-(FeaturesOut+1)]
dataset2 <- dataset
numcol2 <- ncol(dataset2)
numrow2 <- nrow(dataset2)

# Separate features and labels
features <- dataset2[,-c(1,numcol2)]
labels <- dataset2[,numcol2]

#Coarse grain Tuning of mtry
t1 <- tuneRF(features,labels,mtryStart=6, ntreeTry=10, stepFactor=1.2, improve=0.01)
# mtry = 6  OOB error = 18.86% 
# Searching left ...
# mtry = 5   OOB error = 20.09% 
# -0.06494342 0.01 
# Searching right ...
# mtry = 7 	OOB error = 18.27% 
# 0.03135015 0.01 
# mtry = 8 	OOB error = 17.64% 
# 0.03467712 0.01 
# mtry = 9 	OOB error = 16.94% 
# 0.03961672 0.01 
# mtry = 10 	OOB error = 17.32% 
# -0.02239436 0.01 
t2 <- tuneRF(features,labels,mtryStart=6, ntreeTry=50, stepFactor=1.2, improve=0.01)
# mtry = 6  OOB error = 13.59% 
# Searching left ...
# mtry = 5   OOB error = 15.36% 
# -0.1304284 0.01 
# Searching right ...
# mtry = 7 	OOB error = 12.71% 
# 0.06477256 0.01 
# mtry = 8 	OOB error = 12.15% 
# 0.04375885 0.01 
# mtry = 9 	OOB error = 11.8% 
# 0.02897119 0.01 
# mtry = 10 	OOB error = 11.75% 
# 0.003898966 0.01
t3 <- tuneRF(features,labels,mtryStart=18, ntreeTry=50, stepFactor=1.2, improve=0.01)
# mtry = 18  OOB error = 11.19% 
# Searching left ...
# mtry = 15   OOB error = 11.46% 
# -0.02394996 0.01 
# Searching right ...
# mtry = 21 	OOB error = 11.33% 
# -0.01286863 0.01 
t4 <- tuneRF(features,labels,mtryStart=16, ntreeTry=100, stepFactor=1.1, improve=0.01)
# mtry = 16  OOB error = 10.8% 
# Searching left ...
# mtry = 15   OOB error = 10.89% 
# -0.008334877 0.01 
# Searching right ...
# mtry = 17 	OOB error = 10.74% 
# 0.005371365 0.01 

MError <- c()

for (j in 1:11){

  cat(j,"\n")
# Set Parameters
#MCiter <- 4
nBuckets <- 10
fraction <- 0.1
mtry <- 17
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
#MError=0.12898 0.11234 0.11098 0.10940 0.10900 0.10902 0.10838 0.10866 0.10820 0.10862 0.10784
#For j From 12 to 14
#MError=0.10844 0.10736 0.10766
}
#####!!!!!!!!!!!!!!!!!!!!!! Check tuneRF function for initial search on mtry 