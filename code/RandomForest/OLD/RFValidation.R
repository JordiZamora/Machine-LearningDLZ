if (!require("foreach")) install.packages("foreach")
if (!require("doSNOW")) install.packages("doSNOW")
if (!require("randomForest")) install.packages("randomForest")
#source("RF_XValidation.R")
source("RF_XValidationBuckets.R")

setwd("~/Documents/Cursos/DataScience/2ndTerm/HomeworkMachineLearning/Project/code")

#Load data
rawdata <- read.csv("../data/Kaggle_Covertype_training.csv")
rawdata[,numcol] <- factor(rawdata[,numcol]) #Classes as factors
numcol <- ncol(rawdata)
numrow <- nrow(rawdata)

# Set Parameters
#MCiter <- 4
nBuckets <- 10
fraction <- 0.1
mtry <- 20
ntree <- 10
cores <- 4

# Dimension Reduction
dataset <- rawdata
#TrainSimplif <- dataset[,-c(22,30,52)]

# Separate features and labels
features <- dataset[,-c(1,numcol)]
labels <- dataset[,numcol]
ntrain <- length(labels)

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
# results<- foreach(MCiter = MCiterList, .combine = rbind) %dopar% {
#   MisclassError <- XVal_RF(features, labels, 
#                            MCiter=MCiter, fraction=fraction, 
#                            mtry=mtry, ntree=ntree)
#   
# }
stopCluster(cl)

MError <- colMeans(results)

#####!!!!!!!!!!!!!!!!!!!!!! Check tune function for cross validation 