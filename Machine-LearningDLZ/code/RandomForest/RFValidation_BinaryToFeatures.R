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



for (j in 1:11){

  cat(j,"\n")
# Set Parameters
#MCiter <- 4
nBuckets <- 10
fraction <- 0.1
mtry <- 10
ntree <- round(10+190*(j-1)/10)
cores <- 4

# Dimension Reduction
names <- names(rawdata)[12:55]
dataset <- rawdata
dataset2 <- dataset[,1:11]
dataset2 <- cbind(dataset2, wild=rep(NA,numrow), soil=rep(NA,numrow))

#dataset2[,12:13] <- factor(t(apply(dataset[,12:55], 1, function(x) names[x==1])))
names <- c(seq(1,4),seq(1,40))
dataset2[,12:13] <- t(apply(dataset[,12:55], 1, function(x) names[x==1]))
dataset2[,14] <- dataset[,numcol]

numcol2 <- ncol(dataset2)
numrow2 <- nrow(dataset2)

#TrainSimplif <- dataset[,-c(22,30,52)]

# Separate features and labels
features <- dataset2[,-c(1,numcol2)]
labels <- factor(dataset2[,numcol2])
ntrain <- length(labels)

t1 <- tuneRF(features,labels,mtryStart=10, ntreeTry=50, stepFactor=1.2, improve=0.01)
# mtry = 10  OOB error = 11.27% 
# Searching left ...
# mtry = 9   OOB error = 11.15% 
# 0.01047399 0.01 
# mtry = 8 	OOB error = 11.22% 
# -0.006458558 0.01 
# Searching right ...
# mtry = 12 	OOB error = 11.27% 
# -0.01058486 0.01 
t1 <- tuneRF(features,labels,mtryStart=9, ntreeTry=100, stepFactor=1.5, improve=0.01)
# mtry = 9  OOB error = 10.6% 
# Searching left ...
# mtry = 6   OOB error = 10.61% 
# -0.001131862 0.01 
# Searching right ...
# mtry = 12 	OOB error = 10.88% 
# -0.02641011 0.01

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

MError[j] <- colSums(results)
#MError=[1] 0.12402 0.11114 0.10964 0.10706 0.10628 0.10550 0.10680 0.10604 0.10536 0.10562 0.10710
}
#####!!!!!!!!!!!!!!!!!!!!!! Check tuneRF function for initial search on mtry 