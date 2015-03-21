if (!require("foreach")) install.packages("foreach")
if (!require("doSNOW")) install.packages("doSNOW")
if (!require("randomForest")) install.packages("randomForest")
setwd("~/Documents/Cursos/DataScience/2ndTerm/HomeworkMachineLearning/Project/code")

#source("RF_XValidation.R")
source("RF_XValidationBuckets.R")


#Load data
rawdata <- read.csv("../data/Kaggle_Covertype_training.csv")
rawdata[,numcol] <- factor(rawdata[,numcol]) #Classes as factors
numcol <- ncol(rawdata)
numrow <- nrow(rawdata)


# Set Parameters
#MCiter <- 4
nBuckets <- 10
fraction <- 0.1
mtry <- 10
ntree <- 100
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
labels <- dataset2[,numcol2]
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

MError <- colSums(results)
#MError= 0.10724

#####!!!!!!!!!!!!!!!!!!!!!! Check tuneRF function for initial search on mtry 