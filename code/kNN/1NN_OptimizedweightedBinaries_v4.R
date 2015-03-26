if (!require("foreach")) install.packages("foreach")
if (!require("doSNOW")) install.packages("doSNOW")
library(class)#install.packages("class")
#Load data
rawdata <- read.csv("../data/Kaggle_Covertype_training.csv")
rawTestdata <- read.csv("../data/Kaggle_Covertype_test.csv")
numcol <- ncol(rawdata)
numrow <- nrow(rawdata)
rawdata[,numcol] <- factor(rawdata[,numcol]) #Classes as factors

set.seed(10)

Misclass1NN <- function(weights,rawdata, cores){
  aux <- rawdata[,-c(1,30)]
  columnsAux <- ncol(aux)
  aux[,11:(columnsAux-1)] <- aux[,11:(columnsAux-1)]*weights
  
  training <- aux
  noObs<-nrow(training)
  cores <- cores
  noBuckets <- 8
  idx <- rep(1:noBuckets, each=ceiling(nrow(training)/noBuckets))
  idx <- idx[sample(1:noObs)]
  training <- data.frame(training,bucket=idx)
  
  BucketsList <- split(seq(1,noBuckets), seq(1,cores))
  
  cl <- makeCluster(cores, type="SOCK", outfile="") 
  registerDoSNOW(cl)  
  
  MisclassErrPar<- foreach(Buckets = BucketsList, .combine = rbind) %dopar% {
    library(class)#install.packages("class")
    MisclassErrBucket <- c()
    for (i in 1:length(Buckets)){    
      
      cv<-Buckets[i]
      k<-1
      
      traincol <- ncol(training)
      Xtrain <- training[training$bucket != cv, -c(traincol-1, traincol)]  
      Ytrain <- training[training$bucket != cv, (traincol-1)]
      # subsetting the test phase data
      Xtest <- training[training$bucket == cv, -c(traincol-1, traincol)]
      Ytest <- training[training$bucket == cv, (traincol-1)]
      
      # kNN results
      testPredictions <- knn(train=Xtrain, test=Xtest, cl=Ytrain, k=k)
      MisclassErrBucket[i] <- mean (testPredictions != Ytest)
    }
    MisclassErrBucket
  }
  stopCluster(cl)
  
  MisclassErr <- mean(MisclassErrPar)
  return(MisclassErr)
}

MisclassErrOut <- c()
for (i in 1:26){
  weights <- 1+250*(i-1)/25
  MisclassErr <- Misclass1NN(weights,rawdata,8)
  cat("i=", i, " ",c(MisclassErr,weights), "\n")
  MisclassErrOut <- rbind(MisclassErrOut, c(MisclassErr,weights))
}
# [,1] [,2]
# [1,] 0.10312    1
# [2,] 0.10150   11
# [3,] 0.09860   21
# [4,] 0.09390   31
# [5,] 0.09030   41
# [6,] 0.08684   51
# [7,] 0.08540   61
# [8,] 0.08322   71
# [9,] 0.08184   81
# [10,] 0.08254   91
# [11,] 0.08078  101
# [12,] 0.08134  111
# [13,] 0.08244  121
# [14,] 0.08370  131
# [15,] 0.08300  141
# [16,] 0.08324  151
# [17,] 0.08284  161
# [18,] 0.08476  171
# [19,] 0.08270  181
# [20,] 0.08336  191
# [21,] 0.08288  201
# [22,] 0.08308  211
# [23,] 0.08422  221
# [24,] 0.08416  231
# [25,] 0.08234  241
# [26,] 0.08278  251

# [,1] [,2]
# [1,] 0.10290    1
# [2,] 0.10100   11
# [3,] 0.09896   21
# [4,] 0.09290   31
# [5,] 0.09056   41
# [6,] 0.08710   51
# [7,] 0.08488   61
# [8,] 0.08280   71
# [9,] 0.08190   81
# [10,] 0.08120   91
# [11,] 0.08150  101
# [12,] 0.08230  111
# [13,] 0.08264  121
# [14,] 0.08174  131
# [15,] 0.08312  141
# [16,] 0.08240  151
# [17,] 0.08422  161
# [18,] 0.08368  171
# [19,] 0.08342  181
# [20,] 0.08240  191
# [21,] 0.08254  201
# [22,] 0.08358  211
# [23,] 0.08310  221
# [24,] 0.08450  231
# [25,] 0.08294  241
# [26,] 0.08378  251
write.csv(MisclassErrOut, file = "1NN_OptimalWeight.csv", row.names=FALSE)

weights <- MisclassErrOut[which.min(MisclassErrOut[,1]),2]

train <- rawdata[,-c(1,30)]
columnsAux <- ncol(train)
train[,11:(columnsAux-1)] <- train[,11:(columnsAux-1)]*weights

Xtrain <- train[,-columnsAux]
Ytrain <- factor(train[,columnsAux])
Xtest <- rawTestdata[,-c(1,30)]
Xtest[,11:(columnsAux-1)] <- Xtest[,11:(columnsAux-1)]*weights

testPredictions <- knn(train=Xtrain, test=Xtest, cl=Ytrain, k=1)

prediction<-data.frame(rawTestdata[,1],testPredictions)
names(prediction) <- c("id", "Cover_Type")
write.csv(prediction, file = "1NN_PredictionWeight.csv", row.names=FALSE)
