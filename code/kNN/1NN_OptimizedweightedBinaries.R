library(class)#install.packages("class")
#Load data
rawdata <- read.csv("../data/Kaggle_Covertype_training.csv")
numcol <- ncol(rawdata)
numrow <- nrow(rawdata)
rawdata[,numcol] <- factor(rawdata[,numcol]) #Classes as factors

Misclass1NN <- function(weights,rawdata){
  aux <- rawdata[,-c(1,30)]
  columnsAux <- ncol(aux)
  for (i in 11:(columnsAux-1)){
    aux[,i] <- aux[,i]*weights[i-wfrom+1]
  }
  
  training <- aux
  noObs<-nrow(training)
  noBuckets <- 10
  idx <- rep(1:noBuckets, each=ceiling(nrow(training)/noBuckets))
  idx <- idx[sample(1:noObs)]
  training <- data.frame(training,bucket=idx)
  
  MisclassErrBucket <- c()
  
  for (i in 1:noBuckets){    
    ks <- 1
    results<-data.frame(1,1,1)
    testError<-data.frame(1,1)
    cv<-i
    k<-1
    names(training)
    traincol <- ncol(training)
    Xtrain <- training[training$bucket != cv, -c(traincol-1, traincol)]  
    Ytrain <- training[training$bucket != cv, (traincol-1)]
    # subsetting the test phase data
    Xtest <- training[training$bucket == cv, -c(traincol-1, traincol)]
    Ytest <- training[training$bucket == cv, (traincol-1)]
    # kNN results
    info<-paste(as.character(cv),as.character(k))
    testPredictions <- knn(train=Xtrain, test=Xtest, cl=Ytrain, k=1)
    MisclassErrBucket[i] <- mean (testPredictions != Ytest)
  }
  MisclassErr <- mean(MisclassErrBucket)
  return(MisclassErr)
}

#weights <- rep(100, 43)
#MisclassErr <- Misclass1NN(weights,rawdata)
#MisclassErr=0.08116

out<-optim(weights, Misclass1NN, rawdata=rawdata)
