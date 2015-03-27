library(class)#install.packages("class")
#Load data
rawdata <- read.csv("../data/Kaggle_Covertype_training.csv")
numcol <- ncol(rawdata)
numrow <- nrow(rawdata)
rawdata[,numcol] <- factor(rawdata[,numcol]) #Classes as factors



MisclassErr <- c()
weightsv <- c()
for (i in 1:5){
  aux <- rawdata[,-c(1,30)]
  columnsAux <- ncol(aux)
  weightsv[i] <-(50*(i-1)+1)
  weights <- seq(1,43)
  aux[,11:(columnsAux-1)] <- aux[,11:(columnsAux-1)]*weightsv[i]
  head(aux)
  
  training <- aux
  noObs<-nrow(training)
  noBuckets <- 10
  idx <- rep(1:noBuckets, each=ceiling(nrow(training)/noBuckets))
  idx <- idx[sample(1:noObs)]
  training <- data.frame(training,bucket=idx)
  head(training)
  ks <- 1
  results<-data.frame(1,1,1)
  testError<-data.frame(1,1)
  cv<-3
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
  MisclassErr[i] <- mean (testPredictions != Ytest)
}
#1knn
#without scaling
#Misclass=10.24
#1knn
#with all scaling
#Misclass=14.78
#1knn
#with continuous scaled
#Misclass=14.6
#1knn
#with weighted binaries
#Misclass= 0.0794
#1knn- Bucket 1
#with weighted binaries
#weights=1  51 101 151 201
#Misclass= 0.1032 0.0786 0.0786 0.0796 0.0856
#1knn- Bucket 2
#with weighted binaries
#weights=1  51 101 151 201
#Misclass= 0.0996 0.0860 0.0764 0.0848 0.0896
#1knn- Bucket 3
#with weighted binaries
#weights=1  51 101 151 201
#Misclass= 0.1030 0.0900 0.0838 0.0764 0.0812


plot(weightsv, MisclassErr)
