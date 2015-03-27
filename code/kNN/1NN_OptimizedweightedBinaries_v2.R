if (!require("foreach")) install.packages("foreach")
if (!require("doSNOW")) install.packages("doSNOW")
library(class)#install.packages("class")
#Load data
rawdata <- read.csv("../data/Kaggle_Covertype_training.csv")
numcol <- ncol(rawdata)
numrow <- nrow(rawdata)
rawdata[,numcol] <- factor(rawdata[,numcol]) #Classes as factors
#rawdata <- rawdata[1:5000,]

Misclass1NN <- function(weights,rawdata){
  aux <- rawdata[,-c(1,30)]
  columnsAux <- ncol(aux)
  for (i in 11:14){
    aux[,i] <- aux[,i]*weights[1]
  }
  
  for (i in 15:(columnsAux-1)){
    aux[,i] <- aux[,i]*weights[2]
  }
  
  training <- aux
  noObs<-nrow(training)
  cores <- 8
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
      ks <- 1
      results<-data.frame(1,1,1)
      testError<-data.frame(1,1)
      cv<-Buckets[i]
      k<-1
      
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
    MisclassErrBucket
  }
  stopCluster(cl)
  
  MisclassErr <- mean(MisclassErrPar)
  return(MisclassErr)
}

#weights <- rep(100, 43)
#MisclassErr <- Misclass1NN(weights,rawdata)
#MisclassErr=0.08116

weights <- rep(100, 2)
out<-optim(weights, Misclass1NN, rawdata=rawdata, control= list(maxit = 50,reltol=0.001))
out
# $par
# [1] 105.3125  98.1250
# 
# $value
# [1] 0.08026
# 
# $counts
# function gradient 
# 51       NA 
# 
# $convergence
# [1] 1
# 
# $message
# NULL

# weights <- 97.3#rep(100, 43)
# MisclassErr <- Misclass1NN(weights,rawdata)

weights <- rep(100, 2)
out<-optim(weights, Misclass1NN, rawdata=rawdata, control= list(maxit = 50,reltol=0.001))
out
# $par
# [1] 105.3125  98.1250
# 
# $value
# [1] 0.08026
# 
# $counts
# function gradient 
# 51       NA 
# 
# $convergence
# [1] 1
# 
# $message
# NULL
# weights <- 97.3#rep(100, 43)
# MisclassErr <- Misclass1NN(weights,rawdata)


weights <- rep(100, 3)
out<-optim(weights, Misclass1NN, rawdata=rawdata, control= list(maxit = 20,reltol=0.001))
out

# $par
# [1] 110 100 100
# 
# $value
# [1] 0.08036
# 
# $counts
# function gradient 
# 21       NA 
# 
# $convergence
# [1] 1
# 
# $message
# NULL

weights <- rep(100, 43)
out<-optim(weights, Misclass1NN, rawdata=rawdata, method = "SANN",control= list(maxit = 20,reltol=0.001))
out
# $par
# [1]  99.34888  97.58086 100.95126 101.74253  97.17588 100.73555  96.22255  99.13942 103.25045 101.08850 100.92852
# [12]  98.28490  98.11669  95.52136 101.56706 102.23425 100.22004  98.57982 100.14905 100.15669  99.75461 102.28271
# [23] 105.05455  98.31146 105.19361 103.46330  95.40264 102.32247 100.15073 103.32841 103.78541  98.93182 103.65389
# [34] 100.50647 101.74244  96.49546 103.00788  99.32445 102.19013  99.93963  94.96720 104.62333  95.74271
# 
# $value
# [1] 0.08022
# 
# $counts
# function gradient 
# 20       NA 
# 
# $convergence
# [1] 0
# 
# $message
# NULL


weights <- rep(85, 43)
out<-optim(weights, Misclass1NN, rawdata=rawdata, method = "SANN",control= list(maxit = 20,reltol=0.001))
out
# $par
# [1] 86.38195 84.89251 89.07514 81.45907 85.88083 83.99318 86.83670 86.90292 87.62363 85.43999 84.91735 83.98239
# [13] 83.34699 87.47047 84.35511 83.08721 87.77955 83.86654 86.48770 86.32820 84.83666 78.24234 84.09656 83.13557
# [25] 83.87690 84.59446 87.72058 81.51324 84.60411 82.92687 82.68295 84.83857 83.84387 85.44786 86.22714 86.92949
# [37] 86.29091 86.56501 86.52828 85.18068 86.31204 88.30410 84.19522
# 
# $value
# [1] 0.08062
# 
# $counts
# function gradient 
# 20       NA 
# 
# $convergence
# [1] 0
# 
# $message
# NULL

weights <- rep(125, 43)
out<-optim(weights, Misclass1NN, rawdata=rawdata, method = "SANN",control= list(maxit = 10,reltol=0.001))
out
# $par
# [1] 123.1663 123.7508 126.7354 121.7557 121.8186 127.0847 121.8593 126.7210 122.7458 128.4421 124.1288 123.6769
# [13] 124.7041 122.0016 125.4130 126.2464 127.4251 124.8171 123.5850 127.0065 123.0559 126.2534 124.8677 124.5061
# [25] 122.4539 128.8767 123.6422 125.7728 127.7182 121.2566 124.4288 122.3535 120.3944 122.1244 125.2252 124.0439
# [37] 125.3817 126.2584 125.9374 124.7612 126.9171 129.2229 121.6363
# 
# $value
# [1] 0.08048
# 
# $counts
# function gradient Type: DONE 
# 
# 10       NA 
# 
# $convergence
# [1] 0
# 
# $message
# NULL
