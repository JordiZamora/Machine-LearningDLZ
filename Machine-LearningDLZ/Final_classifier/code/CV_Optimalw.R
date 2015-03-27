Misclass1NN <- function(weights,rawdata, cores, k=1){
  #Misclassification error for k nearest neighbours with cross-validation for different weights
  #Parameters:
  #weights: A numeric value used to rescale the binary features of the data (from column 11 to ncol-1)
  #rawdata: The training data set with a first column of indexs and a last column with the labels
  #cores: Integer for the number of cores used for parallel computing
  #k: Integer for the number of nearest neighbours
  #Values
  #The function returns a numeric value for the misclassification error
  
  aux <- rawdata[,-c(1,30)] #remove unnecessary columns
  columnsAux <- ncol(aux)
  
  #Weighting of the binary variables
  aux[,11:(columnsAux-1)] <- aux[,11:(columnsAux-1)]*weights 
  training <- aux #cleaned data set
  
  #Prepare data for parallel cross-validation
  noBuckets <- 8 #number of buckets
  noObs<-nrow(training)
  idx <- rep(1:noBuckets, each=ceiling(nrow(training)/noBuckets))
  idx <- idx[sample(1:noObs)] #Generate a random sample of indices
  training <- data.frame(training,bucket=idx) #add column with bucket index
  
  BucketsList <- split(seq(1,noBuckets), seq(1,cores)) #Split buckets for parallelization
  
  #Parallel cross-validation
  cl <- makeCluster(cores, type="SOCK", outfile="") 
  registerDoSNOW(cl)  
  
  MisclassErrPar<- foreach(Buckets = BucketsList, .combine = rbind) %dopar% {
    library(class)#install.packages("class")
    MisclassErrBucket <- c() #Output variable per bucket
    for (i in 1:length(Buckets)){    
      cv<-Buckets[i] #Current out-of-sample bucket
      #In-sample-data
      traincol <- ncol(training)
      Xtrain <- training[training$bucket != cv, -c(traincol-1, traincol)]  
      Ytrain <- training[training$bucket != cv, (traincol-1)]
      #Out-of-sample data
      Xtest <- training[training$bucket == cv, -c(traincol-1, traincol)]
      Ytest <- training[training$bucket == cv, (traincol-1)]
      
      # kNN classification
      testPredictions <- knn(train=Xtrain, test=Xtest, cl=Ytrain, k=k)
      MisclassErrBucket[i] <- mean (testPredictions != Ytest) #Misclassification Error for the bucket
    }
    MisclassErrBucket
  }
  stopCluster(cl)
  
  #Cross-validated misclassification error
  MisclassErr <- mean(MisclassErrPar)
  return(MisclassErr)
}