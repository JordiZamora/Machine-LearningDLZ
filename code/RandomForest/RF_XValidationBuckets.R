XVal_RF <- function(x, y, Buckets=seq(1:10), nBuckets=10, mtry=NULL, ntree=500){
  #####--- Monte Carlo Cross Validation for Random Forest classifier -----####
  #Parameters
  #x: Matrix or data frame containing the predictive variables
  #y: Vector of predicted variables as factors
  #Buckets: List of Buckets for cross validation
  #nBuckets: Number of Buckets
  #mtry: Number of factors used on each Random Forest iteration. If mtry is not specified it will use sqrt(ncol(x)).
  #ntree: Number of factors used on each Random Forest iteration. If mtry is not specified it will use sqrt(ncol(x)).
  #Values
  #The function returns the mean misclassification error
  
  if (!require("randomForest")) install.packages("randomForest")

  if(is.null(mtry)) mtry <- ceiling(sqrt(ncol(x)))
  N <- length(y)
  pick <- round(N/nBuckets) #N data left out for Xvalid
  sumOfMisclass <- 0

  # Bucket iterations
  for (i in Buckets){
    cat("Iteration:", i,"\n")
    #Define sample to use as test
    subseted <- seq((1+(i-1)*pick),(i*pick))
    #Subset the data
    InSampleX <- x[-subseted,]
    InSampleY <- factor(y[-subseted])
    OutSampleX <- x[subseted,]
    OutSampleY <- factor(y[subseted])
    
    #Run random forest
    RFout <- randomForest(InSampleX,
                          InSampleY,
                          OutSampleX,
                          OutSampleY, 
                          mtry=mtry,
                          ntree=ntree)
    #Extract total number of misclassified data
    ncolResult <- ncol(RFout$test$confusion)
    MisclassError <- sum(rowSums(RFout$test$confusion[,-ncolResult])*
                           RFout$test$confusion[,ncolResult])
    
    sumOfMisclass <- sumOfMisclass + MisclassError
  }
  MisclassErrorMean <- sumOfMisclass/(pick*nBuckets)
  return(MisclassErrorMean)
}