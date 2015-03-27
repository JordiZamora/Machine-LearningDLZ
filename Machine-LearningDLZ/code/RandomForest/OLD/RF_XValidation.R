XVal_RF <- function(x, y, MCiter=100, fraction=0.1, mtry=NULL, ntree=500){
  #####--- Monte Carlo Cross Validation for Random Forest classifier -----####
  #Parameters
  #x: Matrix or data frame containing the predictive variables
  #y: Vector of predicted variables as factors
  #MCiter: Number of Monte Carlo iterations
  #fraction: Fraction of data left out on each iteration
  #mtry: Number of factors used on each Random Forest iteration. If mtry is not specified it will use sqrt(ncol(x)).
  #ntree: Number of factors used on each Random Forest iteration. If mtry is not specified it will use sqrt(ncol(x)).
  #Values
  #The function returns the mean misclassification error
  
  if (!require("randomForest")) install.packages("randomForest")

  if(is.null(mtry)) mtry <- ceiling(sqrt(ncol(x)))
  N <- length(y)
  pick <- round(N*fraction) #N data left out for Xvalid
  sumOfMisclass <- 0

# Monte Carlo iterations
  for (i in 1:MCiter){
    cat("Iteration:", i,"\n")
    #Define random sample to use as test
    subseted <- sample(1:N,pick)
    #Subset the data
    InSampleX <- x[-subseted,]
    InSampleY <- y[-subseted]
    OutSampleX <- x[subseted,]
    OutSampleY <- y[subseted]
    
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
  MisclassErrorMean <- sumOfMisclass/(MCiter*pick)
  return(MisclassErrorMean)
}