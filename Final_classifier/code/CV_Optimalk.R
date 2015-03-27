optimalK<-function(training_data, buckets, ks) {
  #Misclassification error for k nearest neighbours with cross-validation for different k's
  #Parameters:
  #buckets: An integer with the number of buckets for cross-validation
  #training_data: The training data set with a first column of indexs and a last column with the labels
  #ks: Integer or vector of integers with the number of nearest neighbours to be evaluated
  #Values
  #The function returns a numeric or vector of numeric values for the misclassification error
  
  #assigning obs to buckets for cross validation
  noBuckets <- buckets
  noObs<-nrow(training_data)
  idx <- rep(1:noBuckets, each=ceiling(nrow(rawdata)/noBuckets)) 
  idx <- idx[sample(1:noObs)]
  
  training <- training_data %>% mutate(bucket=idx) 
  
  training<- training %>% select( -soil_type_15)
  
  ks <- ks
  
  results<-data.frame(k=1,bucket=1,error=1)
  
  for (cv in 1:buckets) {
    
    #creating training data dependents and independents
    Xtrain <- training  %>% filter(bucket != cv) %>% select(-Cover_Type, -id, -bucket)
    Ytrain <- training %>% filter(bucket != cv) %>% select(Cover_Type)
    
    # subsetting the test phase data
    Xtest <- training %>% filter(bucket == cv) %>% select( -Cover_Type, -id, -bucket)
    Ytest <- training %>% filter(bucket == cv) %>% select( Cover_Type)
    
    #Run kNN for each K and compute error
    for (k in 1:length(ks)) {
      # kNN results
      info<-paste(as.character(cv),as.character(k))
      testPredictions <- knn(train=Xtrain, test=Xtest, cl=Ytrain$Cover_Type, k=ks[k])
      totalError <-  mean(testPredictions != Ytest$Cover_Type)
      #track errors
      results<-rbind(results,c(k=ks[k],bucket=cv,error=totalError))
    }
    
  }
  return(results)
}