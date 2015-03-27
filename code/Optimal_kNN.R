#####Creates function to select optimal k for KNN

if (!require("foreach")) install.packages("foreach")
if (!require("doSNOW")) install.packages("doSNOW")
if (!require("class")) install.packages("class")
if (!require("plyr")) install.packages("plyr")
if (!require("dplyr")) install.packages("dplyr")
#Load data
rawdata <- read.csv("../data/Kaggle_Covertype_training.csv")
rawTestdata <- read.csv("../data/Kaggle_Covertype_test.csv")
numcol <- ncol(rawdata)
numrow <- nrow(rawdata)
rawdata[,numcol] <- factor(rawdata[,numcol]) #Classes as factors

#for replication
set.seed(10)

#select optimal k  for NN
#No rescaling of continuous features


optimalK<-function(training_data, buckets, ks) {
    
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
            
                #creating training data dependents
                Xtrain <- training  %>% filter(bucket != cv) %>% select(-Cover_Type, -id, -bucket)
                
                #creating training data independent
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

#k's to choose from
ks<-c(1,3, 5, 7,9)

#run function with cross validation to choose k
results<-optimalK(rawdata, 10 , ks)

#select optimal k with lowest mean error across buckets and k
optK<-aggregate(results$error[-1], list(results$k[-1]),mean)

k<-optK[which.min(optK[,2]),1] 


