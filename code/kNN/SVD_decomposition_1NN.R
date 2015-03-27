
---
title: "Untitled"
author: "Samantha Dalton"
date: "Saturday, March 21, 2015"
output: html_document
---

```{r}

library("class")
install.packages("dplyr")
library("dplyr")

training<-read.csv("../data/Kaggle_Covertype_training.csv", header=T)
test<-read.csv("../data/Kaggle_Covertype_test.csv", header=T)


#checking out soil composition
table(test$soil_type_37)
table(test$soil_type_7)

#Load data
rawdata <- training
numcol <- ncol(rawdata)
numrow <- nrow(rawdata)
rawdata[,numcol] <- factor(rawdata[,numcol]) #Classes as factors

#remover soiltype15, covertype and id before SVD
aux <- rawdata[,-c(1,30,numcol)]

########## tried pca code first, but didn't work well #####################
#aux[,1:11] <- scale(aux[,1:11])
# aux <- scale(aux)
# range01 <- function(x){(x-min(x))/(max(x)-min(x))}
# aux[,1:11]<- apply(aux[,1:11],2,range01)

#out<-princomp(aux[,1:ncol(aux)])
# PCAFrame <- as.data.frame(cbind(out$scores, Cov_Type=rawdata[,numcol]))
# PCAFrame[,ncol(PCAFrame)] <- factor(PCAFrame[,ncol(PCAFrame)])

depthv <- c()
MisclassErr <-c()
out<-svd(aux[,1:ncol(aux)])

#creating a loop to determine how many SVD components we should use
for (i in 2:40){
      depth <- i
      depthv[i] <- depth
      #SVD
      us <- as.matrix(out$u[, 1:depth])
      vs <- as.matrix(out$v[, 1:depth])
      ds <- as.matrix(diag(out$d)[1:depth, 1:depth])
      Xreduced <- us %*% ds
      SVDFrame <- as.data.frame(cbind(Xreduced, Cov_Type=rawdata[,numcol]))
      SVDFrame[,ncol(SVDFrame)] <- factor(SVDFrame[,ncol(SVDFrame)])
      
      training <- SVDFrame
      
      #Creating buckest to train SVD performace using 1KNN
      noObs<-nrow(training)
      noBuckets <- 10
      idx <- rep(1:noBuckets, each=ceiling(nrow(training)/noBuckets)) 
      idx <- idx[sample(1:noObs)]
      
      
      training <- data.frame(training,bucket=idx) 
      
      results<-data.frame(1,1,1)
      testError<-data.frame(1,1)

#looping through 10 buckets for cross validation to determine optimal # of components to use for SVD
      for (cv in 1:10) {
            names(training)
            traincol <- ncol(training)
            Xtrain <- training[training$bucket != cv, -c(traincol-1, traincol)]
            
            Ytrain <- training[training$bucket != cv,  (traincol-1)]
            
            # subsetting the test phase data
            Xtest <- training[training$bucket == cv, -c(traincol-1, traincol)]
            
            Ytest <- training[training$bucket == cv,  (traincol-1)]
            
            table(rawdata$Cover_Type,rawdata$soil_type_37)
                  names(rawdata)
            
            # kNN results
            testPredictions1 <- knn(train=Xtrain, test=Xtest, cl=Ytrain, k=1)
            MisclassErr <- rbind(MisclassErr,c(depth, cv, mean(testPredictions != Ytest)))
      }
}

#reporting misclassification error by bucket and # of svd components
error<- data.frame(MisclassErr)
write.csv(x = MisclassErr, file = "data/knn_svd_errors.csv", row.names = F)

# reading in data to determine optimal # of components to use with lowest error
svd_errors<-read.csv("data/knn_svd_errors.csv", header=T)
results<-aggregate(svd_errors$V3, list(svd_errors$V1), mean)
depth<-which.min(results$x)