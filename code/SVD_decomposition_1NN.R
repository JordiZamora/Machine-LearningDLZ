
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

setwd("C:\\Users\\Samantha\\Documents\\GitHub\\Machine-LearningDLZ")
training<-read.csv("data/Kaggle_Covertype_training.csv", header=T)
test<-read.csv("data/Kaggle_Covertype_test.csv", header=T)



table(test$soil_type_37)
table(test$soil_type_7)

#Load data
rawdata <- training
numcol <- ncol(rawdata)
numrow <- nrow(rawdata)
rawdata[,numcol] <- factor(rawdata[,numcol]) #Classes as factors

#remover soiltype15, covertype and id before SVD
aux <- rawdata[,-c(1,30,numcol)]
#aux[,1:11] <- scale(aux[,1:11])
# aux <- scale(aux)
# range01 <- function(x){(x-min(x))/(max(x)-min(x))}
# aux[,1:11]<- apply(aux[,1:11],2,range01)

#out<-princomp(aux[,1:ncol(aux)])
# PCAFrame <- as.data.frame(cbind(out$scores, Cov_Type=rawdata[,numcol]))
# PCAFrame[,ncol(PCAFrame)] <- factor(PCAFrame[,ncol(PCAFrame)])

names(aux)
depthv <- c()
MisclassErr <-c()
out<-svd(aux[,1:ncol(aux)])


for (i in 2:40){
depth <- i
depthv[i] <- depth
us <- as.matrix(out$u[, 1:depth])
vs <- as.matrix(out$v[, 1:depth])
ds <- as.matrix(diag(out$d)[1:depth, 1:depth])
Xreduced <- us %*% ds
SVDFrame <- as.data.frame(cbind(Xreduced, Cov_Type=rawdata[,numcol]))
SVDFrame[,ncol(SVDFrame)] <- factor(SVDFrame[,ncol(SVDFrame)])

training <- SVDFrame

noObs<-nrow(training)
noBuckets <- 10
idx <- rep(1:noBuckets, each=ceiling(nrow(training)/noBuckets)) 
idx <- idx[sample(1:noObs)]


training <- data.frame(training,bucket=idx) 
head(training)


results<-data.frame(1,1,1)
testError<-data.frame(1,1)


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

error<- data.frame(MisclassErr)
names(error)
aggregate(error, by=list(error$X1), FUN=mean)

write.csv(x = MisclassErr, file = "data/knn_svd_errors.csv", row.names = F)

matrix(MisclassErr,39,10, byrow=TRUE)

class(MisclassErr)

plots<-qplot(x=depth, y=cv, data=error, colour=cv) + geom_line()

jpeg(filename = 'plots/kErrorFeature.jpg', units = "in", width = 5, height = 5, res = 400)
plot1
dev.off()

svd_errors<-read.csv("data/knn_svd_errors.csv", header=T)


names(svd_errors)
results<-aggregate(svd_errors$V3, list(svd_errors$V1), mean)

results<-aggregate(svd_errors$V3, list(svd_errors$V1), mean)
depth<-which.min(results$x)

```
Submission Code
```{r}


aux <- rawdata[,-c(1,30,numcol)]
#aux[,1:11] <- scale(aux[,1:11])
# aux <- scale(aux)
# range01 <- function(x){(x-min(x))/(max(x)-min(x))}
# aux[,1:11]<- apply(aux[,1:11],2,range01)

#out<-princomp(aux[,1:ncol(aux)])
# PCAFrame <- as.data.frame(cbind(out$scores, Cov_Type=rawdata[,numcol]))
# PCAFrame[,ncol(PCAFrame)] <- factor(PCAFrame[,ncol(PCAFrame)])


  us <- as.matrix(out$u[, 1:depth])
  vs <- as.matrix(out$v[, 1:depth])
  ds <- as.matrix(diag(out$d)[1:depth, 1:depth])
  Xreduced <- us %*% ds
  SVDFrame <- as.data.frame(cbind(Xreduced, Cov_Type=rawdata[,numcol]))
  SVDFrame[,ncol(SVDFrame)] <- factor(SVDFrame[,ncol(SVDFrame)])

Xtrain <- training[, -c(traincol-1, traincol)]

Ytrain <- training[,  (traincol-1)]


t.aux <- test[,-c(1,30)]
t.out<-svd(t.aux[,1:ncol(t.aux)])

t.us <- as.matrix(t.out$u[, 1:depth])
t.vs <- as.matrix(t.out$v[, 1:depth])
t.ds <- as.matrix(diag(t.out$d)[1:depth, 1:depth])
t.Xreduced <- t.us %*% t.ds
t.SVDFrame <- as.data.frame(cbind(t.Xreduced))
t.SVDFrame[,ncol(t.SVDFrame)] <- factor(t.SVDFrame[,ncol(t.SVDFrame)])

test2<-t.SVDFrame


Xtrain <- training[ -c(depth)]

Ytrain <- factor(training[c(depth)][,1])

testPredictions <- knn(train=Xtrain, test=test2, cl=Ytrain, k=1)

length(Xtrain)
length(test2)
length(Ytrain)

names(Xtrain)
names(test2)
