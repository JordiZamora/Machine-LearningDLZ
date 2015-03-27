setwd("C:/Users/Epoch/Desktop/BGSE/ML/Competition")
#install.packages("e1071")
#install.packages("rpart")
#install.packages("kernlab")
library("kernlab")
library("e1071")
library("rpart")

dataFull <- read.table('Kaggle_Covertype_training.csv', header=TRUE, sep=",")
n <- nrow(dataFull)
d <- ncol(dataFull)

leaveOut <- 0.8 # for cross validation - # 10% leave out - for cross-validation
train <- dataFull[1:(n*(1-leaveOut)), 2:d] # training data
nn <- nrow(train)
test <- dataFull[(nn+1):(nn+400), 2:d-1]
trueClass <- dataFull[(nn+1):(nn+400), d]

# one against all application of SVM
yMat <- matrix(0, nn, 7)
for (i in 1:7) {
  ind <- which(train[, 55] %in% i)
  yMat[ind, i] <- 1
  yMat[-ind, i] <- -1
}

trueYMat <- matrix(0, length((nn+1):(nn+400)), 7)
for (i in 1:7) {
  ind <- which(trueClass %in% i)
  trueYMat[ind, i] <- 1
  trueYMat[-ind, i] <- -1
}

# Classifying using one-against all and error is calculated based on majority vote
modelAll <- ksvm(x=as.matrix(train[, 2:(d-1)]), y=as.factor(train[, 55]), C=1, kernel="polydot")
# with C=0.01, kernel="polydot" error rate 28.77% insample
# Pair-wise where every class is done manually
model1 <- ksvm(x=as.matrix(train[, 1:(d-1)]), y=as.matrix(yMat[, 1]), C=1) # bad results
model2 <- ksvm(x=as.matrix(train[, 1:(d-1)]), y=as.matrix(yMat[, 2]), C=1) # bad results
model3 <- ksvm(x=as.matrix(train[, 1:(d-1)]), y=as.matrix(yMat[, 3]), C=1) # bad results
model4 <- ksvm(x=as.matrix(train[, 1:(d-1)]), y=as.matrix(yMat[, 4]), C=1) # good results
model5 <- ksvm(x=as.matrix(train[, 1:(d-1)]), y=as.matrix(yMat[, 5]), C=1) # good results
model6 <- ksvm(x=as.matrix(train[, 1:(d-1)]), y=as.matrix(yMat[, 6]), C=1) # good results
model7 <- ksvm(x=as.matrix(train[, 1:(d-1)]), y=as.matrix(yMat[, 7]), C=1) # good results

#tune - function for cross validation
crossVal <- function(trueClass, predV) {
  # error for support vector machines
  a <- sum(sign(trueClass)==sign(predV))/length(predV)
  return (1-a)
}

predV <- predict(model7, test, type="response")
error <- crossVal(trueYMat[, 7], predV)

# now write the combinational model - traning 1:3 classes on classes 1:3 data and 4:7 on all data
model4 <- ksvm(x=as.matrix(train[, 1:(d-1)]), y=as.matrix(yMat[, 4]), C=1) # good results
model5 <- ksvm(x=as.matrix(train[, 1:(d-1)]), y=as.matrix(yMat[, 5]), C=1) # good results
model6 <- ksvm(x=as.matrix(train[, 1:(d-1)]), y=as.matrix(yMat[, 6]), C=1) # good results
model7 <- ksvm(x=as.matrix(train[, 1:(d-1)]), y=as.matrix(yMat[, 7]), C=1) # good results

#data for 1:3 classes

indTemp <- which(train[, d-1] %in% c(1, 2, 3))
data3Cl <- train[indTemp, ]
model13cl <- ksvm(x=as.matrix(data3Cl[, 2:(d-1)]), y=as.factor(data3Cl[, 55]), C=1, kernel="polydot", prob.model = TRUE)

errVec <- matrix(0, 4,2)
errVec <- as.data.frame(errVec)
pred
for (i in 1:4) {
  nameM <- paste("model", toString(i+3), sep= "")
  predVec <- predict(nameM, test)
  errVec[i, 1] <- nameM
  errVec[i, 2] <- crossVal(nameM, predV)
 #errVec[i, 2] <- 8-i+runif(1)
}

listM <- errVec[with(errVec, order(V2)), 1] #ordered by accuracy list of SVM models

svmMagic <- function(modelList, modelNameVec, newData) {
# modelList has to be a list of ksvm obejects sorted in such a descending order of accuracy
  # last element in modelList is the general model to use if binary pair wise classification fails
# modelNameVec contains model names 
  resultVec <- rep(0, nrow(newData))
  for (j in nrow(newData)) {
    for (i in 1:length(listM-1)) {
      class <- as.numeric(substr(modelNameVec[i], 6, 6))
      modelM <- listM[[i]]
      pred <- predict(modelM, newData[j, ])
      if (pred>0) {
        resultVec[j] <- class
        break
      }
    }
    if (resultVec[j] == 0) {
      predClass <- predict(modelList[[length(modelList)]], newData[j, ], type="response")
      resultVec[j] <- as.numeric(predClass[1]) 
    }
    
  }
  return(resultVec)
}

