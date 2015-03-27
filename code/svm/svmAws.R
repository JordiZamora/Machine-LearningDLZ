setwd("C:/Users/Epoch/Desktop/BGSE/ML/Competition")
install.packages("e1071")
install.packages("rpart")
install.packages("kernlab")
library("kernlab")
library("e1071")
library("rpart")

#developments made on the multiSVM.R # include standardisation of the continuous variables and no soil_type_15

dataFull <- read.table('Kaggle_Covertype_training.csv', header=TRUE, sep=",")

n <- nrow(dataFull)
d <- ncol(dataFull)

leaveOut <- 0.8 # for cross validation - # 10% leave out - for cross-validation
train <- dataFull[1:(n*(1-leaveOut)), 2:d] # training data
nn <- nrow(train)
test <- dataFull[(nn+1):(nn+400), 3:d-1]
trueClass <- dataFull[(nn+1):(nn+400), d]

dims <- dim(train)
dims2 <- dim(test)
trainAdj <- matrix(0, dims[1], dims[2]-1)
testAdj <- matrix(0, dims2[1], dims2[2]-1)

for (i in 1:10) {
  trainAdj[, i] <- (train[, i]-mean(train[, i]))/sd(train[, i])
  testAdj[, i] <- (test[, i]- mean(test[, i]))/sd(test[, i])
  
}
for (i in 11:28) {
  trainAdj[, i] <- train[, i]
  testAdj[, i] <- test[, i]
}

for (i in 29:ncol(trainAdj)) {
  trainAdj[, i] <- train[, i+1]
  if (i <= ncol(testAdj)) {
    testAdj[, i] <- test[, i+1]  
  }
  
}
train <- trainAdj # took out soil type 15 and standardised continuous variables
test <- testAdj

#tune - function for cross validation
crossVal <- function(trueClass, predV) {
  # error for support vector machines
  a <- sum(sign(trueClass)==sign(predV))/length(predV)
  return (1-a)
}


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
d <- ncol(train)
# now write the combinational model
model1 <- ksvm(x=as.matrix(train[, 1:(d-1)]), y=as.matrix(yMat[, 1]), C=1) 
model2 <- ksvm(x=as.matrix(train[, 1:(d-1)]), y=as.matrix(yMat[, 2]), C=1) 
model3 <- ksvm(x=as.matrix(train[, 1:(d-1)]), y=as.matrix(yMat[, 3]), C=1)
model4 <- ksvm(x=as.matrix(train[, 1:(d-1)]), y=as.matrix(yMat[, 4]), C=1) 
model5 <- ksvm(x=as.matrix(train[, 1:(d-1)]), y=as.matrix(yMat[, 5]), C=1) 
model6 <- ksvm(x=as.matrix(train[, 1:(d-1)]), y=as.matrix(yMat[, 6]), C=1) 
model7 <- ksvm(x=as.matrix(train[, 1:(d-1)]), y=as.matrix(yMat[, 7]), C=1) 

# the error results are different every time you run the above models
# - because maybe the optimisation routine is shaky

# lets try some different kernels
model1 <- ksvm(x=as.matrix(train[, 1:(d-1)]), y=as.matrix(yMat[, 1]), C=1,, kernel="polydot") 
model2 <- ksvm(x=as.matrix(train[, 1:(d-1)]), y=as.matrix(yMat[, 2]), C=1, kernel="polydot") 
model3 <- ksvm(x=as.matrix(train[, 1:(d-1)]), y=as.matrix(yMat[, 3]), C=1, kernel="polydot")
model4 <- ksvm(x=as.matrix(train[, 1:(d-1)]), y=as.matrix(yMat[, 4]), C=1, kernel="polydot") 
model5 <- ksvm(x=as.matrix(train[, 1:(d-1)]), y=as.matrix(yMat[, 5]), C=1, kernel="polydot") 
model6 <- ksvm(x=as.matrix(train[, 1:(d-1)]), y=as.matrix(yMat[, 6]), C=1, kernel="polydot") 
model7 <- ksvm(x=as.matrix(train[, 1:(d-1)]), y=as.matrix(yMat[, 7]), C=1, kernel="polydot") 

# Classifying using one-against all and error is calculated based on majority vote
modelAll <- ksvm(x=as.matrix(train[, 1:ncol(train)-1]), 
                 y=as.factor(train[, ncol(train)]),
                 C=1, kernel="polydot")



errVec <- matrix(0, 4,2)
errVec <- as.data.frame(errVec)

pred4 <- predict(model4, test, type="response")
pred5 <- predict(model5, test, type="response")
pred6 <- predict(model6, test, type="response")
pred7 <- predict(model7, test, type="response")

predList <-list(pred4, pred5, pred6, pred7)

for (i in 1:4) {
  nameM <- paste("model", toString(i+3), sep= "")
  errVec[i, 1] <- nameM
  errVec[i, 2] <- crossVal(trueClass, predList[[i]])
  #errVec[i, 2] <- 8-i+runif(1)
}

listM <- errVec[with(errVec, order(V2)), 1] #ordered by accuracy list of SVM models
