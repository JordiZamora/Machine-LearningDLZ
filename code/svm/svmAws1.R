setwd("C:/Users/Epoch/Desktop/BGSE/ML/Competition")
install.packages("e1071")
install.packages("rpart")
install.packages("kernlab")
library("kernlab")
library("e1071")
library("rpart")

dataFull <- read.table('Kaggle_Covertype_training.csv', header=TRUE, sep=",")

n <- nrow(dataFull)
D <- ncol(dataFull)

leaveOut <- 0.8 # for cross validation - # 10% leave out - for cross-validation
train <- dataFull[1:(n*(1-leaveOut)), 2:D] # training data
nn <- nrow(train)
test <- dataFull[(nn+1):(nn+400), 3:D-1]
testNsc <- test
trainNsc <- train # to preserve the non-scaled variables - works a lot better 
# with SVMS
trueClass <- as.numeric(dataFull[(nn+1):(nn+400), D])

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

######  Now removing soil type 15 for unscaled variables the same thing to unscaled variables
trainAdjNS <- trainNsc[, -29]
testAdjNS <- testNsc[, -29]

# one against all application of SVM - by classes
yMat <- matrix(0, nn, 7)
for (i in 1:7) {
  ind <- which(train[, ncol(train)] %in% i)
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
modelAll1_1 <- ksvm(x=as.matrix(train[, 1:ncol(train)-1]), 
                    y=as.factor(train[, ncol(train)]),
                    C=1)

### trying various kernels and C params 
# Increasing the C parameter improves in sample fit
modelAll1_2 <- ksvm(x=as.matrix(train[, 1:ncol(train)-1]), 
                    y=as.factor(train[, ncol(train)]),
                    C=1000, cross=10)
pred <- predict(modelAll1_2, test)
err <- crossVal(as.numeric(pred), trueClass)

modelAll1 <- ksvm(x=as.matrix(train[, 1:ncol(train)-1]), 
                  y=as.factor(train[, ncol(train)]),
                  C=1, kernel="polydot")
## now trying to 





d <- ncol(train)
# now write the combinational model
model1 <- ksvm(x=as.matrix(train[, 1:(d-1)]), y=as.matrix(yMat[, 1]), C=1) 
model2 <- ksvm(x=as.matrix(train[, 1:(d-1)]), y=as.matrix(yMat[, 2]), C=1) 
model3 <- ksvm(x=as.matrix(train[, 1:(d-1)]), y=as.matrix(yMat[, 3]), C=1)
model4 <- ksvm(x=as.matrix(train[, 1:(d-1)]), y=as.matrix(yMat[, 4]), C=1) 
model5 <- ksvm(x=as.matrix(train[, 1:(d-1)]), y=as.matrix(yMat[, 5]), C=1) 
model6 <- ksvm(x=as.matrix(train[, 1:(d-1)]), y=as.matrix(yMat[, 6]), C=1) 
model7 <- ksvm(x=as.matrix(train[, 1:(d-1)]), y=as.matrix(yMat[, 7]), C=1) 

errorList1 <- c(model1@error, model2@error, model3@error, model4@error,
                model5@error, model6@error, model7@error)

# the error results are different every time you run the above models
# - because maybe the optimisation routine is shaky

# also much better results were observed using non-scaled data

# lets try some different kernels
model11 <- ksvm(x=as.matrix(train[, 1:(d-1)]), y=as.matrix(yMat[, 1]), C=1, kernel="tanhdot") 
model22 <- ksvm(x=as.matrix(train[, 1:(d-1)]), y=as.matrix(yMat[, 2]), C=1, kernel="tanhdot") 
model33 <- ksvm(x=as.matrix(train[, 1:(d-1)]), y=as.matrix(yMat[, 3]), C=1, kernel="tanhdot")
model44 <- ksvm(x=as.matrix(train[, 1:(d-1)]), y=as.matrix(yMat[, 4]), C=1, kernel="tanhdot") 
model55 <- ksvm(x=as.matrix(train[, 1:(d-1)]), y=as.matrix(yMat[, 5]), C=1, kernel="tanhdot") 
model66 <- ksvm(x=as.matrix(train[, 1:(d-1)]), y=as.matrix(yMat[, 6]), C=1, kernel="tanhdot") 
model77 <- ksvm(x=as.matrix(train[, 1:(d-1)]), y=as.matrix(yMat[, 7]), C=1, kernel="tanhdot") 
# polydot is pretty terrible gives higher errors than rbfdot
errorList2 <- c(model11@error, model22@error, model33@error, model44@error,
                model55@error, model66@error, model77@error)


################ Trying pairwise SVM with non-scaled variables

# 
model1_1 <- ksvm(x=as.matrix(trainAdjNS[, 1:(d-1)]), y=as.matrix(yMat[, 1]), C=1) 
model2_2 <- ksvm(x=as.matrix(trainAdjNS[, 1:(d-1)]), y=as.matrix(yMat[, 2]), C=1) 
model3_3 <- ksvm(x=as.matrix(trainAdjNS[, 1:(d-1)]), y=as.matrix(yMat[, 3]), C=1)
model4_4 <- ksvm(x=as.matrix(trainAdjNS[, 1:(d-1)]), y=as.matrix(yMat[, 4]), C=1) 
model5_5 <- ksvm(x=as.matrix(trainAdjNS[, 1:(d-1)]), y=as.matrix(yMat[, 5]), C=1) 
model6_6 <- ksvm(x=as.matrix(trainAdjNS[, 1:(d-1)]), y=as.matrix(yMat[, 6]), C=1) 
model7_7 <- ksvm(x=as.matrix(trainAdjNS[, 1:(d-1)]), y=as.matrix(yMat[, 7]), C=1) 
# polydot is pretty terrible gives higher errors than rbfdot
errorList3 <- c(model1_1@error, model2_2@error, model3_3@error, model4_4@error,
                model5_5@error, model6_6@error, model7_7@error)
