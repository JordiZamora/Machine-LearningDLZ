# SVM training using all data

dataFull <- read.table('Kaggle_Covertype_training.csv', header=TRUE, sep=",")
testD <- read.table('Kaggle_Covertype_test.csv', header=TRUE, sep=",")
testDd <- testD[ , -c(1, 30)]# taking out IDs and soil_type_15

n <- nrow(dataFull)
D <- ncol(dataFull)

leaveOut <- 0 # for cross validation - # 10% leave out - for cross-validation
train <- dataFull[1:(n*(1-leaveOut)), -c(1, 30)] # training data
nn <- nrow(train)
test <- testDd
# with SVMS

dims <- dim(train)
dims2 <- dim(test)
trainAdj <- matrix(0, dims[1], dims[2])
testAdj <- matrix(0, dims2[1], dims2[2])

for (i in 1:ncol(train)) {
  if (i <= 10) {
    trainAdj[, i] <- (train[, i]-mean(train[, i]))/sd(train[, i])
    testAdj[, i] <- (test[, i]- mean(test[, i]))/sd(test[, i])  
  } else {
    trainAdj[,i] <- train[, i]
    if (i <= ncol(testAdj)){
      testAdj[, i] <- test[, i] 
    }
  }
}

train <- trainAdj # took out soil type 15 and standardised continuous variables
test <- testAdj


# Increasing the C parameter improves in sample fit
modelAll1_2 <- ksvm(x=as.matrix(train[, 1:ncol(train)-1]), 
                    y=as.factor(train[, ncol(train)]),
                    C=5000)

errorInS <- modelAll1_2@error
predVector <- predict(modelAll1_2, test)
outPut <- data.frame(testD[, 1], as.numeric(predVector))
colnames(outPut) <- c("id", "Cover_Type")
write.table(outPut, "predictClass.csv", sep=",", row.names=FALSE, col.names=TRUE)
