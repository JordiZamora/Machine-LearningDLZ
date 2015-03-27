########## Optimal prediction of forest covertypes using kNN ##########
#Load required packages
if (!require("foreach")) install.packages("foreach")
if (!require("doSNOW")) install.packages("doSNOW")
if (!require("class")) install.packages("class")
if (!require("plyr")) install.packages("plyr")
if (!require("dplyr")) install.packages("dplyr")
source("CV_Optimalw.R") #Load in cross validation function for weights
source("CV_Optimalk.R") #Load in cross validation function for k


########### Loading the data ########### 
#Load training and test sets
rawdata <- read.csv("../../data/Kaggle_Covertype_training.csv") #Training
rawTestdata <- read.csv("../../data/Kaggle_Covertype_test.csv") #Test
numcol <- ncol(rawdata)
rawdata[,numcol] <- factor(rawdata[,numcol]) #Classes as factors

########### Parameter optimization ########### 
set.seed(10) #set seed for reproducibility
ncores <- 8 #Number of cores

#k Optimization. No rescaling of continuous features
ks<-c(1,3, 5, 7,9) #Number of NN
#run function with cross validation to choose k
results<-optimalK(rawdata, 
                  10, 
                  ks) #see CV_Optimalk.R
optK<-aggregate(results$error[-1], list(results$k[-1]),mean)
k<-optK[which.min(optK[,2]),1] #Optimal k


#Optimization of weights with cross-validation
MisclassErrOut <- c()
for (i in 1:26){
  weights <- 1+250*(i-1)/25 #Tested weights
  MisclassErr <- Misclass1NN(weights = weights, 
                             rawdata = rawdata, 
                             cores = ncores, 
                             k = k) #see CV_Parallel_kNN.R
  cat("i=", i, " ",c(MisclassErr,weights), "\n")
  MisclassErrOut <- rbind(MisclassErrOut, 
                          c(MisclassErr,weights)) #Data frame of results
}

#Save the misclassification errors for different weights
#write.csv(MisclassErrOut, file = "1NN_OptimalWeight.csv", row.names=FALSE)


########### Classification of Test set ########### 
weights <- MisclassErrOut[which.min(MisclassErrOut[,1]),2] #Select the optimal weight

#Build training and testing sets. It selects all the training set for training
train <- rawdata[,-c(1,30)] #remove Ids and soil_type_15
columnsAux <- ncol(train)

#Training data
train[,11:(columnsAux-1)] <- train[,11:(columnsAux-1)]*weights #weighted binary columns
Xtrain <- train[,-columnsAux] #matrix of features
Ytrain <- factor(train[,columnsAux]) #matrix of labels

#Testing data
Xtest <- rawTestdata[,-c(1,30)] #Remove Ids and soil_type_15. Only features on it
Xtest[,11:(columnsAux-1)] <- Xtest[,11:(columnsAux-1)]*weights #weighted binary columns

#Classification of the testing set
testPredictions <- knn(train=Xtrain, 
                       test=Xtest, 
                       cl=Ytrain, 
                       k=k)

########### Saving the predicted covertypes ########### 
#Save the predicted classes in the Kaggle format
prediction<-data.frame(rawTestdata[,1],testPredictions)
names(prediction) <- c("id", "Cover_Type")
write.csv(prediction, file = "1NN_PredictionWeight.csv", row.names=FALSE)
