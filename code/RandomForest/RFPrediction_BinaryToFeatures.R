#This fail is not the original. May contain errorsi

if (!require("foreach")) install.packages("foreach")
if (!require("doSNOW")) install.packages("doSNOW")
if (!require("randomForest")) install.packages("randomForest")
setwd("~/Documents/Cursos/DataScience/2ndTerm/HomeworkMachineLearning/Project/code/RandomForest")
#Load data
Traindata <- read.csv("../../data/Kaggle_Covertype_training.csv")
Testdata <- read.csv("../../data/Kaggle_Covertype_test.csv")
numcolTrain <- ncol(Traindata)
numcolTest <- ncol(Testdata)
Traindata[,numcolTrain] <- factor(Traindata[,numcolTrain]) #Classes as factors

# Set Parameters
mtry <- 10
ntree <- 96
cores <- 4

# Dimension Reduction
#Train data
names <- names(Traindata)[12:55]
dataset <- Traindata
TraindataB <- dataset[,1:11]
TraindataB <- cbind(TraindataB, wild=rep(NA,numrow), soil=rep(NA,numrow))

#dataset2[,12:13] <- factor(t(apply(dataset[,12:55], 1, function(x) names[x==1])))
names <- c(seq(1,4),seq(1,40))
TraindataB[,12:13] <- t(apply(dataset[,12:55], 1, function(x) names[x==1]))
TraindataB[,14] <- dataset[,numcol]

numcolTrain <- ncol(TraindataB)
numrowTrain <- nrow(TraindataB)

#Test data
names <- names(Testdata)[12:55]
dataset <- Testdata
TestdataB <- dataset[,1:11]
TestdataB <- cbind(TestdataB, wild=rep(NA,numrow), soil=rep(NA,numrow))

names <- c(seq(1,4),seq(1,40))
TestdataB[,12:13] <- t(apply(dataset[,12:55], 1, function(x) names[x==1]))
TestdataB[,14] <- dataset[,numcol]

numcolTest <- ncol(TestdataB)
numrowTest <- nrow(TestdataB)


# Separate features and labels
Trainfeatures <- TraindataB[,-c(1,numcolTrain)]
Trainlabels <- TraindataB[,numcolTrain]
Testfeatures <- TestdataB

#Split the test data in n cores
TestdataBList <- split(Testfeatures, seq(1,cores))

#Run random forest
cl <- makeCluster(cores, type="SOCK", outfile="") 
registerDoSNOW(cl)  

results<- foreach(Testfeatures = TestdataBList, .packages="randomForest", .combine=rbind) %dopar% {
  TestfeaturesAux <- Testfeatures[,-1]
  RFout <- randomForest(Trainfeatures, Trainlabels,
                        TestfeaturesAux,
                        mtry=mtry, ntree=ntree)
  prediction<-data.frame(Testfeatures[,1],RFout$test$predicted)
}
stopCluster(cl)

sortedResults <-results[order(results[,1]),]
names(sortedResults) <- c("id", "Cover_Type")

write.csv(sortedResults, file = "Prediction_RF_Buckets_DimRed1.csv", row.names=FALSE)
