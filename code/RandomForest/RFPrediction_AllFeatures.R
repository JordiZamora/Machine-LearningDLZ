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
mtry <- 39
ntree <- 1000
cores <- 4

# Dimension Reduction
#Train data
TraindataB <- Traindata[,-which(names(Traindata)=="soil_type_15")]

numcolTrain <- ncol(TraindataB)
numrowTrain <- nrow(TraindataB)

#Test data
TestdataB <- Testdata[,-which(names(Testdata)=="soil_type_15")]

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

write.csv(sortedResults, file = "Prediction_RF_AllFeatures.csv", row.names=FALSE)
