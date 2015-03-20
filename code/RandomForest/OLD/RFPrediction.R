if (!require("foreach")) install.packages("foreach")
if (!require("doSNOW")) install.packages("doSNOW")
if (!require("randomForest")) install.packages("randomForest")
#Load data
Traindata <- read.csv("../data/Kaggle_Covertype_training.csv")
Testdata <- read.csv("../data/Kaggle_Covertype_test.csv")
numcolTrain <- ncol(Traindata)
numcolTest <- ncol(Testdata)
Traindata[,numcolTrain] <- factor(Traindata[,numcolTrain]) #Classes as factors

# Set Parameters
MCiter <- 4
mtry <- 20
ntree <- 10
cores <- 4

# Dimension Reduction
TraindataB <- Traindata
TestdataB <- Testdata
#TrainSimplif <- dataset[,-c(22,30,52)]

# Separate features and labels
# Trainfeatures <- TraindataB[1:7000,-c(1,numcolTrain)]
# Trainlabels <- TraindataB[1:7000,numcolTrain]
Trainfeatures <- TraindataB[,-c(1,numcolTrain)]
Trainlabels <- TraindataB[,numcolTrain]

#Split the test data in n cores
TestdataBList <- split(TestdataB, seq(1,cores))

#Run random forest
cl <- makeCluster(cores, type="SOCK", outfile="") 
registerDoSNOW(cl)  

results<- foreach(TestdataC = TestdataBList, .packages="randomForest") %dopar% {
#  Testfeatures <- TestdataC[1:7000,-1]
  Testfeatures <- TestdataC[,-1]
  RFout <- randomForest(Trainfeatures, Trainlabels,
                        Testfeatures,
                        mtry=mtry, ntree=ntree)
  RFout
}
stopCluster(cl)

prediction <- c()
for (i in 1:cores){
  prediction <- rbind(prediction, 
                      cbind(names(results[[i]]$test$predicted),
                            results[[i]]$test$predicted))
}
