# Load funtions to substitute predictions according to a priory knowledge of the data
source('AprioryUpdateFunctions.R')

#Load predicted data
predictions <- read.csv("kNN/1NN_PredictionWeight.csv")
#Load Test data
Testdata <- read.csv("../data/Kaggle_Covertype_test.csv")

#Apply updates to the predicted labels
submitCorrected <- aPrioryUpdate (Testdata, predictions)
#Save updated predictions
write.csv(submitCorrected, file = "kNN/1NN_PredictionWeight_UpdateAPriory.csv", row.names=FALSE)