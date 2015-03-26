# Load funtions to substitute predictions according to a priory knowledge of the data
source('AprioryUpdateFunctions.R')

#Load predicted data
predictions <- read.csv("../kNN/Cover_type_submission.csv")
#Load Test data
Testdata <- read.csv("../../data/Kaggle_Covertype_test.csv")

#Apply updates to the predicted labels
submitCorrected <- aPrioryUpdate (Testdata, predictions)
#Save updated predictions
write.csv(submitCorrected, file = "Cover_type_submission_CorrectedAPriory.csv", row.names=FALSE)