corrector <- function (x, badcovers, newcover){
  for (i in badcovers){
    x[x[,2]==i,2] <- newcover 
  }
  return(x)
}

submit <- read.csv("../kNN/Cover_type_submission.csv")
submitCorrected <- submit

#A priory corrections according to the table of wild_area and soil_type occurrence
#The new cover is selected among the cover types predicted without counts in the train wild and soil types

# Uniquely defined covers
ids <- Testdata[Testdata[,c("soil_type_7")]==1,"id"]
#Correction
submitCorrected[submit[,"id"] %in% ids,] <- corrector(submit[submit[,"id"] %in% ids,],c(1,3,4,5,6,7),2)

ids <- Testdata[Testdata[,c("soil_type_37")]==1,"id"]
#Correction
submitCorrected[submit[,"id"] %in% ids,] <- corrector(submit[submit[,"id"] %in% ids,],c(1,2,3,4,5,6),7)


#soils and wilds with counts > 2000

ids <- Testdata[Testdata[,c("wild_area_1")]==1,"id"]
table(submit[submit[,"id"] %in% ids,2])
#Correction
submitCorrected[submit[,"id"] %in% ids,] <- corrector(submit[submit[,"id"] %in% ids,],c(3,6),2)
table(submitCorrected[submit[,"id"] %in% ids,2])


ids <- Testdata[Testdata[,c("wild_area_2")]==1,"id"]
table(submit[submit[,"id"] %in% ids,2])
#Correction
submitCorrected[submit[,"id"] %in% ids,] <- corrector(submit[submit[,"id"] %in% ids,],c(3,4,5,6),1)

ids <- Testdata[Testdata[,c("wild_area_3")]==1,"id"]
#Correction
submitCorrected[submit[,"id"] %in% ids,] <- corrector(submit[submit[,"id"] %in% ids,],c(4),2)


ids <- Testdata[Testdata[,c("wild_area_4")]==1,"id"]
#Correction
submitCorrected[submit[,"id"] %in% ids,] <- corrector(submit[submit[,"id"] %in% ids,],c(1,5,7),3)

ids <- Testdata[Testdata[,c("soil_type_10")]==1,"id"]
#Correction
submitCorrected[submit[,"id"] %in% ids,] <- corrector(submit[submit[,"id"] %in% ids,],c(7),3)

ids <- Testdata[Testdata[,c("soil_type_12")]==1,"id"]
#Correction
submitCorrected[submit[,"id"] %in% ids,] <- corrector(submit[submit[,"id"] %in% ids,],c(3,4,5,6,7),2)

ids <- Testdata[Testdata[,c("soil_type_22")]==1,"id"]
#Correction
submitCorrected[submit[,"id"] %in% ids,] <- corrector(submit[submit[,"id"] %in% ids,],c(3,4,5,6),1)

ids <- Testdata[Testdata[,c("soil_type_23")]==1,"id"]
#Correction
submitCorrected[submit[,"id"] %in% ids,] <- corrector(submit[submit[,"id"] %in% ids,],c(3,4),1)


ids <- Testdata[Testdata[,c("soil_type_29")]==1,"id"]
#Correction
submitCorrected[submit[,"id"] %in% ids,] <- corrector(submit[submit[,"id"] %in% ids,],c(3,4,6),2)


ids <- Testdata[Testdata[,c("soil_type_30")]==1,"id"]
#Correction
submitCorrected[submit[,"id"] %in% ids,] <- corrector(submit[submit[,"id"] %in% ids,],c(3,4,6),2)


ids <- Testdata[Testdata[,c("soil_type_31")]==1,"id"]
#Correction
submitCorrected[submit[,"id"] %in% ids,] <- corrector(submit[submit[,"id"] %in% ids,],c(3,4),2)

ids <- Testdata[Testdata[,c("soil_type_32")]==1,"id"]
#Correction
submitCorrected[submit[,"id"] %in% ids,] <- corrector(submit[submit[,"id"] %in% ids,],c(4),2)

ids <- Testdata[Testdata[,c("soil_type_33")]==1,"id"]
#Correction
submitCorrected[submit[,"id"] %in% ids,] <- corrector(submit[submit[,"id"] %in% ids,],c(4),2)



write.csv(submitCorrected, file = "Prediction_1NN_CorrectedAPriory.csv", row.names=FALSE)


# 
# #soils and wilds with counts > 3000
#submission 90.820 Not better
# 
# ids <- Testdata[Testdata[,c("wild_area_1")]==1,"id"]
# table(submit[submit[,"id"] %in% ids,2])
# #Correction
# submitCorrected[submit[,"id"] %in% ids,] <- corrector(submit[submit[,"id"] %in% ids,],c(3,6),2)
# table(submitCorrected[submit[,"id"] %in% ids,2])
# 
# 
# ids <- Testdata[Testdata[,c("wild_area_3")]==1,"id"]
# #Correction
# submitCorrected[submit[,"id"] %in% ids,] <- corrector(submit[submit[,"id"] %in% ids,],c(4),2)
# 
# 
# ids <- Testdata[Testdata[,c("wild_area_4")]==1,"id"]
# #Correction
# submitCorrected[submit[,"id"] %in% ids,] <- corrector(submit[submit[,"id"] %in% ids,],c(1,5,7),3)
# 
# ids <- Testdata[Testdata[,c("soil_type_23")]==1,"id"]
# #Correction
# submitCorrected[submit[,"id"] %in% ids,] <- corrector(submit[submit[,"id"] %in% ids,],c(3,4),1)
# 
# 
# ids <- Testdata[Testdata[,c("soil_type_29")]==1,"id"]
# #Correction
# submitCorrected[submit[,"id"] %in% ids,] <- corrector(submit[submit[,"id"] %in% ids,],c(3,4,6),2)
# 
# 
# ids <- Testdata[Testdata[,c("soil_type_32")]==1,"id"]
# #Correction
# submitCorrected[submit[,"id"] %in% ids,] <- corrector(submit[submit[,"id"] %in% ids,],c(4),2)
# 
# ids <- Testdata[Testdata[,c("soil_type_33")]==1,"id"]
# #Correction
# submitCorrected[submit[,"id"] %in% ids,] <- corrector(submit[submit[,"id"] %in% ids,],c(4),2)
# 
# 
# 
# write.csv(submitCorrected, file = "Prediction_1NN_CorrectedAPriory3000.csv", row.names=FALSE)



#soils and wilds with counts > 2000 and also cases with prob counts >70

# Uniquely defined covers
ids <- Testdata[Testdata[,c("soil_type_7")]==1,"id"]
#Correction
submitCorrected[submit[,"id"] %in% ids,] <- corrector(submit[submit[,"id"] %in% ids,],c(1,3,4,5,6,7),2)

ids <- Testdata[Testdata[,c("soil_type_37")]==1,"id"]
#Correction
submitCorrected[submit[,"id"] %in% ids,] <- corrector(submit[submit[,"id"] %in% ids,],c(1,2,3,4,5,6),7)

#Good guesses
ids <- Testdata[Testdata[,c("soil_type_8")]==1,"id"]
#Correction
submitCorrected[submit[,"id"] %in% ids,] <- corrector(submit[submit[,"id"] %in% ids,],c(3,4,5,6,7),2)


ids <- Testdata[Testdata[,c("soil_type_9")]==1,"id"]
#Correction
submitCorrected[submit[,"id"] %in% ids,] <- corrector(submit[submit[,"id"] %in% ids,],c(3,4,5,6,7),2)


ids <- Testdata[Testdata[,c("soil_type_11")]==1,"id"]
#Correction
submitCorrected[submit[,"id"] %in% ids,] <- corrector(submit[submit[,"id"] %in% ids,],c(4,7),2)


ids <- Testdata[Testdata[,c("soil_type_13")]==1,"id"]
#Correction
submitCorrected[submit[,"id"] %in% ids,] <- corrector(submit[submit[,"id"] %in% ids,],c(3,4,7),2)


ids <- Testdata[Testdata[,c("soil_type_18")]==1,"id"]
#Correction
submitCorrected[submit[,"id"] %in% ids,] <- corrector(submit[submit[,"id"] %in% ids,],c(3,4,6,7),2)


ids <- Testdata[Testdata[,c("soil_type_21")]==1,"id"]
#Correction
submitCorrected[submit[,"id"] %in% ids,] <- corrector(submit[submit[,"id"] %in% ids,],c(3,4,5,6),1)


ids <- Testdata[Testdata[,c("soil_type_26")]==1,"id"]
#Correction
submitCorrected[submit[,"id"] %in% ids,] <- corrector(submit[submit[,"id"] %in% ids,],c(3,4,6),2)


ids <- Testdata[Testdata[,c("soil_type_28")]==1,"id"]
#Correction
submitCorrected[submit[,"id"] %in% ids,] <- corrector(submit[submit[,"id"] %in% ids,],c(3,4,6,7),2)

ids <- Testdata[Testdata[,c("soil_type_34")]==1,"id"]
#Correction
submitCorrected[submit[,"id"] %in% ids,] <- corrector(submit[submit[,"id"] %in% ids,],c(3,4),2)


#Majority vote

ids <- Testdata[Testdata[,c("wild_area_1")]==1,"id"]
table(submit[submit[,"id"] %in% ids,2])
#Correction
submitCorrected[submit[,"id"] %in% ids,] <- corrector(submit[submit[,"id"] %in% ids,],c(3,6),2)
table(submitCorrected[submit[,"id"] %in% ids,2])


ids <- Testdata[Testdata[,c("wild_area_2")]==1,"id"]
table(submit[submit[,"id"] %in% ids,2])
#Correction
submitCorrected[submit[,"id"] %in% ids,] <- corrector(submit[submit[,"id"] %in% ids,],c(3,4,5,6),1)

ids <- Testdata[Testdata[,c("wild_area_3")]==1,"id"]
#Correction
submitCorrected[submit[,"id"] %in% ids,] <- corrector(submit[submit[,"id"] %in% ids,],c(4),2)


ids <- Testdata[Testdata[,c("wild_area_4")]==1,"id"]
#Correction
submitCorrected[submit[,"id"] %in% ids,] <- corrector(submit[submit[,"id"] %in% ids,],c(1,5,7),3)

ids <- Testdata[Testdata[,c("soil_type_10")]==1,"id"]
#Correction
submitCorrected[submit[,"id"] %in% ids,] <- corrector(submit[submit[,"id"] %in% ids,],c(7),3)

ids <- Testdata[Testdata[,c("soil_type_12")]==1,"id"]
#Correction
submitCorrected[submit[,"id"] %in% ids,] <- corrector(submit[submit[,"id"] %in% ids,],c(3,4,5,6,7),2)

ids <- Testdata[Testdata[,c("soil_type_22")]==1,"id"]
#Correction
submitCorrected[submit[,"id"] %in% ids,] <- corrector(submit[submit[,"id"] %in% ids,],c(3,4,5,6),1)

ids <- Testdata[Testdata[,c("soil_type_23")]==1,"id"]
#Correction
submitCorrected[submit[,"id"] %in% ids,] <- corrector(submit[submit[,"id"] %in% ids,],c(3,4),1)


ids <- Testdata[Testdata[,c("soil_type_29")]==1,"id"]
#Correction
submitCorrected[submit[,"id"] %in% ids,] <- corrector(submit[submit[,"id"] %in% ids,],c(3,4,6),2)


ids <- Testdata[Testdata[,c("soil_type_30")]==1,"id"]
#Correction
submitCorrected[submit[,"id"] %in% ids,] <- corrector(submit[submit[,"id"] %in% ids,],c(3,4,6),2)


ids <- Testdata[Testdata[,c("soil_type_31")]==1,"id"]
#Correction
submitCorrected[submit[,"id"] %in% ids,] <- corrector(submit[submit[,"id"] %in% ids,],c(3,4),2)

ids <- Testdata[Testdata[,c("soil_type_32")]==1,"id"]
#Correction
submitCorrected[submit[,"id"] %in% ids,] <- corrector(submit[submit[,"id"] %in% ids,],c(4),2)

ids <- Testdata[Testdata[,c("soil_type_33")]==1,"id"]
#Correction
submitCorrected[submit[,"id"] %in% ids,] <- corrector(submit[submit[,"id"] %in% ids,],c(4),2)



write.csv(submitCorrected, file = "Prediction_1NN_CorrectedAPriory2000AndP70.csv", row.names=FALSE)
