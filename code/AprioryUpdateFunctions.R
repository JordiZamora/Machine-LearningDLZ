corrector <- function (x, badcovers, newcover){
  #Substitute labels in badcovers by newcover label
  for (i in badcovers){
    x[x[,2]==i,2] <- newcover 
  }
  return(x)
}

aPrioryUpdate <- function(Testdata, submit){
  #A priory corrections according to the table of wild_area and soil_type occurrence
  #The new cover is selected among the cover types predicted without counts in the train wild and soil types
  #The a priory substitution is applyed to soiltypes that uniquely identify the cover type. For soils and
  #wilds with counts >4% we update the cover type to the most probable if the predicted cover was not observed
  #in that soil or wild type for the training data.

  submitCorrected <- submit
  
  ##### Uniquely defined covers
  ids <- Testdata[Testdata[,c("soil_type_7")]==1,"id"] #rows with targeted data
  #Correction
  zeros <- c(1,3,4,5,6,7) #Not observed covertypes
  mfct <- 2 #most frequent covertype
  submitCorrected[submit[,"id"] %in% ids,] <- corrector(submit[submit[,"id"] %in% ids,],zeros,mfct)
  
  ids <- Testdata[Testdata[,c("soil_type_37")]==1,"id"]
  #Correction
  zeros <- c(1,2,3,4,5,6) #Not observed covertypes
  mfct <- 7 #most frequent covertype
  submitCorrected[submit[,"id"] %in% ids,] <- corrector(submit[submit[,"id"] %in% ids,],zeros,mfct)
  
  
  ######soils and wilds with counts > 2000
  
  ids <- Testdata[Testdata[,c("wild_area_1")]==1,"id"]
  #Correction
  zeros <- c(3,6) #Not observed covertypes
  mfct <- 2 #most frequent covertype
  submitCorrected[submit[,"id"] %in% ids,] <- corrector(submit[submit[,"id"] %in% ids,],zeros,mfct)
  
  ids <- Testdata[Testdata[,c("wild_area_2")]==1,"id"]
  #Correction
  zeros <- c(3,4,5,6) #Not observed covertypes
  mfct <- 1 #most frequent covertype
  submitCorrected[submit[,"id"] %in% ids,] <- corrector(submit[submit[,"id"] %in% ids,],zeros,mfct)
  
  ids <- Testdata[Testdata[,c("wild_area_3")]==1,"id"]
  #Correction
  zeros <- c(4) #Not observed covertypes
  mfct <- 2 #most frequent covertype
  submitCorrected[submit[,"id"] %in% ids,] <- corrector(submit[submit[,"id"] %in% ids,],zeros,mfct)
  
  
  ids <- Testdata[Testdata[,c("wild_area_4")]==1,"id"]
  #Correction
  zeros <- c(1,5,7) #Not observed covertypes
  mfct <- 3 #most frequent covertype
  submitCorrected[submit[,"id"] %in% ids,] <- corrector(submit[submit[,"id"] %in% ids,],zeros,mfct)
  
  ids <- Testdata[Testdata[,c("soil_type_10")]==1,"id"]
  #Correction
  zeros <- c(7) #Not observed covertypes
  mfct <- 3 #most frequent covertype
  submitCorrected[submit[,"id"] %in% ids,] <- corrector(submit[submit[,"id"] %in% ids,],zeros,mfct)
  
  ids <- Testdata[Testdata[,c("soil_type_12")]==1,"id"]
  #Correction
  zeros <- c(3,4,5,6,7) #Not observed covertypes
  mfct <- 2 #most frequent covertype
  submitCorrected[submit[,"id"] %in% ids,] <- corrector(submit[submit[,"id"] %in% ids,],zeros,mfct)
  
  ids <- Testdata[Testdata[,c("soil_type_22")]==1,"id"]
  #Correction
  zeros <- c(3,4,5,6) #Not observed covertypes
  mfct <- 1 #most frequent covertype
  submitCorrected[submit[,"id"] %in% ids,] <- corrector(submit[submit[,"id"] %in% ids,],zeros,mfct)
  
  ids <- Testdata[Testdata[,c("soil_type_23")]==1,"id"]
  #Correction
  zeros <- c(3,4) #Not observed covertypes
  mfct <- 1 #most frequent covertype
  submitCorrected[submit[,"id"] %in% ids,] <- corrector(submit[submit[,"id"] %in% ids,],zeros,mfct)
  
  
  ids <- Testdata[Testdata[,c("soil_type_29")]==1,"id"]
  #Correction
  zeros <- c(3,4,6) #Not observed covertypes
  mfct <- 2 #most frequent covertype
  submitCorrected[submit[,"id"] %in% ids,] <- corrector(submit[submit[,"id"] %in% ids,],zeros,mfct)
  
  
  ids <- Testdata[Testdata[,c("soil_type_30")]==1,"id"]
  #Correction
  zeros <- c(3,4,6) #Not observed covertypes
  mfct <- 2 #most frequent covertype
  submitCorrected[submit[,"id"] %in% ids,] <- corrector(submit[submit[,"id"] %in% ids,],zeros,mfct)
  
  
  ids <- Testdata[Testdata[,c("soil_type_31")]==1,"id"]
  #Correction
  zeros <- c(3,4) #Not observed covertypes
  mfct <- 2 #most frequent covertype
  submitCorrected[submit[,"id"] %in% ids,] <- corrector(submit[submit[,"id"] %in% ids,],zeros,mfct)
  
  ids <- Testdata[Testdata[,c("soil_type_32")]==1,"id"]
  #Correction
  zeros <- c(4) #Not observed covertypes
  mfct <- 2 #most frequent covertype
  submitCorrected[submit[,"id"] %in% ids,] <- corrector(submit[submit[,"id"] %in% ids,],zeros,mfct)
  
  ids <- Testdata[Testdata[,c("soil_type_33")]==1,"id"]
  #Correction
  zeros <- c(4) #Not observed covertypes
  mfct <- 2 #most frequent covertype
  submitCorrected[submit[,"id"] %in% ids,] <- corrector(submit[submit[,"id"] %in% ids,],zeros,mfct)
  return (submitCorrected)
}