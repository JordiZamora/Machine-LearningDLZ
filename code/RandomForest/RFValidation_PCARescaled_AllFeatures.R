if (!require("foreach")) install.packages("foreach")
if (!require("doSNOW")) install.packages("doSNOW")
if (!require("randomForest")) install.packages("randomForest")
setwd("~/Documents/Cursos/DataScience/2ndTerm/HomeworkMachineLearning/Project/code/RandomForest")

#source("RF_XValidation.R")
source("RF_XValidationBuckets.R")


#Load data
rawdata <- read.csv("../../data/Kaggle_Covertype_training.csv")
numcol <- ncol(rawdata)
numrow <- nrow(rawdata)
#rawdata[,numcol] <- factor(rawdata[,numcol]) #Classes as factors

featuresRaw <- rawdata[,-c(1,numcol)]
featuresRaw <- featuresRaw[,-which(names(featuresRaw)=="soil_type_15")]
labels <- factor(rawdata[,numcol])

aux <- featuresRaw
aux <- scale(featuresRaw)
out<-princomp(aux)
features <- out$scores

t1 <- tuneRF(features,labels,mtryStart=30, ntreeTry=10, stepFactor=1.2, improve=0.01)
# mtry = 30  OOB error = 17.17% 
# Searching left ...
# mtry = 25   OOB error = 17.08% 
# 0.005401986 0.01 
# Searching right ...
# mtry = 36 	OOB error = 16.98% 
# 0.01116046 0.01 
# mtry = 43 	OOB error = 16.8% 
# 0.01044974 0.01 
# mtry = 50 	OOB error = 16.94% 
# -0.008260477 0.01 
t1 <- tuneRF(features,labels,mtryStart=30, ntreeTry=10, stepFactor=1.2, improve=0.01)
# mtry = 40  OOB error = 17.06% 
# Searching left ...
# mtry = 34   OOB error = 17.06% 
# 0.0001037287 0.01 
# Searching right ...
# mtry = 48 	OOB error = 16.75% 
# 0.01825001 0.01 
# mtry = 50 	OOB error = 17.13% 
# -0.02295389 0.01 

MError <- c()

for (j in 1:11){

  cat(j,"\n")
# Set Parameters
#MCiter <- 4
nBuckets <- 10
fraction <- 0.1
mtry <- 38
ntree <- round(10+190*(j-1)/10)
cores <- 4


# Parameters Optimization
#MCiterList <- split(rep(round(MCiter/cores), cores), seq(1,cores))
BucketsList <- split(seq(1,nBuckets), seq(1,cores))

cl <- makeCluster(cores, type="SOCK", outfile="") 
registerDoSNOW(cl)  

results<- foreach(Buckets = BucketsList, .combine = rbind) %dopar% {
  MisclassError <- XVal_RF(features, labels, 
                           Buckets=Buckets, nBuckets=nBuckets,
                           mtry=mtry, ntree=ntree)
  MisclassError
  
}

stopCluster(cl)

MError[j] <- colSums(results)
#For j From 1:11
#MError=
}
#####!!!!!!!!!!!!!!!!!!!!!! Check tuneRF function for initial search on mtry 