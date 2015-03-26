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

features <- featuresRaw
t1 <- tuneRF(features,labels,mtryStart=20, ntreeTry=50, stepFactor=1.2, improve=0.01)


out<-princomp(featuresRaw)
features <- out$scores[,1:9]

t1 <- tuneRF(features,labels,mtryStart=6, ntreeTry=50, stepFactor=1.2, improve=0.01)
# mtry = 6  OOB error = 15.46% 
# Searching left ...
# mtry = 5   OOB error = 15.57% 
# -0.007116056 0.01 
# Searching right ...
# mtry = 7 	OOB error = 15.55% 
# -0.006080994 0.01 
t2 <- tuneRF(features,labels,mtryStart=6, ntreeTry=500, stepFactor=1.2, improve=0.01)
# mtry = 6  OOB error = 14.29% 
# Searching left ...
# mtry = 5   OOB error = 14.44% 
# -0.01105978 0.01 
# Searching right ...
# mtry = 7 	OOB error = 14.18% 
# 0.007139857 0.01

features <- out$scores[,1:15]
t1 <- tuneRF(features,labels,mtryStart=10, ntreeTry=10, stepFactor=1.2, improve=0.01)
# mtry = 10  OOB error = 18.28% 
# Searching left ...
# mtry = 9   OOB error = 18.36% 
# -0.004145388 0.01 
# Searching right ...
# mtry = 12 	OOB error = 18.46% 
# -0.009461187 0.01 
t2 <- tuneRF(features,labels,mtryStart=10, ntreeTry=50, stepFactor=1.2, improve=0.01)
# mtry = 10  OOB error = 12.86% 
# Searching left ...
# mtry = 9   OOB error = 12.73% 
# 0.009800871 0.01 
# Searching right ...
# mtry = 12 	OOB error = 12.85% 
# 0.0001555694 0.01 
t3 <- tuneRF(features,labels,mtryStart=9, ntreeTry=100, stepFactor=1.2, improve=0.01)
# mtry = 9  OOB error = 12.1% 
# Searching left ...
# mtry = 8   OOB error = 12.05% 
# 0.003637566 0.01 
# Searching right ...
# mtry = 10 	OOB error = 11.95% 
# 0.01223545 0.01 
# mtry = 12 	OOB error = 12.15% 
# -0.01657181 0.01 
t4 <- tuneRF(features,labels,mtryStart=10, ntreeTry=1000, stepFactor=1.2, improve=0.01)
# mtry = 10  OOB error = 11.52% 
# Searching left ...
# mtry = 9   OOB error = 11.58% 
# -0.004860267 0.01 
# Searching right ...
# mtry = 12 	OOB error = 11.67% 
# -0.01301857 0.01 

features <- out$scores[,1:25]
t1 <- tuneRF(features,labels,mtryStart=10, ntreeTry=50, stepFactor=1.2, improve=0.01)
# mtry = 10  OOB error = 12.15% 
# Searching left ...
# mtry = 9   OOB error = 12.41% 
# -0.02139213 0.01 
# Searching right ...
# mtry = 12 	OOB error = 12.15% 
# 0 0.01 

features <- out$scores
t1 <- tuneRF(features,labels,mtryStart=10, ntreeTry=50, stepFactor=1.2, improve=0.01)
# mtry = 10  OOB error = 12.15% 
# Searching left ...
# mtry = 9   OOB error = 12.28% 
# -0.0108642 0.01 
# Searching right ...
# mtry = 12 	OOB error = 11.86% 
# 0.02353909 0.01 
# mtry = 14 	OOB error = 11.9% 
# -0.003202967 0.01 


features <- out$scores[,1:51]
t1 <- tuneRF(features,labels,mtryStart=6, ntreeTry=50, stepFactor=1.2, improve=0.01)
# mtry = 6  OOB error = 12.91% 
# Searching left ...
# mtry = 5   OOB error = 12.95% 
# -0.003099334 0.01 
# Searching right ...
# mtry = 7 	OOB error = 12.64% 
# 0.0204556 0.01 
# mtry = 8 	OOB error = 12.35% 
# 0.02325581 0.01 
# mtry = 9 	OOB error = 12.55% 
# -0.01652089 0.01 

t2 <- tuneRF(features,labels,mtryStart=30, ntreeTry=50, stepFactor=1.2, improve=0.01)
# mtry = 30  OOB error = 11.88% 
# Searching left ...
# mtry = 25   OOB error = 12.09% 
# -0.01801954 0.01 
# Searching right ...
# mtry = 36 	OOB error = 12.16% 
# -0.02391378 0.01 

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