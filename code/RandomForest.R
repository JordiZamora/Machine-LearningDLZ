
#-------------------------------------------------------------------------
if (!require("compiler")) install.packages("compiler")
if (!require("foreach")) install.packages("foreach")
if (!require("doSNOW")) install.packages("doSNOW")
if (!require("randomForest")) install.packages("randomForest")

setwd("~/Documents/Cursos/DataScience/2ndTerm/HomeworkMachineLearning/Project/code")

#Load data
rawdata <- read.csv("../data/Kaggle_Covertype_training.csv")
numcol <- ncol(rawdata)
numrow <- nrow(rawdata)

#Classes as factors
rawdata[,numcol] <- factor(rawdata[,numcol])
classes <- sort(unique(rawdata[,numcol]))
dim(rawdata)
summary(rawdata)

# Sample a train data
TraindataX <- rawdata[,-numcol]
TrainXSimplif <- TraindataX[,-c(22,30,52)]
TraindataY <- rawdata[,numcol]
ntrain <- length(TraindataY)

# we simply pass the function we want to use through cmpfun()
MCXVRandomForest <- cmpfun(MCXVRandomForest)

###Check for consistency at different depths
# out50 <- tuneRF (TrainXSimplif, TraindataY, mtryStart=20, stepFactor=1.2, ntreeTry=50)
# out100 <- tuneRF (TrainXSimplif, TraindataY, mtryStart=33, stepFactor=1.2, ntreeTry=100)
# 
# ntree <- c(200,500,1000,1500)
# ntreeList <- split(ntree, seq(1,4))
# cl <- makeCluster(4, type="SOCK", outfile="") 
# registerDoSNOW(cl)  
# 
# system.time(results<- foreach(ntree = ntreeList, .combine=rbind) %dopar% {
#   if (!require("randomForest")) install.packages("randomForest")
#   outn00 <- tuneRF (TrainXSimplif, TraindataY, mtryStart=33, stepFactor=1.1, ntreeTry=ntree)
#   cbind(outn00,rep(ntree,nrow(outn00)))
# })
# stopCluster(cl)
# mtry = 33  OOB error = 10.61%  ##ntree=200
# Searching left ...
# mtry = 30   OOB error = 10.72% 
# -0.01036953 0.05 
# Searching right ...
# mtry = 33  OOB error = 10.43%   ##ntree=500
# Searching left ...
# mtry = 36 	OOB error = 10.62% 
# -0.001131222 0.05 
# mtry = 30 	OOB error = 10.5% 
# -0.006134969 0.05 
# Searching right ...
# mtry = 33  OOB error = 10.42%   ##ntree=1000
# Searching left ...
# mtry = 36 	OOB error = 10.51% 
# -0.007476994 0.05 
# mtry = 30 	OOB error = 10.36% 
# 0.005568356 0.05 
# Searching right ...
# mtry = 36 	OOB error = 10.39% 
# 0.002880184 0.05 
# mtry = 33  OOB error = 10.39%  ###ntree=15000
# Searching left ...


#Do cross validation in parallel
cl <- makeCluster(4, type="SOCK", outfile="") 
registerDoSNOW(cl)  

mtry <- seq(22,22,2)
mtryList <- split(mtry, seq(1,4))

MCiter <- rep(5,4)
MCiterList <- split(MCiter, seq(1,4))

#DeepTree <- seq(500,2000,500)
#DeepTreeList <- split(DeepTree, seq(1,4))

#results<- foreach(mtry = mtryList, ntree=DeepTreeList, .combine=rbind) %dopar% {
#   out <- data.frame ()
#   for (i in 1:length(mtry)){
#     for (j in 1:length(ntree)){
#       MisclassError <- MCXVRandomForest (TraindataX, 
#                                          TraindataY, 
#                                          MCiter=100, 
#                                          fraction=0.1, 
#                                          mtry=mtry[i],
#                                          ntree=ntree[j])
#       out <- rbind(out, data.frame(mtry=mtry[i],
#                                    ntree=ntree[j], 
#                                    Misclass_Error=MisclassError))
#     }
#   }
#   out
# }

# system.time(results<- foreach(mtry = mtryList, .combine=rbind) %dopar% {
#   out <- data.frame ()
#   for (i in 1:length(mtry)){
#       MisclassError <- MCXVRandomForest (TraindataX, 
#                                          TraindataY, 
#                                          MCiter=20, 
#                                          fraction=0.1, 
#                                          mtry=mtry[i])
#       out <- rbind(out, data.frame(mtry=mtry[i],
#                                    ntree=500, 
#                                    Misclass_Error=MisclassError))
#   }
#   out
# })

system.time(results<- foreach(MCiter = MCiterList, .combine=rbind) %dopar% {
  out <- data.frame ()
  for (i in 1:length(MCiter)){
      MisclassError <- MCXVRandomForest (TraindataX, 
                                         TraindataY, 
                                         MCiter=MCiter, 
                                         fraction=0.1, 
                                         mtry=22)
      out <- rbind(out, data.frame(mtry=mtry[i],
                                   MCiter=MCiter,
                                   ntree=500, 
                                   Misclass_Error=MisclassError))
  }
  out
})
stopCluster(cl)
#Mciter=10, ntree=500,fraction=0.1, N=5000, time~20min
#     mtry Misclass_Error
#1    14         0.1208
#2    18         0.1174
#3    22         0.1078
#4    26         0.1174
#5    15         0.1238
#6    19         0.1260
#7    23         0.1192
#8    27         0.1222
#9    16         0.1192
#10   20         0.1196
#11   24         0.1230
#12   28         0.1182
#13   17         0.1210
#14   21         0.1138
#15   25         0.1090
#16   29         0.1210


