MCXVRandomForest <- function(x, y, MCiter=100, fraction=0.1, mtry=NULL, ntree=500){
  ################-------------------------------------#########
  ###--------------- k-fold Cross Validation (XV)
  ###------------------- Monte Carlo sampling
  ###--------------- for Random Forest classifier
  #####----------------------------------------------#######
  #x: Matrix or data frame containing the predictive variables
  #y: Vector of predicted variables as factors
  #MCiter: Number of Monte Carlo iterations
  #fraction: Fraction of data left out on each iteration
  #mtry: Number of factors used on each Random Forest iteration. If mtry is not specified it will use sqrt(ncol(x)).

  if (!require("compiler")) install.packages("compiler")
  if (!require("randomForest")) install.packages("randomForest")
  
  if(is.null(mtry)) mtry <- ceiling(sqrt(ncol(x)))
  ndata <- length(y)
  pick <- round(ndata*fraction) #DataPoints to leave out for testing
    
  sumOfMisclass <- 0
  listToChoose <- seq(1,length(y))
  randomForest <- cmpfun(randomForest)
  
  for (i in 1:MCiter){
    #Define random sample to use as test
    seq_ItoF <- sample(listToChoose,pick)
    #Subset the data
    InSampleX <- x[-seq_ItoF,]
    InSampleY <- y[-seq_ItoF]
    OutSampleX <- x[seq_ItoF,]
    OutSampleY <- y[seq_ItoF]

    #Run random forest
    randomTest <- randomForest(InSampleX,
                               InSampleY,
                               OutSampleX,
                               OutSampleY, 
                               mtry=mtry,
                               ntree=ntree)
    #Extract total number of misclassified data
    ncolResult <- ncol(randomTest$test$confusion)
    MisclassError <- sum(rowSums(randomTest$test$confusion[,-ncolResult])*
      randomTest$test$confusion[,ncolResult])
  
    sumOfMisclass <- sumOfMisclass + MisclassError
  }
  MisclassErrorMean <- sumOfMisclass/(MCiter*pick)
  return(MisclassErrorMean)
}
#-------------------------------------------------------------------------
if (!require("compiler")) install.packages("compiler")
if (!require("foreach")) install.packages("foreach")
if (!require("doSNOW")) install.packages("doSNOW")
if (!require("randomForest")) install.packages("randomForest")

rawdata <- read.csv("../data/covtype.data")
numcol <- ncol(rawdata)
numrow <- nrow(rawdata)
rawdata[,numcol] <- factor(rawdata[,numcol])
classes <- sort(unique(rawdata[,numcol]))
dim(rawdata)
summary(rawdata)

# Sample a train data
TraindataX <- rawdata[1:5000,-numcol]
TraindataY <- rawdata[1:5000,numcol]
ntrain <- length(TraindataY)

# we simply pass the function we want to use through cmpfun()
MCXVRandomForest <- cmpfun(MCXVRandomForest)

#Do cross validation in parallel
cl <- makeCluster(4, type="SOCK", outfile="") 
registerDoSNOW(cl)  

mtry <- seq(14,28,2)
mtryList <- split(mtry, seq(1,4))

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

system.time(results<- foreach(mtry = mtryList, .combine=rbind) %dopar% {
  out <- data.frame ()
  for (i in 1:length(mtry)){
      MisclassError <- MCXVRandomForest (TraindataX, 
                                         TraindataY, 
                                         MCiter=20, 
                                         fraction=0.1, 
                                         mtry=mtry[i])
      out <- rbind(out, data.frame(mtry=mtry[i],
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

#feature reduction: 
sum(rowSums(rawdata[,15:54])>1)

subset1 <- rawdata[rawdata[,51]==1,]
summary(subset1)
#col 21 only type 2 trees
#col 29 only type 6 trees
#col 51 only type 7 trees

feature <- data.frame()
for (i in 15:54){
  feature <- rbind(feature, summary(rawdata[rawdata[,i]==1,numcol]))
}
feature
feature[feature[,3]==0 & feature[,4]==0  & feature[,5]==0 & feature[,6]==0 & feature[,7]==0,]
feature[feature[,1]==0 & feature[,2]==0 & feature[,7]==0,]
