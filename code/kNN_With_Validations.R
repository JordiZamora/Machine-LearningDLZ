# -----------------------------------------------------------
# L_n distance calculation and sorting
# -----------------------------------------------------------
#' Sorted L_n distances
#'
#' Calculate and sort the distance between all pairs of data points.
#' @param TrainData A data frame with n columns of features.
#' @param TestData A data frame with the same number of columns of TrainData. If TestData is not provided, SortDist calculates the distance between all pairs of points in TrainData.
#' @param p Order of the L_p distance metric to be used.
#' @return A matrix with indexes of the neighbours sorted by distance. Each row corresponds to a data point in TestData (or TrainData if TestData is not provided). Each column corresponds to the kth nearest neighbour in TestData.
#' @export
#' @import assertthat 
#' @examples
#' Data <- mtcars[,c("mpg", "wt", "cyl")]
#' nTrain <- 25
#' p <- 2
#' TrainData <- Data[1:nTrain, c("mpg", "wt")]
#' TestData <- Data[(nTrain+1):nrow(mtcars), c("mpg", "wt")]
#' #Calculate the distance between elements in the Training Set
#' TrainSortedDist <- SortDist(TrainData, p=2)
#' # or Calculate distante between each element o the TestData and the training set
#' TestSortedDist <- SortDist(TrainData,TestData,2)


SortDist <- function (TrainData,TestData,p=2, cores=2){
  
  # test the inputs
  if (!require("assertthat")) install.packages("assertthat")
  if (!require("foreach")) install.packages("foreach")
  if (!require("doSNOW")) install.packages("doSNOW")
  not_empty(TrainData);
  not_empty(TestData);
  assert_that(p %in% c(1, 2, Inf))  
  
  probe <- TestData

  #Split probe for parallel computing
  nbreaks <- ceiling(nrow(probe)/cores)
  factorsList <- c(rep(1:(cores-1),each=nbreaks), 
                   rep(cores, nrow(probe) - (cores-1)*nbreaks))
  probeSplit <- split(probe, factorsList )

  cl <- makeCluster(cores, type="SOCK", outfile="") 
  registerDoSNOW(cl)  

  results<- foreach(probe = probeSplit, .combine=rbind) %dopar% {  

  noProbe <- nrow(probe)  
  noObs <- nrow(TrainData)
  Lpdist <- matrix(0,noProbe,noObs)
  
  # Compute the distances from probe data to train data under Lp metric
  if (p!=Inf){
    for (column in 1:ncol(TrainData)){
      Lpdist <- Lpdist + outer(probe[,column], 
                               TrainData[,column],
                               function(a,b) abs(a-b)^p)
    }
    Lpdist <- (Lpdist)^(1/p)
  } else {
    for (column in 1:ncol(TrainData)){
      Lpdist <- pmax(Lpdist, outer(probe[,column], 
                                   TrainData[,column],
                                   function(a,b) abs(a-b)))
    }
  }
  
  # Compute the indexes of the NN to the train data and sort them 
  LpSortIndex <- t(apply(Lpdist,1,order))
}
stopCluster(cl)

  return(results)
}

# -----------------------------------------------------------
# kNN classifier from a matrix of sorted distances
# -----------------------------------------------------------
#' kNN classifier
#'
#' Classify the input with a k nearest neighbors classifier.
#' 
#' @param TrainData A data frame with n columns of features.
#' @param trainclass A vector of classes corresponding to the data in TrainData.
#' @param LpSortIndex A matrix with indexes of the neighbours sorted by distance. Each row corresponds to a data point in testing set. Each column must correspond to the kth nearest neighbour in the testing set.
#' @param TestDataClass A vector of classes corresponding to the data in the test set.
#' @param k An array of numeric values corresponding to the order of the nearest neighbour classifier.
#' @return Returns a list with the predicted class and/or accuracy according to the matrix of indexes LpSortIndex. If TestDataclass is not provided only the predicted class is returned.
#' @export
#' @import assertthat 
#' @examples
#' Data <- mtcars[,c("mpg", "wt", "cyl")]
#' nTrain <- 25
#' p <- 2
#' k <- 1
#' TrainData <- Data[1:nTrain, c("mpg", "wt")]
#' TrainDataClass <- Data[1:nTrain, "cyl"]
#' TestData <- Data[(nTrain+1):nrow(mtcars), c("mpg", "wt")]
#' TestDataClass <- Data[(nTrain+1):nrow(mtcars), "cyl"]
#' # Calculate distante between each element o the TestData and the training set
#' TestSortedDist <- SortDist(TrainData,TestData,2)
#' kNN.out <- kNNClass(TrainData,
#'                     TrainDataClass,
#'                     TestSortedDist,
#'                     TestDataClass, k=k)

kNNClass <- function(TrainData, trainclass, LpSortIndex, TestDataClass=NULL, k=1){
  
  # test the inputs
  if (!require("assertthat")) install.packages("assertthat")
  not_empty(TrainData); not_empty(trainclass); not_empty(LpSortIndex);
  assert_that(is.numeric(k))  
  #assert_that(nrow(TrainData)==length(trainclass))  
  
  # If the label of the test data is not known compute a single value of k
  if (is.null(TestDataClass) && length(k)>1){
    k <- k[1]
  }
  
  accuracy <- rep(0,1,length(k))
  unicLabel <- unique(trainclass)
  
  if (is.null(TestDataClass)){
    kNNs <- as.data.frame(LpSortIndex[,1:k]) #Choose the k-NN indexes
    classes <- apply(kNNs, 2, function(x) trainclass[x]) #Find the class of the NN
    if(is.null(nrow(classes))){
      nrowAux <- length(classes)
    } else {
      nrowAux <- nrow(classes)
    }
    counts <- matrix(0,nrowAux,length(unicLabel))
    
    for (j in 1:length(unicLabel)){
      counts[,j] <- rowSums(classes==unicLabel[j]) #Count each class
    }
    
    predictedClasses <- apply(counts,1, function(x) which.max(x)) #Select more frequent class
    predictedClasses <- unicLabel[predictedClasses] #Substitute class value
    out <- list(PredictedClasses = predictedClasses)
    
  } else {
    for (i in 1:length(k)){
      kNNs <- as.data.frame(LpSortIndex[,1:k[i]])
      classes <- apply(kNNs, 2, function(x) trainclass[x])
      if(is.null(nrow(classes))){
        nrowAux <- length(classes)
      } else {
        nrowAux <- nrow(classes)
      }
      counts <- matrix(0,nrowAux,length(unicLabel))
      
      if(is.null(nrow(classes))){
        for (j in 1:length(unicLabel)){
          counts[,j] <- classes==unicLabel[j]
        }            
      } else {
        for (j in 1:length(unicLabel)){
          counts[,j] <- rowSums(classes==unicLabel[j])
        }        
      }
      
      predictedClasses <- apply(counts,1, function(x) which.max(x))
      predictedClasses <- unicLabel[predictedClasses]
      # Compute accuracy
      accuracy[i] <- mean(predictedClasses==TestDataClass)
    }
    
    if (length(k)>1){
      out <- list(accuracy = accuracy)
    } else {
      out <- list(PredictedClasses = predictedClasses, accuracy = accuracy)
    }
  }
  
  return(out)
}
###----------------------------------------------------------------------




library(ggplot2)#install.packages("ggplot2")




trainData <- read.csv("training.csv") #Train data

N <- nrow(trainData)
SampleData <- trainData[,2:ncol(trainData)]
SampleClass <- trainData[,1] #Train data class


####Calculate the distances once for all methods

k <- seq(1,23,1) #k-NN to be computed
p <- 2 # Manhattan (1), Euclidean (2) or Chebyshev (Inf)

#Obtain the index of all nearest neighbours for the training data
LpSortIndex <- SortDist(SampleData,SampleData, p=p,cores=4)
LpSortIndex2 <- LpSortIndex[,-1]

################-------------------------------------#########
###----------------- Resubstitution
#####----------------------------------------------#######
TestClass <- SampleClass
kNN.out_Resubst <- kNNClass(SampleData,
                            SampleClass,
                            LpSortIndex,
                            SampleClass, k)
kopt_Resubstit <- k[which.min(1-kNN.out_Resubst$accuracy)]
MisclassErrTest_Resubst <- data.frame(k, MisclassError=1-kNN.out_Resubst$accuracy)

################-------------------------------------#########
###--------------------  Leave-One-Out
#####----------------------------------------------#######
sumOfMisclass <- rep(0,length(k))

SampleData_LOO <- SampleData
SampleClass_LOO <- SampleClass

for (i in 1:N){
TestClass <- SampleClass[i]
LpSortIndex_LOO <- rbind(LpSortIndex2[i,])
kNN.outAux <- kNNClass(SampleData_LOO,
                       SampleClass_LOO,
                       LpSortIndex_LOO,
                       TestClass, k)
sumOfMisclass <- sumOfMisclass + kNN.outAux$accuracy
}
kNN.out_LOO <- sumOfMisclass/N

kopt_LOO <- k[which.min(1-kNN.out_LOO)]
MisclassErrTest_LOO <- data.frame(k,MisclassError=1-kNN.out_LOO)

################-------------------------------------#########
###--------------- k-fold Cross Validation
###------------------- Monte Carlo sampling
#####----------------------------------------------#######
#Note that I could do in a short time 100 MC rounds with a single core. 
#With a bit more effort I could improve this substantially by parallelizing the code
N_MC <- 100 #Number of MC iterations
pick <- 599 #DataPoints to leave out for testing

sumOfMisclass <- rep(0,length(k))
listToChoose <- seq(1,nrow(SampleData))

for (i in 1:N_MC){
  #Define random sample to use as test
  seq_ItoF <- sample(listToChoose,pick)
  #Subset the data
  SampleData_CVMC <- SampleData[-seq_ItoF,]
  SampleClass_CVMC <- SampleClass
  TestClass <- SampleClass[seq_ItoF]
  #remove the train points from the distances matrix
  #This subsetting is a mess I know, but I run out of time
  LpSortIndex_CVMC <- LpSortIndex2[seq_ItoF,]
  
  LpSortIndex_CVMCAux <- matrix(0,nrow(LpSortIndex_CVMC),
                                    ncol(LpSortIndex_CVMC)-length(seq_ItoF)+1)
  for (j in 1:nrow(LpSortIndex_CVMC)){
    LpSortIndex_CVMCAux[j,] <- LpSortIndex_CVMC[j,!(LpSortIndex_CVMC[j,] %in% seq_ItoF) ]
  }
  
  kNN.outAux <- kNNClass(SampleData_CVMC,
                         SampleClass_CVMC,
                         LpSortIndex_CVMC,
                         TestClass, k)
  sumOfMisclass <- sumOfMisclass + kNN.outAux$accuracy
}
kNN.out_CVMC <- sumOfMisclass/N_MC

kopt_CVMC <- k[which.min(1-kNN.out_CVMC)]
MisclassErrTest_CVMC <- data.frame(k,MisclassError=1-kNN.out_CVMC)


################-------------------------------------#########
###--------------- k-fold Cross Validation
###------------------- Blocks sampling
#####----------------------------------------------#######
blocks <- 10 #Number of blocks

sumOfMisclass <- rep(0,length(k))

for (i in 1:blocks){
  #Define starting and ending point of the block
  initDeletData <- floor(nrow(SampleData)/blocks)*(i-1)+1
  finalDeletData <- floor(nrow(SampleData)/blocks)*i
  seq_ItoF <- seq(initDeletData,finalDeletData)
  #Subset the data
  SampleData_CVBlocks <- SampleData[-seq_ItoF,]
  SampleClass_CVBlocks <- SampleClass
  TestClass <- SampleClass[seq_ItoF]
  #remove the train points from the distances matrix
  #This subsetting is a mess I know, but I run out of time
  LpSortIndex_CVBlocks <- LpSortIndex2[seq_ItoF,]
  LpSortIndex_CVBlocksAux <- matrix(0,nrow(LpSortIndex_CVBlocks),
                                    ncol(LpSortIndex_CVBlocks)-length(seq_ItoF)+1)
  for (j in 1:nrow(LpSortIndex_CVBlocks)){
    LpSortIndex_CVBlocksAux[j,] <- LpSortIndex_CVBlocks[j,!(LpSortIndex_CVBlocks[j,] %in% seq_ItoF) ]
  }
  
  kNN.outAux <- kNNClass(SampleData_CVBlocks,
                         SampleClass_CVBlocks,
                         LpSortIndex_CVBlocksAux,
                         TestClass, k)
  sumOfMisclass <- sumOfMisclass + kNN.outAux$accuracy
}
kNN.out_CVBlocks <- sumOfMisclass/blocks

kopt_CVBlocks <- k[which.min(1-kNN.out_CVBlocks)]
MisclassErrTest_CVBlocks <- data.frame(k,MisclassError=1-kNN.out_CVBlocks)

AllInOne <- rbind(cbind(MisclassErrTest_Resubst, 
                        Method= rep("Resubstitution",nrow(MisclassErrTest_Resubst))),
                  cbind(MisclassErrTest_LOO, 
                        Method= rep("Leave-one-out",nrow(MisclassErrTest_LOO))),
                  cbind(MisclassErrTest_CVMC, 
                        Method= rep("CV MC",nrow(MisclassErrTest_CVMC))),
                  cbind(MisclassErrTest_CVBlocks, 
                        Method= rep("CV Blocks",nrow(MisclassErrTest_CVBlocks))))

#Plot the misclassification errors
ggplot(data=AllInOne, aes(x=k, y=MisclassError, colour=Method)) +
  geom_line() +
  ylab("Misclassification Error")

#Load the test Sample with unknown labels
TestData <- read.csv("test.csv") #Test data
#Predict labels for the data
LpSortIndex_Test <- SortDist(SampleData,TestData,p=p,cores=4)
kNN.out_Test <- kNNClass(SampleData, SampleClass, LpSortIndex_Test, k=1)

#Save labels predicted
write.csv(kNN.out_Test, file = "PredictedDigits_JZM.csv",row.names=FALSE)
