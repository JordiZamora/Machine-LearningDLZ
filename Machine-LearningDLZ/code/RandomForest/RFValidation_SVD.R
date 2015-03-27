if (!require("foreach")) install.packages("foreach")
if (!require("doSNOW")) install.packages("doSNOW")
if (!require("randomForest")) install.packages("randomForest")
library(foreach)
library(doSNOW)
library(randomForest)
setwd("~/ProjectAWS/code/randomForest")

#source("RF_XValidation.R")
source("RF_XValidationBuckets.R")


#Load data
rawdata <- read.csv("../../data/Kaggle_Covertype_training.csv")
numcol <- ncol(rawdata)
numrow <- nrow(rawdata)

#Dimension reduction
depth <- 6
aux <- rawdata[,-c(1,30,numcol)]
out<-svd(aux[,1:ncol(aux)])
us <- as.matrix(out$u[, 1:depth])
vs <- as.matrix(out$v[, 1:depth])
ds <- as.matrix(diag(out$d)[1:depth, 1:depth])
Xreduced <- us %*% ds
SVDFrame <- as.data.frame(cbind(Xreduced, Cov_Type=rawdata[,numcol]))
SVDFrame[,ncol(SVDFrame)] <- factor(SVDFrame[,ncol(SVDFrame)])

numcol2 <- ncol(SVDFrame)
numrow2 <- nrow(SVDFrame)

features <- SVDFrame[,-numcol2]
labels <- factor(SVDFrame[,numcol2])

t1 <- tuneRF(features,labels, mtryStart=4, ntreeTry=100, stepFactor=2, improve=0.01)



MError <- data.frame()

cores <- 8

for (i in 1:10){
  mtry <- 34 + i
  
  for (j in 1:11){
    
    cat(j,"\n")
    # Set Parameters
    #MCiter <- 4
    nBuckets <- cores
    fraction <- 0.1
    ntree <- round(10+190*(j-1)/10)
    
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
    
    MError[j+(i-1)*cores,1] <- mtry
    MError[j+(i-1)*cores,2] <- ntree
    MError[j+(i-1)*cores,3] <- colSums(results)
    names(MError) <- c("mtry", "ntree", "Misclass")
  }
}
#write.csv(MError, file = "Result_Xval_AllFeatures_Systematic.csv", row.names=FALSE)
MErrorOut <- read.csv("Result_Xval_AllFeatures_Systematic.csv")
MErrorOut2 <- read.csv("Result_Xval_AllFeatures_SystematicB.csv")
MErrorOuts <- rbind(MErrorOut,MErrorOut2)
MErrorOuts[,"mtry"] <- factor(MErrorOuts[,"mtry"])
ggplot(data=MErrorOuts, aes(x=ntree,y=Misclass, group=mtry, col=mtry)) + 
  geom_line() + 
  xlim(c(0,1000)) + 
  ylim(c(0.104,0.1125))
#For j From 1:11
#MError=
# mtry ntree Misclass
# 1    35    10  0.12686
# 2    35    29  0.11048
# 3    35    48  0.10836
# 4    35    67  0.10838
# 5    35    86  0.10772
# 6    35   105  0.10604
# 7    35   124  0.10562
# 8    35   143  0.10612
# 9    36    10  0.12620
# 10   36    29  0.11228
# 11   36    48  0.10864
# 12   36    67  0.10932
# 13   36    86  0.10702
# 14   36   105  0.10616
# 15   36   124  0.10640
# 16   36   143  0.10652
# 17   37    10  0.12586
# 18   37    29  0.11002
# 19   37    48  0.10848
# 20   37    67  0.10796
# 21   37    86  0.10760
# 22   37   105  0.10630
# 23   37   124  0.10544
# 24   37   143  0.10626
# 25   38    10  0.12460
# 26   38    29  0.11216
# 27   38    48  0.10786
# 28   38    67  0.10820
# 29   38    86  0.10650
# 30   38   105  0.10612
# 31   38   124  0.10686
# 32   38   143  0.10572
# 33   39    10  0.12748
# 34   39    29  0.11168
# 35   39    48  0.10884
# 36   39    67  0.10854
# 37   39    86  0.10738
# 38   39   105  0.10686
# 39   39   124  0.10666
# 40   39   143  0.10542
# 41   40    10  0.12512
# 42   40    29  0.11194
# 43   40    48  0.10896
# 44   40    67  0.10722
# 45   40    86  0.10764
# 46   40   105  0.10652
# 47   40   124  0.10666
# 48   40   143  0.10550
# 49   41    10  0.12680
# 50   41    29  0.11146
# 51   41    48  0.10834
# 52   41    67  0.10778
# 53   41    86  0.10642
# 54   41   105  0.10698
# 55   41   124  0.10720
# 56   41   143  0.10658
# 57   42    10  0.12676
# 58   42    29  0.11174
# 59   42    48  0.10918
# 60   42    67  0.10804
# 61   42    86  0.10776
# 62   42   105  0.10686
# 63   42   124  0.10664
# 64   42   143  0.10654
# 65   43    10  0.12354
# 66   43    29  0.11180
# 67   43    48  0.10898
# 68   43    67  0.10734
# 69   43    86  0.10704
# 70   43   105  0.10628
# 71   43   124  0.10648
# 72   43   143  0.10680
# 73   44    10  0.12538
# 74   44    29  0.11118
# 75   44    48  0.10974
# 76   44    67  0.10826
# 77   44    86  0.10678
# 78   44   105  0.10792
# 79   44   124  0.10634
# 80   44   143  0.10714
# 81   44   162  0.10630
# 82   44   181  0.10698
# 83   44   200  0.10602