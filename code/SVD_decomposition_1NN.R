library(class)#install.packages("class")

#Load data
rawdata <- read.csv("../data/Kaggle_Covertype_training.csv")
numcol <- ncol(rawdata)
numrow <- nrow(rawdata)
rawdata[,numcol] <- factor(rawdata[,numcol]) #Classes as factors

aux <- rawdata[,-c(1,30,numcol)]
#aux[,1:11] <- scale(aux[,1:11])
# aux <- scale(aux)
# range01 <- function(x){(x-min(x))/(max(x)-min(x))}
# aux[,1:11]<- apply(aux[,1:11],2,range01)

#out<-princomp(aux[,1:ncol(aux)])
# PCAFrame <- as.data.frame(cbind(out$scores, Cov_Type=rawdata[,numcol]))
# PCAFrame[,ncol(PCAFrame)] <- factor(PCAFrame[,ncol(PCAFrame)])

depthv <- c()
MisclassErr <-  c()
out<-svd(aux[,1:ncol(aux)])
for (i in 2:40){
depth <- i
depthv[i] <- depth
us <- as.matrix(out$u[, 1:depth])
vs <- as.matrix(out$v[, 1:depth])
ds <- as.matrix(diag(out$d)[1:depth, 1:depth])
Xreduced <- us %*% ds
SVDFrame <- as.data.frame(cbind(Xreduced, Cov_Type=rawdata[,numcol]))
SVDFrame[,ncol(SVDFrame)] <- factor(SVDFrame[,ncol(SVDFrame)])

training <- SVDFrame

noObs<-nrow(training)
noBuckets <- 10
idx <- rep(1:noBuckets, each=ceiling(nrow(training)/noBuckets)) 
idx <- idx[sample(1:noObs)]


training <- data.frame(training,bucket=idx) 
head(training)
ks <- 1

results<-data.frame(1,1,1)
testError<-data.frame(1,1)

cv<-1
k<-1

names(training)
traincol <- ncol(training)
Xtrain <- training[training$bucket != cv, -c(traincol-1, traincol)]
head(Xtrain)
dim(Xtrain)

Ytrain <- training[training$bucket != cv,  (traincol-1)]

# subsetting the test phase data
Xtest <- training[training$bucket == cv, -c(traincol-1, traincol)]

Ytest <- training[training$bucket == cv,  (traincol-1)]



# kNN results
info<-paste(as.character(cv),as.character(k))
testPredictions <- knn(train=Xtrain, test=Xtest, cl=Ytrain, k=3)
MisclassErr[i] <- mean (testPredictions != Ytest)
}
#1knn
#Misclass=
# [1]     NA 0.5320 0.2604 0.1584 0.1162 0.1064 0.0988 0.1022 0.0978 0.1078 0.1030 0.1110 0.0970 0.1018 0.1066 0.0962 0.1010 0.0952 0.1094
# [20] 0.1012 0.1050 0.1038 0.0994 0.1090 0.0996 0.1010 0.0982 0.1038 0.1014 0.1048 0.1068 0.0992 0.1008 0.1038 0.1094 0.0934 0.0956 0.1026
# [39] 0.1012 0.1072

#3knn
#Misclass=
# [1]     NA 0.4878 0.2496 0.1734 0.1458 0.1360 0.1224 0.1162 0.1220 0.1178 0.1232 0.1236 0.1240 0.1156 0.1178 0.1188 0.1116 0.1202 0.1292
# [20] 0.1202 0.1210 0.1180 0.1216 0.1270 0.1188 0.1262 0.1178 0.1188 0.1212 0.1204 0.1206 0.1278 0.1236 0.1212 0.1224 0.1226 0.1220 0.1146
# [39] 0.1178 0.1184

plot(depthv, MisclassErr)
