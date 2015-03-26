
source('multiplot.R')
library(ggplot2)

#Load data
rawdata <- read.csv("../data/Kaggle_Covertype_training.csv")
numcol <- ncol(rawdata)
numrow <- nrow(rawdata)
rawdata[,numcol] <- factor(rawdata[,numcol]) #Classes as factors

### Plot the density distribution of the continuous variables 
plotList <- list()

plotList[[1]] <- ggplot(data=rawdata,aes(x=elevation,fill=Cover_Type,col=Cover_Type)) + 
  #   geom_bar() +
  geom_density(alpha = 0.2) +
  xlab(names(rawdata)[2]) + 
  theme(legend.position="none")

plotList[[2]] <- ggplot(data=rawdata,aes(x=aspect,fill=Cover_Type,col=Cover_Type)) + 
  #   geom_bar() +
  geom_density(alpha = 0.2) +
  xlab(names(rawdata)[3]) + 
  theme(legend.position="none")

plotList[[3]] <- ggplot(data=rawdata,aes(x=slope,fill=Cover_Type,col=Cover_Type)) + 
  #   geom_bar() +
  geom_density(alpha = 0.2) +
  xlab(names(rawdata)[4]) + 
  theme(legend.position="none")
plotList[[4]] <- ggplot(data=rawdata,aes(x=hor_dist_hyd,fill=Cover_Type,col=Cover_Type)) + 
  #   geom_bar() +
  geom_density(alpha = 0.2) +
  xlab(names(rawdata)[5]) + 
  theme(legend.position="none")
plotList[[5]] <- ggplot(data=rawdata,aes(x=ver_dist_hyd,fill=Cover_Type,col=Cover_Type)) + 
  #   geom_bar() +
  geom_density(alpha = 0.2) +
  xlab(names(rawdata)[6])
plotList[[6]] <- ggplot(data=rawdata,aes(x=hor_dist_road,fill=Cover_Type,col=Cover_Type)) + 
  #   geom_bar() +
  geom_density(alpha = 0.2) +
  xlab(names(rawdata)[7]) + 
  theme(legend.position="none")
plotList[[7]] <- ggplot(data=rawdata,aes(x=hill_9am,fill=Cover_Type,col=Cover_Type)) + 
  #   geom_bar() +
  geom_density(alpha = 0.2) +
  xlab(names(rawdata)[8]) + 
  theme(legend.position="none")
plotList[[8]] <- ggplot(data=rawdata,aes(x=hill_noon,fill=Cover_Type,col=Cover_Type)) + 
  #   geom_bar() +
  geom_density(alpha = 0.2) +
  xlab(names(rawdata)[9]) + 
  theme(legend.position="none")
plotList[[9]] <- ggplot(data=rawdata,aes(x=hill_3pm,fill=Cover_Type,col=Cover_Type)) + 
  #   geom_bar() +
  geom_density(alpha = 0.2) +
  xlab(names(rawdata)[10]) + 
  theme(legend.position="none")
plotList[[10]] <- ggplot(data=rawdata,aes(x=hor_dist_fire,fill=Cover_Type,col=Cover_Type)) + 
  #   geom_bar() +
  geom_density(alpha = 0.2) +
  xlab(names(rawdata)[11])


layout <- matrix(seq(1,10), nrow = 2, byrow = TRUE)
myPlotList <- plotList
multiplot(plotlist = myPlotList, layout = layout)

categorical <- rawdata[,12:55]
summaryCategorical <- data.frame(mean=colMeans(categorical), counts=colSums(categorical))
wilds <- apply(categorical[,1:4],2,function(x) x*as.numeric(rawdata[,56]))
wilds2 <- apply(wilds,2, function(x) table(x))

soils <- apply(categorical[,5:ncol(categorical)],2,function(x) x*as.numeric(rawdata[,56]))
soils2 <- apply(soils,2, function(x) table(x))


summaryCategorical <- cbind(summaryCategorical,
                            Cover1=0,
                            Cover2=0,
                            Cover3=0,
                            Cover4=0,
                            Cover5=0,
                            Cover6=0,
                            Cover7=0)

for (j in 1:length(wilds2)){
  name <- names(wilds2[[j]])[-1]
  for (i in name){
    ii <- as.numeric(i)
    jj <- which(name==i)+1
    summaryCategorical[j,(2+ii)] <- wilds2[[j]][jj]
  }
}


for (j in 1:length(soils2)){
  name <- names(soils2[[j]])[-1]
  for (i in name){
    ii <- as.numeric(i)
    jj <- which(name==i)+1
    summaryCategorical[(length(wilds2)+j),(2+ii)] <- soils2[[j]][jj]
  }
}

#Output the statistics of categorical data
summaryCategorical
#Or in fractions
summaryCategoricalFrac <- summaryCategorical
summaryCategoricalFrac[,3:9] <- round(summaryCategorical[,3:9]*100/summaryCategorical[,2])
summaryCategoricalFrac

#Or in percentage of the total
table (rawdata[,numcol])*100/50000

#Visual inspection of the best problem coordinates
#Principal Component analysis
# aux <- rawdata[,-c(1,30,numcol)]
# #aux[,1:11] <- scale(aux[,1:11])
# aux <- scale(aux)
# range01 <- function(x){(x-min(x))/(max(x)-min(x))}
# aux[,1:11]<- apply(aux[,1:11],2,range01)
# 
# out<-princomp(aux[,1:ncol(aux)])
# PCAFrame <- as.data.frame(cbind(out$scores, Cov_Type=rawdata[,numcol]))
# PCAFrame[,ncol(PCAFrame)] <- factor(PCAFrame[,ncol(PCAFrame)])
# 

#SVD
aux <- rawdata[,-c(1,30,numcol)]
out<-svd(aux[,1:ncol(aux)])
us <- as.matrix(out$u[, 1:depth])
vs <- as.matrix(out$v[, 1:depth])
ds <- as.matrix(diag(out$d)[1:depth, 1:depth])
Xreduced <- us %*% ds
SVDFrame <- as.data.frame(cbind(Xreduced, Cov_Type=rawdata[,numcol]))
SVDFrame[,ncol(SVDFrame)] <- factor(SVDFrame[,ncol(SVDFrame)])

#Plot of the first 2 components
ggplot()+
  geom_point(data=SVDFrame, aes(x=V1, y=V2, fill=Cov_Type, col=Cov_Type), alpha=0.5)
