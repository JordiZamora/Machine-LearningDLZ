
source('multiplot.R')
library(ggplot2)

#Load data
rawdata <- read.csv("../data/Kaggle_Covertype_training.csv")
numcol <- ncol(rawdata)
numrow <- nrow(rawdata)
rawdata[,numcol] <- factor(rawdata[,numcol]) #Classes as factors

plotList <- list()

plotList[[1]] <- ggplot(data=rawdata,aes(x=elevation)) + 
  geom_histogram() +
  xlab(names(rawdata)[2])

plotList[[2]] <- ggplot(data=rawdata,aes(x=aspect)) + 
  geom_histogram() +
  xlab(names(rawdata)[3])

plotList[[3]] <- ggplot(data=rawdata,aes(x=slope)) + 
  geom_histogram() +
  xlab(names(rawdata)[4])
plotList[[4]] <- ggplot(data=rawdata,aes(x=hor_dist_hyd)) + 
  geom_histogram() +
  xlab(names(rawdata)[5])
plotList[[5]] <- ggplot(data=rawdata,aes(x=ver_dist_hyd)) + 
  geom_histogram() +
  xlab(names(rawdata)[6])
plotList[[6]] <- ggplot(data=rawdata,aes(x=hor_dist_road)) + 
  geom_histogram() +
  xlab(names(rawdata)[7])
plotList[[7]] <- ggplot(data=rawdata,aes(x=hill_9am)) + 
  geom_histogram() +
  xlab(names(rawdata)[8])
plotList[[8]] <- ggplot(data=rawdata,aes(x=hill_noon)) + 
  geom_histogram() +
  xlab(names(rawdata)[9])
plotList[[9]] <- ggplot(data=rawdata,aes(x=hill_3pm)) + 
  geom_histogram() +
  xlab(names(rawdata)[10])
plotList[[10]] <- ggplot(data=rawdata,aes(x=hor_dist_fire)) + 
  geom_histogram() +
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

table (rawdata[,numcol])*100/50000
