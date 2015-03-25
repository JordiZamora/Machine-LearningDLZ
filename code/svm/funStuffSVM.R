# In this file some results are sumarised in graphs on interesting variations of the models
# tested in svmTry2.R # also it includes support functions

install.packages("ggplot2")
library("ggplot2")
#function for cross validation
crossVal <- function(trueClass, predV) {
  # error for support vector machines
  a <- sum(sign(trueClass)==sign(predV))/length(predV)
  return (1-a)
}

# lets make a plot of how error increases with C 

cVec <- seq(1, 1000, 50)
errVec <- rep(2, length(cVec))
crossValVec <- rep(1, length(cVec))
for (i in 1:length(cVec)) {
  modl <- modelAll1_2 <- ksvm(x=as.matrix(train[, 1:ncol(train)-1]), 
                              y=as.factor(train[, ncol(train)]),
                              C=cVec[i], cross=10)
  errVec[i] <- modl@error
  crossValVec[i] <- modl@cross
}

errDF <- data.frame(cVec, errVec, crossValVec)
colnames(errDF) <- c("C", "InSError", "CrossValErr")

ggplot(data = errDF, aes(x = C)) + 
  geom_line(aes(y=InSError, colour="Error - Insample")) +
  geom_line(aes(y=CrossValErr, colour="Error - 10xCross Val.")) +
  xlab("C-parameter") +
  ylab("Error Rate") +
  theme_bw(base_size = 14, base_family = "Helvetica")+
  ggtitle("C parameter vs errors")