
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
  geom_line(aes(y=InSError, colour="red")) +
  geom_line(aes(y=CrossValErr, colour="blue")) +
  xlab("Height") +
  ylab("Weight") +
  theme_bw(base_size = 14, base_family = "Helvetica")