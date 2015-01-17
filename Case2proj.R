dev.off() 
rm(list=ls())

data <- read.table("case2regionsOnePerBatch.txt",header=T)

plot(data$pos[1:52]/data$total[1:52],typ="l")
data$ratio <- (data$pos/data$total)


lm1 <- lm(ratio~.,data)
anova(lm1)
lm2 <- step(lm1)
