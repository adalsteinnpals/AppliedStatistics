dev.off() 
rm(list=ls())

data <- read.table("case2regionsOnePerBatch.txt",header=T)

#visualization of the data
data$ratio <- (data$pos/data$total)

plot(data$ratio,typ="l")
plot(data$ratio[1:52],typ="l")

names(data)
data.primary <- data[,c("aveTemp","relHum","sunHours","precip","ratio")]

#plot of the correlation between variables
pairs(data.primary,panel=panel.smooth)

#install.packages("tree")
#library(tree)
model1<-tree(ratio~.,data.primary)

plot(model1)
text(model1)
#split the aveTemp in 11.25

#install.packages("mgcv")
library(mgcv)
par(mfrow=c(2,2))
model2<-gam(ratio~s(aveTemp)+s(relHum)+s(sunHours)+s(precip),data=data.primary)
summary(model2)
plot(model2)




lm1 <- lm(ratio~.,data.primary)

anova(lm1)
lm2 <- step(lm1)
