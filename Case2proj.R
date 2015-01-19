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
library(tree)
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






#### Part2 - Analyzis of position

data$ratioR1 = (data$R1pos/data$R1total)
data$ratioR2 = (data$R2pos/data$R2total)
data$ratioR3 = (data$R3pos/data$R3total)
data$ratioR4 = (data$R4pos/data$R4total)
data$ratioR5 = (data$R5pos/data$R5total)
data$ratioR6 = (data$R6pos/data$R6total)
data$ratioR7 = (data$R7pos/data$R7total)
data$ratioR8 = (data$R8pos/data$R8total)



