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
#comment on plot as it indicates correlation with variables
##### 
#not sure how for implying on the model development
#install.packages("tree")
library(tree)
tree.mod1<-tree(ratio~.,data.primary)
plot(tree.mod1)
text(tree.mod1)
#split the aveTemp in 11.25
#should we use this for our first models???
#try
pwl<-function(x,x0){
  return( (x > x0) * (x-x0) )
}

#####
#try to figure out if higher order term should be added
#install.packages("mgcv")
library(mgcv)
#is the dependency of the parameters on each other the reason for no interactions taking into 
#consideration?
gam1<-gam(ratio~s(aveTemp)+s(relHum)+s(sunHours)+s(precip),data=data.primary)
summary(gam1)
#all s(<terms>) significant - is that important?
par(mfrow=c(2,2))
plot(gam1)
#yields following considerations:
#aveTemp - quadratic or linear
#relHum - linear
#sunHours - linear, quadratic or nonlinear
#precip - nonlinear or linear

#eliminate the na rows - is that right to do?
data.primary <- data.primary[complete.cases(data.primary),]
#make a raw lm 
lm1 <- lm(ratio~aveTemp+relHum+sunHours+precip,data=data.primary)
summary(lm1)
anova(lm1)
#sunHours not significant - is that of interest here?
plot(lm1)
#clear lack of higher order terms or transformation
par(mfrow=c(2,2))
for ( i in  c("aveTemp","relHum","sunHours","precip")){
  plot(data.primary[[i]],residuals(lm1),type="n",xlab=i)
  panel.smooth(data.primary[[i]],residuals(lm1))
}
#residuals do not look good as expected try a new model - add square terms
lm2 <- lm(ratio~aveTemp+relHum+sunHours+precip+I(aveTemp^2)+I(sunHours^2),data.primary)
plot(lm2)
#looks better than before
anova(lm2)
#sunHours still not significant- but the squared term is
par(mfrow=c(2,2))
for ( i in  c("aveTemp","relHum","sunHours","precip")){
  plot(data.primary[[i]],residuals(lm2),type="n",xlab=i)
  panel.smooth(data.primary[[i]],residuals(lm2))
}
#try third model
lm3 <- lm(ratio~aveTemp+relHum+sunHours+precip+I(aveTemp^2)+I(sunHours^2)+I(precip^2),data.primary)
plot(lm3)
#looks similar as before
anova(lm3)
#sunHours still not significant as well as the sqared precip term
par(mfrow=c(2,2))
for ( i in  c("aveTemp","relHum","sunHours","precip")){
  plot(data.primary[[i]],residuals(lm3),type="n",xlab=i)
  panel.smooth(data.primary[[i]],residuals(lm3))
}

#remove the insignificant term which is the same as lm3

#take sqrt of the ratio now
lm5 <- lm(sqrt(ratio)~aveTemp+relHum+sunHours+precip+I(aveTemp^2)+I(sunHours^2),data.primary)
plot(lm5)
#looks similar as before - makes little difference
anova(lm5)

#take log of the ratio now
lm6 <- lm(log(ratio+1)~aveTemp+relHum+sunHours+precip+I(aveTemp^2)+I(sunHours^2),data.primary)
#is it ok to add 1 to in the log transformation
plot(lm6)
#looks similar as before - makes little difference
anova(lm6)


######
#try to split for a new model - use pwl




