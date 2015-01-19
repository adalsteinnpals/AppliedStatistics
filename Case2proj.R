dev.off() 
rm(list=ls())
data <- read.table("case2regionsOnePerBatch.txt",header=T)
#make new column in the data
data$ratio <- (data$pos/data$total)
####################### Visualize the data #######################
plot(data$ratio,typ="l")
plot(data$ratio[1:52],typ="l")

#new data.frame that includes only the parameters of interest
names(data)
data.primary <- data[,c("aveTemp","maxTemp","relHum","sunHours","precip","ratio")]
data.primary.vis <- data.primary[!(data.primary$relHum==0),] #delete zeros from the humidity data
##############!!!!remove the rows that have relHum==0

#plot of the correlation between variables
pairs(data.primary.vis,panel=panel.smooth)
#comment on this plot in the report

#eliminate the na rows 
data.primary.NAomit0 <- data.primary[complete.cases(data.primary),]
#comment on plot as it indicates correlation with variables
##### 
#use tree for visualizing in which degree the different variables influence the variance
#install.packages("tree")
library(tree)
tree.mod1<-tree(ratio~.,data.primary)
plot(tree.mod1)
text(tree.mod1)
#the temperature and the sun hours seem to have great impact on the variation (consider these variables)

####################### Analyze the parameters for patterns #######################
#install.packages("mgcv")
library(mgcv)
gam1<-gam(ratio~s(aveTemp)+s(maxTemp)+s(maxTemp)+s(relHum)+s(sunHours)+s(precip),data=data.primary.NAomit0)
summary(gam1)
#the s(precip is not significant)
par(mfrow=c(2,3))
plot(gam1)
#Following suggestions from the gam model
    #aveTemp - pwl or quadratic
    #maxTemp - linear or quadratic
    #relHum - linear
    #sunHours - linear or nonlinear
    #precip - linear or pwl - not significant though
####### intermediate test 19.ja ###########
lm13 <- lm(ratio~aveTemp*maxTemp*relHum*sunHours,data=data.primary.NAomit0)
summary(lm13)
lm14 <- step(lm13)
#drop the highest order term
drop1(lm14,test="F")
summary(lm14)
par(mfrow=c(2,2))
plot(lm14)

for ( i in  c("aveTemp","maxTemp","relHum","sunHours")){
  plot(data.primary.NAomit0[[i]],residuals(lm14),type="n",xlab=i)
  panel.smooth(data.primary.NAomit0[[i]],residuals(lm14))
}

pwl<-function(x,x0){
  return( (x > x0) * (x-x0) )
}
optim<-optimize(function(zz){
  sum( residuals(lm(ratio ~ aveTemp*maxTemp*relHum*sunHours + pwl(aveTemp,zz),data=data.primary.NAomit0))^2 )
},c(3,8))
(x0.opt<-optim$minimum)
#add a pwl for aveTemp
lm15 <- update(lm14,~.+pwl(aveTemp,x0.opt))
anova(lm15)

lm16 <- step(lm15)
anova(lm16)
drop1(lm16,test="F")
lm17 <- update(lm16,~.-maxTemp:relHum:sunHours)
anova(lm17)
plot(lm17)
for ( i in  c("aveTemp","maxTemp","relHum","sunHours")){
  plot(data.primary.NAomit0[[i]],residuals(lm17),type="n",xlab=i)
  panel.smooth(data.primary.NAomit0[[i]],residuals(lm17))
}
drop1(lm17,test="F")
summary(lm17)

#similar as 
lm18 <- lm(log(ratio+1)~(aveTemp+maxTemp+relHum+sunHours)^2+pwl(aveTemp,x0.opt),data=data.primary.NAomit0)
summary(lm18)
plot(lm18)

######
#make a raw lm with all parameters and interactions
lm1 <- lm(ratio~aveTemp*maxTemp*relHum*sunHours*precip,data=data.primary.NAomit0)
anova(lm1)
drop1(lm1)
#suggest that no parameters should be eliminated as the highest order term is significant
#HOW DO WE CHOOSE WHICH PARAMETER INTERACTIONS TO KEEP?
par(mfrow=c(2,2))
plot(lm1)
#residuals do not look good as expected
par(mfrow=c(3,2))
for ( i in  c("aveTemp","maxTemp","relHum","sunHours","precip")){
  plot(data.primary.NAomit0[[i]],residuals(lm1),type="n",xlab=i)
  panel.smooth(data.primary.NAomit0[[i]],residuals(lm1))
}
lm12 <- lm(ratio~aveTemp+maxTemp+relHum+sunHours+precip,data.primary.NAomit0)
par(mfrow=c(2,2))
plot(lm12)
#looks better than before
anova(lm2)
#sunHours still not significant- but the squared term is
par(mfrow=c(2,2))
for ( i in  c("aveTemp","relHum","sunHours","precip")){
  plot(data.primary[[i]],residuals(lm2),type="n",xlab=i)
  panel.smooth(data.primary[[i]],residuals(lm2))
}



lm2 <- lm(ratio~aveTemp+relHum+sunHours+precip+pwl(aveTemp,11.25)+I(aveTemp^2)+I(sunHours^2),data.primary)
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
lm3 <- lm(ratio~aveTemp+relHum+sunHours+precip+pwl(aveTemp,11.25)+I(aveTemp^2)+I(sunHours^2)+I(precip^2),data.primary)
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
lm5 <- lm(sqrt(ratio)~aveTemp+relHum+sunHours+precip+pwl(aveTemp,11.25)+I(aveTemp^2)+I(sunHours^2),data.primary)
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



<<<<<<< HEAD

=======

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



>>>>>>> 9444a5f5f61adb6e5732ab54c8b03983bb244840
