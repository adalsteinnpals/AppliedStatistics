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
data.primary <- data.primary[(data.primary$relHum[1:470]!=0),] #delete zeros from the humidity data
##############!!!!remove the rows that have relHum==0

#plot of the correlation between variables
pairs(data.primary,panel=panel.smooth)
#comment on this plot in the report

#eliminate the na rows 
data.primary.NAomit0 <- data.primary[complete.cases(data.primary),]
data.primary.NAomit1 <- data.primary[complete.cases(data.primary$relHum),]

#comment on plot as it indicates correlation with variables
##### 
#use tree for visualizing in which degree the different variables influence the variance
#install.packages("tree")
library(tree)
par(mfrow=c(1,1))
tree.mod1<-tree(ratio~.,data.primary.NAomit0)
plot(tree.mod1)
text(tree.mod1)
#the temperature and the sun hours seem to have great impact on the variation 
#(consider these variables)

####################### Analyze the parameters for patterns #######################
#install.packages("mgcv")
library(mgcv)
<<<<<<< HEAD
#in order to locate possible transformations of the explanatory variables - no interactions
gam1<-gam(ratio~s(aveTemp)+s(maxTemp)+s(maxTemp)+s(relHum)+s(sunHours)+s(precip),data=data.primary.NAomit0)
summary(gam1) 
#the s(precip) is not significant
=======
gam1<-gam(ratio~s(aveTemp)+s(maxTemp)+s(relHum)+s(sunHours)+s(precip),data=data.primary.NAomit0)
summary(gam1)
#the s(precip is not significant)
>>>>>>> 14dce1f50d8aaa4beb3d7fa85df089c23981d8b2
par(mfrow=c(2,3))
plot(gam1)


gam1<-gam(ratio~s(aveTemp)+s(maxTemp)+s(relHum)+s(sunHours)+s(precip),data=data.primary.NAomit0)
summary(gam1)
#the s(precip is not significant)
par(mfrow=c(2,3))
plot(gam1)
#Following suggestions from the gam model
    #aveTemp - pwl or quadratic
    #maxTemp - linear or quadratic
    #relHum - linear
    #sunHours - linear or nonlinear
    #precip - linear or pwl - not significant 

####### intermediate test 19.jan ###########
#we check the significance of the variables is similar for the general linear model - with no interactions
lm1 <- lm(ratio~(aveTemp+maxTemp+relHum+sunHours+precip),data.primary.NAomit0)
anova(lm1)
drop1(lm1,test="F")
par(mfrow=c(2,2))
plot(lm1)
# according to a F test, precip is insignificant and the residuals look bad
#such was also the case for the residuals in the two- and three-way interactions
#we prefere to look at transformation rather than interaction - BETTER REASONING NEEDED

#for next model we remove the "precip" as it was not significant in previous models
#furthermore it seems to be fairly uncorrelated to the other parameters - see paired plot
#we saw in gam1 that an pwl term could be added for the aveTemp - so we locate the optimal split point
pwl<-function(x,x0){
  return( (x > x0) * (x-x0) )
}

optim<-optimize(function(zz){
  sum( residuals(lm(ratio ~ (aveTemp+maxTemp+relHum+sunHours)+pwl(aveTemp,zz),data=data.primary.NAomit0))^2 )
},c(5,11)) 

(x2.opt<-optim$minimum)

lm2 <- lm(ratio~aveTemp+maxTemp+relHum+sunHours+pwl(aveTemp,x2.opt),data=data.primary.NAomit0)
anova(lm2)
drop1(lm2,test="F")
par(mfrow=c(2,2))
plot(lm2)
#residuals look better but still not sufficiently good
#look at individual residuals
for ( i in  c("aveTemp","maxTemp","relHum","sunHours")){
  plot(data.primary.NAomit0[[i]],residuals(lm2),type="n",xlab=i)
  panel.smooth(data.primary.NAomit0[[i]],residuals(lm2))
}
#the residuals for relHum and sunHours should be considered for transformation
#sunHours are insignificant and could probably be removed 
#new model
#we remove the sunHours due to insignificans - also consider more data due to many NA's in sunHours
#data.primary.NAomit1
optim<-optimize(function(zz){
  sum( residuals(lm(ratio ~ (aveTemp+maxTemp+relHum)+pwl(aveTemp,zz),data=data.primary.NAomit1))^2 )
},c(5,11))  

(x3.opt<-optim$minimum)

lm3 <- lm(ratio~aveTemp+maxTemp+relHum+pwl(aveTemp,x3.opt),data=data.primary.NAomit1)
anova(lm3)
drop1(lm3,test="F")
#all parameters significant
par(mfrow=c(2,2))
plot(lm3)
#variance increases for the total, some strange leverage points, but otherwise better than before
par(mfrow=c(2,2))
for ( i in  c("aveTemp","maxTemp","relHum")){
  plot(data.primary.NAomit1[[i]],residuals(lm3),type="n",xlab=i)
  panel.smooth(data.primary.NAomit1[[i]],residuals(lm3))
}
#the residuals look good for aveTemp and maxTemp -
#according to the drop1 we can drop the relHum and we try that

optim<-optimize(function(zz){
  sum( residuals(lm((ratio) ~aveTemp+maxTemp+pwl(aveTemp,zz),data=data.primary.NAomit0))^2 )
},c(3,15))
(x4.opt<-optim$minimum)

lm4 <- lm((ratio)~aveTemp+maxTemp+pwl(aveTemp,x4.opt),data=data.primary.NAomit0)
anova(lm4)
par(mfrow=c(2,2))
plot(lm4)
#all parameters significant but the variance increases with higher values
par(mfrow=c(2,1))
for ( i in  c("aveTemp","maxTemp")){
  plot(data.primary.NAomit0[[i]],residuals(lm4),type="n",xlab=i)
  panel.smooth(data.primary.NAomit0[[i]],residuals(lm4))
}
#that is thoug no the case for the eplainatory variables so we update the model by taking the 
#sqrt of the response variable

optim<-optimize(function(zz){
  sum( residuals(lm(sqrt(ratio) ~aveTemp+maxTemp+pwl(aveTemp,zz),data=data.primary.NAomit0))^2 )
},c(3,15))
(x5.opt<-optim$minimum)

lm5 <- lm(sqrt(ratio)~aveTemp+maxTemp+pwl(aveTemp,x5.opt),data=data.primary.NAomit0)
anova(lm5)
par(mfrow=c(2,2))
plot(lm5)

######### final model ########
optim<-optimize(function(zz){
  sum( residuals(lm(sqrt(ratio) ~aveTemp+relHum+pwl(aveTemp,zz),data=data.primary.NAomit0))^2 )
},c(3,15))
(x15.opt<-optim$minimum)

lm15 <- lm(sqrt(ratio)~aveTemp+relHum+pwl(aveTemp,x15.opt),data=data.primary.NAomit0)
anova(lm15)
summary(lm15)
par(mfrow=c(2,2))
plot(lm15)

par(mfrow=c(1,1))
tree15 <- tree(lm15)
plot(tree15)
text(tree15)
########


for ( i in  c("aveTemp","relHum","sunHours")){
  plot(data.primary.NAomit0[[i]],residuals(lm15),type="n",xlab=i)
  panel.smooth(data.primary.NAomit0[[i]],residuals(lm15))
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

as.character(data$sunHours)

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



dev.off()

setEPS()
postscript("boxplot.eps")
boxplot(data$ratioR1, data$ratioR2, data$ratioR3, data$ratioR4, data$ratioR5, data$ratioR6, data$ratioR7, data$ratioR8,xlab="Regions",ylab="Ratio of positive cases")
dev.off()


data.sub = subset(data,select=c(year,week,aveTemp,relHum))

data.R1 = data.frame(data.sub,data$ratioR1,region=1)
data.R2 = data.frame(data.sub,data$ratioR2,region=2)
data.R3 = data.frame(data.sub,data$ratioR3,region=3)
data.R4 = data.frame(data.sub,data$ratioR4,region=4)
data.R5 = data.frame(data.sub,data$ratioR5,region=5)
data.R6 = data.frame(data.sub,data$ratioR6,region=6)
data.R7 = data.frame(data.sub,data$ratioR7,region=7)
data.R8 = data.frame(data.sub,data$ratioR8,region=8)


colnames(data.R1) <- c("year","week","aveTemp","relHum","ratio","region")
colnames(data.R2) <- c("year","week","aveTemp","relHum","ratio","region")
colnames(data.R3) <- c("year","week","aveTemp","relHum","ratio","region")
colnames(data.R4) <- c("year","week","aveTemp","relHum","ratio","region")
colnames(data.R5) <- c("year","week","aveTemp","relHum","ratio","region")
colnames(data.R6) <- c("year","week","aveTemp","relHum","ratio","region")
colnames(data.R7) <- c("year","week","aveTemp","relHum","ratio","region")
colnames(data.R8) <- c("year","week","aveTemp","relHum","ratio","region")


data.reg = rbind(data.R1,data.R2,data.R3,data.R4,data.R5,data.R6,data.R7,data.R8)

data.reg$region = as.factor(data.reg$region)

data.reg

data.reg = data.reg[complete.cases(data.reg),]

lm15reg <- lm(sqrt(ratio)~region+(aveTemp+relHum)+pwl(aveTemp,x15.opt),data=data.reg)

summary(lm15reg)
summary(lm15)
par(mfrow=c(2,2))
plot(lm15)
plot(lm15reg)
data.reg



