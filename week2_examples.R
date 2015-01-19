##
## This file contains multiple small examples including
##
## Using:  tree
##         gam
##

####################################
## Example from Chapter 11 of the book
rm(list=ls())
dev.off()
ozone.pollution<-read.table("ozone.data.txt",header=T)
summary(ozone.pollution)

pairs(ozone.pollution,panel=panel.smooth)


### Using trees
#install.packages("tree")
library(tree)
model<-tree(ozone~.,data=ozone.pollution)

plot(model)
text(model)
## Here you can see the splits that gives the largest reduction in variance for each part.
## The height of lines gives the proportion the the variation that is explained. 
## For the ozone data temp<82.5 is the optimal first split and within each group wind is used
## in the following split ... etc.

## trees can help identify interactions ...

###############################
### Example using GAM (Generalized Additive Model)
### - a way to get ideas for transformations
### But cannot show interactions

#install.packages("mgcv")
library(mgcv)
par(mfrow=c(2,2))
model<-gam(ozone~s(rad)+s(temp)+s(wind),data=ozone.pollution)
summary(model)
## s(<something>) is a spline function with fractional df.
plot(model) # the y-labels gives the function and the associated df.
par(mfrow=c(1,1))

## Suggesting: linear in 'rad'
##             non-linear in 'temp'
##             linear or quadratic in 'wind'

test1<-lm(ozone~temp^2,data=ozone.pollution) ## Doesn't work as intented
summary(test1)
test2<-lm(ozone~I(temp^2),data=ozone.pollution) ## Better
summary(test2)

## After some intermediate models one could get to:
model1<-lm(ozone~temp+wind+rad+I(wind^2),data=ozone.pollution)
summary(model1)
par(mfrow=c(2,2))
plot(model1)

## To get ideas for further development and to test assumptions it is a good
## idea to plot residuals vs. all predictors:
par(mfrow=c(2,2))
for ( i in  c("temp","rad","wind")){
  plot(ozone.pollution[[i]],residuals(model1),type="n",xlab=i)
  panel.smooth(ozone.pollution[[i]],residuals(model1))
}
## The learning is that the dependence on temperature seems to lack a nonlinearity ... 
## e.g. a second order term

model2<-lm(ozone~temp+wind+rad+I(wind^2)+I(temp^2),data=ozone.pollution)
model2<-update(model1,~.+I(temp^2))
summary(model2)
par(mfrow=c(2,2))
plot(model2)
## Not perfect ... consider transforming the ozone (sqrt or log)

### Plotting predictions from a GLM
## To show the dependence on wind we need credible values for the other
## predictors.
summary(ozone.pollution)

## Easy way: Use means of others:
pred.data<-data.frame(rad=mean(ozone.pollution$rad),
                      temp=mean(ozone.pollution$temp),
                      wind=seq(2,22,length=20))
pred.int<-predict(model2, int="p",newdata=pred.data)
par(mfrow=c(1,1))

plot(ozone~wind,data=ozone.pollution)
matlines(pred.data$wind,pred.int,lty=c(1,2,2),col=2,lwd=2)
# Doesn't look too good ...

## But if variables are correlated ...
cor(ozone.pollution)
## then the mean is no longer the best estimator ...

## So instead one could use linear regression between the explanatory variables to get 
## credible values for the other predictors.

#####################################################################
## A function to predict the remaining predictors from one predictor:
## data: Data.frame with data
## reference: Which variable is to be the reference, e.g. "wind"
## others: Vector with names of the other variables to be predicted
##         the default is all remaining columns of data.
## ref.values: vector of values where predictions should be made
##         the default is 30 equidistant space values covering the
##         range of the reference.
#####################################################################
lec.fun<-function(data,reference,others=names(data)[names(data)!=reference],ref.values=seq(min(data[[reference]]),max(data[[reference]]),length=30)){
  pdata<-data.frame(reference=ref.values)
  names(pdata)<-reference
  for(i in others){
    lmtmp<-lm(as.formula(paste(i,"~",reference)),data)
    pdata[[i]]<-predict(lmtmp,newdata=pdata[reference])
  }
  return(pdata)
}

## An example:
plec <- lec.fun(ozone.pollution,reference="wind",others=c("rad","temp" ),ref.values=2:21)
plec

pred.plec<-predict(model2, int="p",newdata=plec)
matlines(pred.data$wind,pred.plec,lty=c(1,2,2),col=3,lwd=2)

legend("topright",legend=c("Using means","Using linear models","95% pred.int."),lty=c(1,1,2),col=c(2:3,1),lwd=2)
