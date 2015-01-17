spr<-read.table("SPR.txt",header=TRUE)
str(spr)
spr$EnzymeConc<-as.factor(spr$EnzymeConc)

# A sample model: (I hope that you find a better one.)
a1 <- lm(sqrt(Response)~(Enzyme+EnzymeConc+CaStock+DetStock)^4,spr)

# Calculating 95% confidence intervals:
pred.d<-expand.grid(Enzyme=levels(spr$Enzyme),EnzymeConc=levels(spr$EnzymeConc),DetStock="Det0",CaStock="Ca0")
pred<-predict(a1,pred.d,int="c")^2 # Predictions on original scale

par(mfrow=c(1,1))
# One style
matplot(c(0,2.5,7.5,15)[as.numeric(pred.d$EnzymeConc)],pred,type="n",ylim=range(pred))
matlines(c(0,2.5,7.5,15), cbind(matrix(pred[,1],nrow=4,byrow=TRUE),matrix(pred[,2],nrow=4,byrow=TRUE),matrix(pred[,3],nrow=4,byrow=TRUE)),col=2:6,lty=rep(c(1,2,2),each=5),lwd=2)
legend("topleft",legend=levels(spr$Enzyme),lty=1,col=2:6)

# A different style (Based on the same information)
dn<-list(Enzyme=levels(spr$Enzyme),EnzymeConc=levels(spr$EnzymeConc)) #Dimnames
barplot(matrix(pred[,3],nrow=5,dimnames=dn),beside=TRUE,col=2:6,legend.text=TRUE,args.legend=list(x="topleft"),main="95% confidence interval for Ca0 and Det0",ylab="Response",xlab="Enzyme concentration")
barplot(matrix(pred[,1],nrow=5,dimnames=dn),beside=TRUE,col=2:6,add=TRUE)
barplot(matrix(pred[,2],nrow=5,dimnames=dn),beside=TRUE,col=0,add=TRUE)


## A different way of indicating the error bars
#install.packages("Hmisc")
library(Hmisc)
barplot(matrix(pred[,1],nrow=5,dimnames=dn),beside=TRUE,col=2:6,legend.text=TRUE,args.legend=list(x="topleft"),main="95% confidence interval for Ca0 and Det0",ylab="Response",xlab="Enzyme concentration",ylim=c(0,max(pred)))
errbar((1:23)[-c(1:3)*6]+0.5,y=pred[,1],yminus=pred[,2],yplus=pred[,3],add=TRUE,pch=NA)

