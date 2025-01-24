Nloglike.ch<-function(df,y){
  f<-outer(y,df,FUN=function(y,df) dchisq(y,df,log=TRUE))
  f<-(-1)*apply(f,2,sum)
  return(f)
}
set.seed(143)
n=100
y<-rchisq(n,3)
result<-optim(0.5, Nloglike.ch,y=y,method="Brent",lower = 0.05,upper = 10,hessian = TRUE)
result
df.hat<-result$par
df.hat
se.mle<-sqrt(result$hessian^(-1))
se.mle
print(cbind(direct=c(mean=mean(y), sd=sqrt(mean(y))), optim =c(result$par,se.mle)),digits = 4)
MLE.chisq<-function(df0,y){  
  Nloglike.ch<-function(df,y){
    f<-outer(y,df,FUN=function(y,df) dchisq(y,df,log=TRUE))
    f<-(-1)*apply(f,2,sum)
    return(f)
  }
  result<-optim(df0,Nloglike.ch,y=y,method = "Brent", lower = 0.1, upper = 10, hessian = TRUE)
  return(result)
}
Re<-MLE.chisq(0.5,y)
Re
df.hat<-Re$par
df.hat
B=2000
n<-length(y)
Lhat.Bootstrap<-numeric(B)
for(i in 1:B){
  ystar<-rchisq(n,df.hat)
  Lhat.Bootstrap[i]<-MLE.chisq(0.5,ystar)$par
}
hist(Lhat.Bootstrap, main = "sampling distribution of Bootrapped MLE for chisq", xlab = "estimated value", breaks = 30, prob = TRUE,col="gray93")
abline(v=mean(Lhat.Bootstrap),col="goldenrod4",lty=2,lwd=2)
lines(density(Lhat.Bootstrap),col="maroon",lty=2,lwd=2)
abline(v=2.954225,col="red",lty=3,lwd=2)
mu<-mean(Lhat.Bootstrap)
sigma<-sd(Lhat.Bootstrap)
curve(dnorm(x,mu,sigma),col="orange",add=TRUE)
B=2000
n<-length(y)
Lhat.Bootstrap1<-numeric(B)
for(i in 1:B){
  ystar1<-sample(y,n,replace=TRUE)
  Lhat.Bootstrap1[i]<-MLE.chisq(0.5,ystar1)$par
}
hist(Lhat.Bootstrap1, main = "sampling distribution of Bootrapped MLE for chisq", xlab = "estimated value", breaks = 30, prob = TRUE,col="gray95")
abline(v=mean(Lhat.Bootstrap1),col=2,lty=2,lwd=2)
lines(density(Lhat.Bootstrap1),col="blue",lty=3)
mu<-mean(Lhat.Bootstrap1)
sigma<-sd(Lhat.Bootstrap1)
curve(dnorm(x,mu,sigma),col=colors()[12],add=TRUE)
plot(density(Lhat.Bootstrap),col=4,lty=2)
lines(density(Lhat.Bootstrap1),col=2,lty=3)
abline(v=mean(Lhat.Bootstrap),col= 2,lty=2)
abline(v=mean(Lhat.Bootstrap1),col=4,lty=3)
par(mfrow=c(2,1),mar=c(2,2,2,2))
ts.plot(Lhat.Bootstrap,col="gray70")
abline(h=mean(Lhat.Bootstrap),col="green",lty=2)
ts.plot(Lhat.Bootstrap1,col="gray70")
abline(h=mean(Lhat.Bootstrap1),col="green",lty=2)
Re<-MLE.chisq(0.5,y)
Re
df.hat<-Re$par
SE.df<-sqrt((Re$hessian)^(-1))
alpha=0.05
L<-df.hat-qnorm(1-alpha/2)*SE.df
L
U<-df.hat+qnorm(1-alpha/2)*SE.df
U
c(L,U)
conf.boot<-quantile(Lhat.Bootstrap, probs=c(0.025,0.975), names=FALSE) 
conf.boot
conf.boot1<-quantile(Lhat.Bootstrap1, probs=c(0.025,0.975), names=FALSE) 
conf.boot1
par(mfrow=c(2,1),mar=c(2,2,2,2))
ts.plot(Lhat.Bootstrap,col="gray70")
abline(h=mean(Lhat.Bootstrap),col="black",lty=2)
abline(h=c(L,U),lty=3,col="blue")
ts.plot(Lhat.Bootstrap1,col="gray70")
abline(h=mean(Lhat.Bootstrap1),col="green",lty=2)
abline(h=conf.boot,lty=3,col="pink")
abline(h=conf.boot1,lty=3,col="black")
polygon(c(1:B, rev(1:B)), c(rep(conf.boot[1],B), rev(rep(conf.boot[2],B))), col = rgb(.9,.2,.8,alpha=.2), border = NA)

####### n=500   #############
Nloglike.ch<-function(df,y){
  f<-outer(y,df,FUN=function(y,df) dchisq(y,df,log=TRUE))
  f<-(-1)*apply(f,2,sum)
  return(f)
}
set.seed(143)
n=500
y<-rchisq(n,3)
result<-optim(0.5, Nloglike.ch,y=y,method="Brent",lower = 0.05,upper = 10,hessian = TRUE)
result
df.hat<-result$par
df.hat
se.mle<-sqrt(result$hessian^(-1))
se.mle
print(cbind(direct=c(mean=mean(y), sd=sqrt(mean(y))), optim =c(result$par,se.mle)),digits = 4)
MLE.chisq<-function(df0,y){  
  Nloglike.ch<-function(df,y){
    f<-outer(y,df,FUN=function(y,df) dchisq(y,df,log=TRUE))
    f<-(-1)*apply(f,2,sum)
    return(f)
  }
  result<-optim(df0,Nloglike.ch,y=y,method = "Brent", lower = 0.1, upper = 10, hessian = TRUE)
  return(result)
}
Re<-MLE.chisq(0.5,y)
Re
df.hat<-Re$par
df.hat
B=2000
n<-length(y)
Lhat.Bootstrap<-numeric(B)
for(i in 1:B){
  ystar<-rchisq(n,df.hat)
  Lhat.Bootstrap[i]<-MLE.chisq(0.5,ystar)$par
}
hist(Lhat.Bootstrap, main = "sampling distribution of Bootrapped MLE for chisq", xlab = "estimated value", breaks = 30, prob = TRUE,col="gray93")
abline(v=mean(Lhat.Bootstrap),col="goldenrod4",lty=2,lwd=2)
lines(density(Lhat.Bootstrap),col="maroon",lty=2,lwd=2)
abline(v=2.954225,col="red",lty=3,lwd=2)
mu<-mean(Lhat.Bootstrap)
sigma<-sd(Lhat.Bootstrap)
curve(dnorm(x,mu,sigma),col="orange",add=TRUE)
B=2000
n<-length(y)
Lhat.Bootstrap1<-numeric(B)
for(i in 1:B){
  ystar1<-sample(y,n,replace=TRUE)
  Lhat.Bootstrap1[i]<-MLE.chisq(0.5,ystar1)$par
}
hist(Lhat.Bootstrap1, main = "sampling distribution of Bootrapped MLE for chisq", xlab = "estimated value", breaks = 30, prob = TRUE,col="gray95")
abline(v=mean(Lhat.Bootstrap1),col=2,lty=2,lwd=2)
lines(density(Lhat.Bootstrap1),col="blue",lty=3)
mu<-mean(Lhat.Bootstrap1)
sigma<-sd(Lhat.Bootstrap1)
curve(dnorm(x,mu,sigma),col=colors()[12],add=TRUE)
plot(density(Lhat.Bootstrap),col=4,lty=2)
lines(density(Lhat.Bootstrap1),col=2,lty=3)
abline(v=mean(Lhat.Bootstrap),col= 2,lty=2)
abline(v=mean(Lhat.Bootstrap1),col=4,lty=3)
par(mfrow=c(2,1),mar=c(2,2,2,2))
ts.plot(Lhat.Bootstrap,col="gray70")
abline(h=mean(Lhat.Bootstrap),col="green",lty=2)
ts.plot(Lhat.Bootstrap1,col="gray70")
abline(h=mean(Lhat.Bootstrap1),col="green",lty=2)
Re<-MLE.chisq(0.5,y)
Re
df.hat<-Re$par
SE.df<-sqrt((Re$hessian)^(-1))
alpha=0.05
L<-df.hat-qnorm(1-alpha/2)*SE.df
L
U<-df.hat+qnorm(1-alpha/2)*SE.df
U
c(L,U)
conf.boot<-quantile(Lhat.Bootstrap, probs=c(0.025,0.975), names=FALSE) 
conf.boot
conf.boot1<-quantile(Lhat.Bootstrap1, probs=c(0.025,0.975), names=FALSE) 
conf.boot1
par(mfrow=c(2,1),mar=c(2,2,2,2))
ts.plot(Lhat.Bootstrap,col="gray70")
abline(h=mean(Lhat.Bootstrap),col="black",lty=2)
abline(h=c(L,U),lty=3,col="blue")
ts.plot(Lhat.Bootstrap1,col="gray70")
abline(h=mean(Lhat.Bootstrap1),col="green",lty=2)
abline(h=conf.boot,lty=3,col="pink")
abline(h=conf.boot1,lty=3,col="black")
polygon(c(1:B, rev(1:B)), c(rep(conf.boot[1],B), rev(rep(conf.boot[2],B))), col = rgb(.9,.2,.8,alpha=.2), border = NA)
#####  n=1000 ###
Nloglike.ch<-function(df,y){
  f<-outer(y,df,FUN=function(y,df) dchisq(y,df,log=TRUE))
  f<-(-1)*apply(f,2,sum)
  return(f)
}
set.seed(143)
n=1000
y<-rchisq(n,3)
result<-optim(0.5, Nloglike.ch,y=y,method="Brent",lower = 0.05,upper = 10,hessian = TRUE)
result
df.hat<-result$par
df.hat
se.mle<-sqrt(result$hessian^(-1))
se.mle
print(cbind(direct=c(mean=mean(y), sd=sqrt(mean(y))), optim =c(result$par,se.mle)),digits = 4)
MLE.chisq<-function(df0,y){  
  Nloglike.ch<-function(df,y){
    f<-outer(y,df,FUN=function(y,df) dchisq(y,df,log=TRUE))
    f<-(-1)*apply(f,2,sum)
    return(f)
  }
  result<-optim(df0,Nloglike.ch,y=y,method = "Brent", lower = 0.1, upper = 10, hessian = TRUE)
  return(result)
}
Re<-MLE.chisq(0.5,y)
Re
df.hat<-Re$par
df.hat
B=2000
n<-length(y)
Lhat.Bootstrap<-numeric(B)
for(i in 1:B){
  ystar<-rchisq(n,df.hat)
  Lhat.Bootstrap[i]<-MLE.chisq(0.5,ystar)$par
}
hist(Lhat.Bootstrap, main = "sampling distribution of Bootrapped MLE for chisq", xlab = "estimated value", breaks = 30, prob = TRUE,col="gray93")
abline(v=mean(Lhat.Bootstrap),col="goldenrod4",lty=2,lwd=2)
lines(density(Lhat.Bootstrap),col="maroon",lty=2,lwd=2)
abline(v=2.954225,col="red",lty=3,lwd=2)
mu<-mean(Lhat.Bootstrap)
sigma<-sd(Lhat.Bootstrap)
curve(dnorm(x,mu,sigma),col="orange",add=TRUE)
B=2000
n<-length(y)
Lhat.Bootstrap1<-numeric(B)
for(i in 1:B){
  ystar1<-sample(y,n,replace=TRUE)
  Lhat.Bootstrap1[i]<-MLE.chisq(0.5,ystar1)$par
}
hist(Lhat.Bootstrap1, main = "sampling distribution of Bootrapped MLE for chisq", xlab = "estimated value", breaks = 30, prob = TRUE,col="gray95")
abline(v=mean(Lhat.Bootstrap1),col=2,lty=2,lwd=2)
lines(density(Lhat.Bootstrap1),col="blue",lty=3)
mu<-mean(Lhat.Bootstrap1)
sigma<-sd(Lhat.Bootstrap1)
curve(dnorm(x,mu,sigma),col=colors()[12],add=TRUE)
plot(density(Lhat.Bootstrap),col=4,lty=2)
lines(density(Lhat.Bootstrap1),col=2,lty=3)
abline(v=mean(Lhat.Bootstrap),col= 2,lty=2)
abline(v=mean(Lhat.Bootstrap1),col=4,lty=3)
par(mfrow=c(2,1),mar=c(2,2,2,2))
ts.plot(Lhat.Bootstrap,col="gray70")
abline(h=mean(Lhat.Bootstrap),col="green",lty=2)
ts.plot(Lhat.Bootstrap1,col="gray70")
abline(h=mean(Lhat.Bootstrap1),col="green",lty=2)
Re<-MLE.chisq(0.5,y)
Re
df.hat<-Re$par
SE.df<-sqrt((Re$hessian)^(-1))
alpha=0.05
L<-df.hat-qnorm(1-alpha/2)*SE.df
L
U<-df.hat+qnorm(1-alpha/2)*SE.df
U
c(L,U)
conf.boot<-quantile(Lhat.Bootstrap, probs=c(0.025,0.975), names=FALSE) 
conf.boot
conf.boot1<-quantile(Lhat.Bootstrap1, probs=c(0.025,0.975), names=FALSE) 
conf.boot1
par(mfrow=c(2,1),mar=c(2,2,2,2))
ts.plot(Lhat.Bootstrap,col="gray70")
abline(h=mean(Lhat.Bootstrap),col="black",lty=2)
abline(h=c(L,U),lty=3,col="blue")
ts.plot(Lhat.Bootstrap1,col="gray70")
abline(h=mean(Lhat.Bootstrap1),col="green",lty=2)
abline(h=conf.boot,lty=3,col="pink")
abline(h=conf.boot1,lty=3,col="black")
polygon(c(1:B, rev(1:B)), c(rep(conf.boot[1],B), rev(rep(conf.boot[2],B))), col = rgb(.9,.2,.8,alpha=.2), border = NA)
############   10000    ###########
Nloglike.ch<-function(df,y){
  f<-outer(y,df,FUN=function(y,df) dchisq(y,df,log=TRUE))
  f<-(-1)*apply(f,2,sum)
  return(f)
}
set.seed(143)
n=10000
y<-rchisq(n,3)
result<-optim(0.5, Nloglike.ch,y=y,method="Brent",lower = 0.05,upper = 10,hessian = TRUE)
result
df.hat<-result$par
df.hat
se.mle<-sqrt(result$hessian^(-1))
se.mle
print(cbind(direct=c(mean=mean(y), sd=sqrt(mean(y))), optim =c(result$par,se.mle)),digits = 4)
MLE.chisq<-function(df0,y){  
  Nloglike.ch<-function(df,y){
    f<-outer(y,df,FUN=function(y,df) dchisq(y,df,log=TRUE))
    f<-(-1)*apply(f,2,sum)
    return(f)
  }
  result<-optim(df0,Nloglike.ch,y=y,method = "Brent", lower = 0.1, upper = 10, hessian = TRUE)
  return(result)
}
Re<-MLE.chisq(0.5,y)
Re
df.hat<-Re$par
df.hat
B=2000
n<-length(y)
Lhat.Bootstrap<-numeric(B)
for(i in 1:B){
  ystar<-rchisq(n,df.hat)
  Lhat.Bootstrap[i]<-MLE.chisq(0.5,ystar)$par
}
hist(Lhat.Bootstrap, main = "sampling distribution of Bootrapped MLE for chisq", xlab = "estimated value", breaks = 30, prob = TRUE,col="gray93")
abline(v=mean(Lhat.Bootstrap),col="goldenrod4",lty=2,lwd=2)
lines(density(Lhat.Bootstrap),col="maroon",lty=2,lwd=2)
abline(v=2.954225,col="red",lty=3,lwd=2)
mu<-mean(Lhat.Bootstrap)
sigma<-sd(Lhat.Bootstrap)
curve(dnorm(x,mu,sigma),col="orange",add=TRUE)
B=2000
n<-length(y)
Lhat.Bootstrap1<-numeric(B)
for(i in 1:B){
  ystar1<-sample(y,n,replace=TRUE)
  Lhat.Bootstrap1[i]<-MLE.chisq(0.5,ystar1)$par
}
hist(Lhat.Bootstrap1, main = "sampling distribution of Bootrapped MLE for chisq", xlab = "estimated value", breaks = 30, prob = TRUE,col="gray95")
abline(v=mean(Lhat.Bootstrap1),col=2,lty=2,lwd=2)
lines(density(Lhat.Bootstrap1),col="blue",lty=3)
mu<-mean(Lhat.Bootstrap1)
sigma<-sd(Lhat.Bootstrap1)
curve(dnorm(x,mu,sigma),col=colors()[12],add=TRUE)
plot(density(Lhat.Bootstrap),col=4,lty=2)
lines(density(Lhat.Bootstrap1),col=2,lty=3)
abline(v=mean(Lhat.Bootstrap),col= 2,lty=2)
abline(v=mean(Lhat.Bootstrap1),col=4,lty=3)
par(mfrow=c(2,1),mar=c(2,2,2,2))
ts.plot(Lhat.Bootstrap,col="gray70")
abline(h=mean(Lhat.Bootstrap),col="green",lty=2)
ts.plot(Lhat.Bootstrap1,col="gray70")
abline(h=mean(Lhat.Bootstrap1),col="green",lty=2)
Re<-MLE.chisq(0.5,y)
Re
df.hat<-Re$par
SE.df<-sqrt((Re$hessian)^(-1))
alpha=0.05
L<-df.hat-qnorm(1-alpha/2)*SE.df
L
U<-df.hat+qnorm(1-alpha/2)*SE.df
U
c(L,U)
conf.boot<-quantile(Lhat.Bootstrap, probs=c(0.025,0.975), names=FALSE) 
conf.boot
conf.boot1<-quantile(Lhat.Bootstrap1, probs=c(0.025,0.975), names=FALSE) 
conf.boot1
par(mfrow=c(2,1),mar=c(2,2,2,2))
ts.plot(Lhat.Bootstrap,col="gray70")
abline(h=mean(Lhat.Bootstrap),col="black",lty=2)
abline(h=c(L,U),lty=3,col="blue")
ts.plot(Lhat.Bootstrap1,col="gray70")
abline(h=mean(Lhat.Bootstrap1),col="green",lty=2)
abline(h=conf.boot,lty=3,col="pink")
abline(h=conf.boot1,lty=3,col="black")
polygon(c(1:B, rev(1:B)), c(rep(conf.boot[1],B), rev(rep(conf.boot[2],B))), col = rgb(.9,.2,.8,alpha=.2), border = NA)


##########    FISHER    #########

n=100
y<-rf(n,10,5)
n <- length(y)
MLE.f<-function(df0,y){  
  floglike <- function(df,y) {
    f<- outer(y,df,FUN= function(y,df) df(y,df,5,log = TRUE))
    f<-(-1)*apply(f,2,sum)
    return(f)
  }
  result<-optim(df0,floglike,y=y,method = "Brent", lower = 0, upper = 50, hessian = TRUE)
  return(result)
}
Re<-MLE.f(2,y)
Re
df.hat<-Re$par
df.hat
B=2000   
n<-length(y)
dfhat.Bootstrap<-numeric(B)
for(i in 1:B){
  ystar<-rf(n,df.hat,5)
  dfhat.Bootstrap[i]<-MLE.f(0.5,ystar)$par
}
dfhat.Bootstrap
hist(dfhat.Bootstrap, main = "sampling distribution of Bootrapped MLE for fisher", xlab = "estimated value", breaks = 20, prob = TRUE)
abline(v=mean(dfhat.Bootstrap),col=290,lty=2,lwd=2)
abline(v=10,col="green",lty=2,lwd=2)
lines(density(dfhat.Bootstrap),col="blue",lty=3,lwd=2)
mu<-mean(dfhat.Bootstrap)
sigma<-sd(dfhat.Bootstrap)
curve(dnorm(x,mu,sigma),col=colors()[91],add=TRUE)
B=2000   
n<-length(y)
dfhat.Bootstrap1<-numeric(B)
for(i in 1:B){
  ystar1<-sample(y,n,replace=TRUE)
  dfhat.Bootstrap1[i]<-MLE.f(2,ystar1)$par
}
hist(dfhat.Bootstrap1, main = "sampling distribution of Bootrapped MLE for fisher", xlab = "estimated value", breaks = 20, prob = TRUE,col="gray95")
abline(v=mean(dfhat.Bootstrap1),col=2,lty=2,lwd=2)
lines(density(dfhat.Bootstrap1),col="blue",lty=3)
mu<-mean(dfhat.Bootstrap1)
sigma<-sd(dfhat.Bootstrap1)
curve(dnorm(x,mu,sigma),col=colors()[91],add=TRUE)
plot(density(dfhat.Bootstrap),ylim=c(0,0.4),col="blue",lty=2)
lines(density(dfhat.Bootstrap1),col="red",lty=3)
abline(v=mean(dfhat.Bootstrap),col= "blue",lty=2)
abline(v=mean(dfhat.Bootstrap1),col= "red",lty=3)
abline(v=10,col= "green",lty=3)
par(mfrow=c(2,1),mar=c(2,2,2,2))
ts.plot(dfhat.Bootstrap,col="gray70")
abline(h=mean(dfhat.Bootstrap),col="red",lty=2)
ts.plot(dfhat.Bootstrap1,col="gray70")
abline(h=mean(dfhat.Bootstrap1),col="red",lty=2)
Re<-MLE.f(2,y)
Re
df.hat<-Re$par
SE.df<-sqrt((Re$hessian)^(-1))
alpha=0.05
L<-df.hat-qnorm(1-alpha/2)*SE.df
L
U<-df.hat+qnorm(1-alpha/2)*SE.df
U
c(L,U)
conf.boot<-quantile(dfhat.Bootstrap, probs=c(0.025,0.975), names=FALSE) 
conf.boot
conf.boot1<-quantile(dfhat.Bootstrap1, probs=c(0.025,0.975), names=FALSE) 
conf.boot1
par(mfrow=c(2,1),mar=c(2,2,2,2))
ts.plot(dfhat.Bootstrap,col="gray70")
abline(h=mean(dfhat.Bootstrap),col="red",lty=2)
abline(h=c(L,U),lty=3,col="blue")
ts.plot(dfhat.Bootstrap1,col="gray70")
abline(h=mean(dfhat.Bootstrap1),col="red",lty=2)
abline(h=conf.boot,lty=3,col="blue")
abline(h=conf.boot1,lty=3,col="green")
polygon(c(1:B, rev(1:B)), c(rep(conf.boot[1],B), rev(rep(conf.boot[2],B))), col = rgb(.9,.2,.8,alpha=.2), border = NA)





n=500
y<-rf(n,10,5)
n <- length(y)
MLE.f<-function(df0,y){  
  floglike <- function(df,y) {
    f<- outer(y,df,FUN= function(y,df) df(y,df,5,log = TRUE))
    f<-(-1)*apply(f,2,sum)
    return(f)
  }
  result<-optim(df0,floglike,y=y,method = "Brent", lower = 0, upper = 50, hessian = TRUE)
  return(result)
}
Re<-MLE.f(2,y)
Re
df.hat<-Re$par
df.hat
B=2000   
n<-length(y)
dfhat.Bootstrap<-numeric(B)
for(i in 1:B){
  ystar<-rf(n,df.hat,5)
  dfhat.Bootstrap[i]<-MLE.f(0.5,ystar)$par
}
dfhat.Bootstrap
hist(dfhat.Bootstrap, main = "sampling distribution of Bootrapped MLE for fisher", xlab = "estimated value", breaks = 20, prob = TRUE)
abline(v=mean(dfhat.Bootstrap),col=290,lty=2,lwd=2)
abline(v=10,col="green",lty=2,lwd=2)
lines(density(dfhat.Bootstrap),col="blue",lty=3,lwd=2)
mu<-mean(dfhat.Bootstrap)
sigma<-sd(dfhat.Bootstrap)
curve(dnorm(x,mu,sigma),col=colors()[91],add=TRUE)
B=2000   
n<-length(y)
dfhat.Bootstrap1<-numeric(B)
for(i in 1:B){
  ystar1<-sample(y,n,replace=TRUE)
  dfhat.Bootstrap1[i]<-MLE.f(2,ystar1)$par
}
hist(dfhat.Bootstrap1, main = "sampling distribution of Bootrapped MLE for fisher", xlab = "estimated value", breaks = 20, prob = TRUE,col="gray95")
abline(v=mean(dfhat.Bootstrap1),col=2,lty=2,lwd=2)
lines(density(dfhat.Bootstrap1),col="blue",lty=3)
mu<-mean(dfhat.Bootstrap1)
sigma<-sd(dfhat.Bootstrap1)
curve(dnorm(x,mu,sigma),col=colors()[91],add=TRUE)
plot(density(dfhat.Bootstrap),ylim=c(0,0.4),col="blue",lty=2)
lines(density(dfhat.Bootstrap1),col="red",lty=3)
abline(v=mean(dfhat.Bootstrap),col= "blue",lty=2)
abline(v=mean(dfhat.Bootstrap1),col= "red",lty=3)
abline(v=10,col= "green",lty=3)
par(mfrow=c(2,1),mar=c(2,2,2,2))
ts.plot(dfhat.Bootstrap,col="gray70")
abline(h=mean(dfhat.Bootstrap),col="red",lty=2)
ts.plot(dfhat.Bootstrap1,col="gray70")
abline(h=mean(dfhat.Bootstrap1),col="red",lty=2)
Re<-MLE.f(2,y)
Re
df.hat<-Re$par
SE.df<-sqrt((Re$hessian)^(-1))
alpha=0.05
L<-df.hat-qnorm(1-alpha/2)*SE.df
L
U<-df.hat+qnorm(1-alpha/2)*SE.df
U
c(L,U)
conf.boot<-quantile(dfhat.Bootstrap, probs=c(0.025,0.975), names=FALSE) 
conf.boot
conf.boot1<-quantile(dfhat.Bootstrap1, probs=c(0.025,0.975), names=FALSE) 
conf.boot1
par(mfrow=c(2,1),mar=c(2,2,2,2))
ts.plot(dfhat.Bootstrap,col="gray70")
abline(h=mean(dfhat.Bootstrap),col="red",lty=2)
abline(h=c(L,U),lty=3,col="blue")
ts.plot(dfhat.Bootstrap1,col="gray70")
abline(h=mean(dfhat.Bootstrap1),col="red",lty=2)
abline(h=conf.boot,lty=3,col="blue")
abline(h=conf.boot1,lty=3,col="green")
polygon(c(1:B, rev(1:B)), c(rep(conf.boot[1],B), rev(rep(conf.boot[2],B))), col = rgb(.9,.2,.8,alpha=.2), border = NA)



n=1000
y<-rf(n,10,5)
n <- length(y)
MLE.f<-function(df0,y){  
  floglike <- function(df,y) {
    f<- outer(y,df,FUN= function(y,df) df(y,df,5,log = TRUE))
    f<-(-1)*apply(f,2,sum)
    return(f)
  }
  result<-optim(df0,floglike,y=y,method = "Brent", lower = 0, upper = 50, hessian = TRUE)
  return(result)
}
Re<-MLE.f(2,y)
Re
df.hat<-Re$par
df.hat
B=2000   
n<-length(y)
dfhat.Bootstrap<-numeric(B)
for(i in 1:B){
  ystar<-rf(n,df.hat,5)
  dfhat.Bootstrap[i]<-MLE.f(0.5,ystar)$par
}
dfhat.Bootstrap
hist(dfhat.Bootstrap, main = "sampling distribution of Bootrapped MLE for fisher", xlab = "estimated value", breaks = 20, prob = TRUE)
abline(v=mean(dfhat.Bootstrap),col=290,lty=2,lwd=2)
abline(v=10,col="green",lty=2,lwd=2)
lines(density(dfhat.Bootstrap),col="blue",lty=3,lwd=2)
mu<-mean(dfhat.Bootstrap)
sigma<-sd(dfhat.Bootstrap)
curve(dnorm(x,mu,sigma),col=colors()[91],add=TRUE)
B=2000   
n<-length(y)
dfhat.Bootstrap1<-numeric(B)
for(i in 1:B){
  ystar1<-sample(y,n,replace=TRUE)
  dfhat.Bootstrap1[i]<-MLE.f(2,ystar1)$par
}
hist(dfhat.Bootstrap1, main = "sampling distribution of Bootrapped MLE for fisher", xlab = "estimated value", breaks = 20, prob = TRUE,col="gray95")
abline(v=mean(dfhat.Bootstrap1),col=2,lty=2,lwd=2)
lines(density(dfhat.Bootstrap1),col="blue",lty=3)
mu<-mean(dfhat.Bootstrap1)
sigma<-sd(dfhat.Bootstrap1)
curve(dnorm(x,mu,sigma),col=colors()[91],add=TRUE)
plot(density(dfhat.Bootstrap),ylim=c(0,0.4),col="blue",lty=2)
lines(density(dfhat.Bootstrap1),col="red",lty=3)
abline(v=mean(dfhat.Bootstrap),col= "blue",lty=2)
abline(v=mean(dfhat.Bootstrap1),col= "red",lty=3)
abline(v=10,col= "green",lty=3)
par(mfrow=c(2,1),mar=c(2,2,2,2))
ts.plot(dfhat.Bootstrap,col="gray70")
abline(h=mean(dfhat.Bootstrap),col="red",lty=2)
ts.plot(dfhat.Bootstrap1,col="gray70")
abline(h=mean(dfhat.Bootstrap1),col="red",lty=2)
Re<-MLE.f(2,y)
Re
df.hat<-Re$par
SE.df<-sqrt((Re$hessian)^(-1))
alpha=0.05
L<-df.hat-qnorm(1-alpha/2)*SE.df
L
U<-df.hat+qnorm(1-alpha/2)*SE.df
U
c(L,U)
conf.boot<-quantile(dfhat.Bootstrap, probs=c(0.025,0.975), names=FALSE) 
conf.boot
conf.boot1<-quantile(dfhat.Bootstrap1, probs=c(0.025,0.975), names=FALSE) 
conf.boot1
par(mfrow=c(2,1),mar=c(2,2,2,2))
ts.plot(dfhat.Bootstrap,col="gray70")
abline(h=mean(dfhat.Bootstrap),col="red",lty=2)
abline(h=c(L,U),lty=3,col="blue")
ts.plot(dfhat.Bootstrap1,col="gray70")
abline(h=mean(dfhat.Bootstrap1),col="red",lty=2)
abline(h=conf.boot,lty=3,col="blue")
abline(h=conf.boot1,lty=3,col="green")
polygon(c(1:B, rev(1:B)), c(rep(conf.boot[1],B), rev(rep(conf.boot[2],B))), col = rgb(.9,.2,.8,alpha=.2), border = NA)




n=10000
y<-rf(n,10,5)
n <- length(y)
MLE.f<-function(df0,y){  
  floglike <- function(df,y) {
    f<- outer(y,df,FUN= function(y,df) df(y,df,5,log = TRUE))
    f<-(-1)*apply(f,2,sum)
    return(f)
  }
  result<-optim(df0,floglike,y=y,method = "Brent", lower = 0, upper = 50, hessian = TRUE)
  return(result)
}
Re<-MLE.f(2,y)
Re
df.hat<-Re$par
df.hat
B=2000   
n<-length(y)
dfhat.Bootstrap<-numeric(B)
for(i in 1:B){
  ystar<-rf(n,df.hat,5)
  dfhat.Bootstrap[i]<-MLE.f(0.5,ystar)$par
}
dfhat.Bootstrap
hist(dfhat.Bootstrap, main = "sampling distribution of Bootrapped MLE for fisher", xlab = "estimated value", breaks = 20, prob = TRUE)
abline(v=mean(dfhat.Bootstrap),col=290,lty=2,lwd=2)
abline(v=10,col="green",lty=2,lwd=2)
lines(density(dfhat.Bootstrap),col="blue",lty=3,lwd=2)
mu<-mean(dfhat.Bootstrap)
sigma<-sd(dfhat.Bootstrap)
curve(dnorm(x,mu,sigma),col=colors()[91],add=TRUE)
B=2000   
n<-length(y)
dfhat.Bootstrap1<-numeric(B)
for(i in 1:B){
  ystar1<-sample(y,n,replace=TRUE)
  dfhat.Bootstrap1[i]<-MLE.f(2,ystar1)$par
}
hist(dfhat.Bootstrap1, main = "sampling distribution of Bootrapped MLE for fisher", xlab = "estimated value", breaks = 20, prob = TRUE,col="gray95")
abline(v=mean(dfhat.Bootstrap1),col=2,lty=2,lwd=2)
lines(density(dfhat.Bootstrap1),col="blue",lty=3)
mu<-mean(dfhat.Bootstrap1)
sigma<-sd(dfhat.Bootstrap1)
curve(dnorm(x,mu,sigma),col=colors()[91],add=TRUE)
plot(density(dfhat.Bootstrap),ylim=c(0,0.4),col="blue",lty=2)
lines(density(dfhat.Bootstrap1),col="red",lty=3)
abline(v=mean(dfhat.Bootstrap),col= "blue",lty=2)
abline(v=mean(dfhat.Bootstrap1),col= "red",lty=3)
abline(v=10,col= "green",lty=3)
par(mfrow=c(2,1),mar=c(2,2,2,2))
ts.plot(dfhat.Bootstrap,col="gray70")
abline(h=mean(dfhat.Bootstrap),col="red",lty=2)
ts.plot(dfhat.Bootstrap1,col="gray70")
abline(h=mean(dfhat.Bootstrap1),col="red",lty=2)
Re<-MLE.f(2,y)
Re
df.hat<-Re$par
SE.df<-sqrt((Re$hessian)^(-1))
alpha=0.05
L<-df.hat-qnorm(1-alpha/2)*SE.df
L
U<-df.hat+qnorm(1-alpha/2)*SE.df
U
c(L,U)
conf.boot<-quantile(dfhat.Bootstrap, probs=c(0.025,0.975), names=FALSE) 
conf.boot
conf.boot1<-quantile(dfhat.Bootstrap1, probs=c(0.025,0.975), names=FALSE) 
conf.boot1
par(mfrow=c(2,1),mar=c(2,2,2,2))
ts.plot(dfhat.Bootstrap,col="gray70")
abline(h=mean(dfhat.Bootstrap),col="red",lty=2)
abline(h=c(L,U),lty=3,col="blue")
ts.plot(dfhat.Bootstrap1,col="gray70")
abline(h=mean(dfhat.Bootstrap1),col="red",lty=2)
abline(h=conf.boot,lty=3,col="blue")
abline(h=conf.boot1,lty=3,col="green")
polygon(c(1:B, rev(1:B)), c(rep(conf.boot[1],B), rev(rep(conf.boot[2],B))), col = rgb(.9,.2,.8,alpha=.2), border = NA)

###### NORMAL #####


Nloglike.Norm<- function(mu) {
  f<- outer(y,mu,FUN= function(y,mu) dnorm(y,mu,1,log = TRUE))
  f<-(-1)*apply(f,2,sum)
  return(f)
}
n=100
y<-rnorm(n,20,1)
y
result<-optim(0.5, function(x) Nloglike.Norm(x), method = "Brent", lower = 0.1, upper = 50, hessian = TRUE)
result
mu.hat<-result$par
mu.hat
se.mle<-sqrt(result$hessian^(-1))
se.mle
mu.hat
print(cbind(direct=c(mean=mean(y), sd=sqrt(mean(y))), optim =c(result$par,se.mle)),digits = 3)
MLE.Norm<-function(mu0,y){  
  Nloglike.Norm<- function(mu,y) {
    f<- outer(y,mu,FUN= function(y,mu) dnorm(y,mu,1,log = TRUE))
    f<-(-1)*apply(f,2,sum)
    return(f)
  }
  result<-optim(mu0,Nloglike.Norm,y=y,method = "Brent", lower = 0.1, upper = 50, hessian = TRUE)
  return(result)
}
Re<-MLE.Norm(.5,y)
Re
mu.hat<-Re$par
mu.hat
B=1500
n<-length(y)
muhat.Bootstrap<-numeric(B)
for(i in 1:B){
  ystar<-rnorm(n,mu.hat,1)
  muhat.Bootstrap[i]<-MLE.Norm(0.5,ystar)$par
}
muhat.Bootstrap
hist(muhat.Bootstrap, main = "sampling distribution of Bootrapped MLE for NORMAL", xlab = "estimated value", breaks = 30, prob = TRUE,col="gray")



abline(v=mean(muhat.Bootstrap),col=2,lty=2,lwd=2)
lines(density(muhat.Bootstrap),col="blue",lty=3)
abline(v=19.9,col="yellow",lty=2,lwd=3)
mu<-mean(muhat.Bootstrap)
sigma<-sd(muhat.Bootstrap)
curve(dnorm(x,mu,sigma),col=colors()[49],add=TRUE)
B=1500 
n<-length(y)
muhat.Bootstrap1<-numeric(B)
for(i in 1:B){
  ystar1<-sample(y,n,replace=TRUE)
  muhat.Bootstrap1[i]<-MLE.Norm(0.5,ystar1)$par
}
muhat.Bootstrap1
hist(muhat.Bootstrap1, main = "sampling distribution of Bootrapped MLE for NORMAL", xlab = "estimated value", breaks = 20, prob = TRUE,col="yellow")
abline(v=mean(muhat.Bootstrap1),col=2,lty=2,lwd=2)
lines(density(muhat.Bootstrap1),col="green",lty=3,lwd=4)
mu<-mean(muhat.Bootstrap1)
sigma<-sd(muhat.Bootstrap1)
curve(dnorm(x,mu,sigma),col=colors()[79],add=TRUE)
mean(muhat.Bootstrap1)
plot(density(muhat.Bootstrap),col="blue",lty=2)
lines(density(muhat.Bootstrap1),col="red",lty=4)
abline(v=mean(muhat.Bootstrap),col= "blue",lty=2)
abline(v=mean(muhat.Bootstrap1),col= "red",lty=3)
par(mfrow=c(2,1),mar=c(2,2,2,2))
ts.plot(muhat.Bootstrap,col="gray70")
abline(h=mean(muhat.Bootstrap),col="red",lty=2)
ts.plot(muhat.Bootstrap1,col="gray70")
abline(h=mean(muhat.Bootstrap1),col="red",lty=2)
Re<-MLE.Norm(0.5,y)
Re
mu.hat<-Re$par
SE.Norm<-sqrt((Re$hessian)^(-1))
SE.Norm
alpha=0.05
L<-mu.hat-qnorm(1-alpha/2)*SE.Norm
L
U<-mu.hat+qnorm(1-alpha/2)*SE.Norm
U
c(L,U)
conf.boot<-quantile(muhat.Bootstrap, probs=c(0.025,0.975), names=FALSE) 
conf.boot
conf.boot1<-quantile(muhat.Bootstrap1, probs=c(0.025,0.975), names=FALSE) 
conf.boot1
par(mfrow=c(2,1),mar=c(2,2,2,2))
ts.plot(muhat.Bootstrap,col="gray70")
abline(h=mean(muhat.Bootstrap),col="red",lty=2)
abline(h=c(L,U),lty=3,col="orange")
ts.plot(muhat.Bootstrap1,col="gray70")
abline(h=mean(muhat.Bootstrap1),col="red",lty=2)
abline(h=conf.boot,lty=3,col="orange")
abline(h=conf.boot1,lty=3,col="green")
polygon(c(1:B, rev(1:B)), c(rep(conf.boot[1],B), rev(rep(conf.boot[2],B))), col = rgb(.9,.2,.8,alpha=.2), border = NA)



Nloglike.Norm<- function(mu) {
  f<- outer(y,mu,FUN= function(y,mu) dnorm(y,mu,1,log = TRUE))
  f<-(-1)*apply(f,2,sum)
  return(f)
}
n=500
y<-rnorm(n,20,1)
y
result<-optim(0.5, function(x) Nloglike.Norm(x), method = "Brent", lower = 0.1, upper = 50, hessian = TRUE)
result
mu.hat<-result$par
mu.hat
se.mle<-sqrt(result$hessian^(-1))
se.mle
mu.hat
print(cbind(direct=c(mean=mean(y), sd=sqrt(mean(y))), optim =c(result$par,se.mle)),digits = 3)
MLE.Norm<-function(mu0,y){  
  Nloglike.Norm<- function(mu,y) {
    f<- outer(y,mu,FUN= function(y,mu) dnorm(y,mu,1,log = TRUE))
    f<-(-1)*apply(f,2,sum)
    return(f)
  }
  result<-optim(mu0,Nloglike.Norm,y=y,method = "Brent", lower = 0.1, upper = 50, hessian = TRUE)
  return(result)
}
Re<-MLE.Norm(.5,y)
Re
mu.hat<-Re$par
mu.hat
B=1500
n<-length(y)
muhat.Bootstrap<-numeric(B)
for(i in 1:B){
  ystar<-rnorm(n,mu.hat,1)
  muhat.Bootstrap[i]<-MLE.Norm(0.5,ystar)$par
}
muhat.Bootstrap
hist(muhat.Bootstrap, main = "sampling distribution of Bootrapped MLE for NORMAL", xlab = "estimated value", breaks = 30, prob = TRUE,col="gray")



abline(v=mean(muhat.Bootstrap),col=2,lty=2,lwd=2)
lines(density(muhat.Bootstrap),col="blue",lty=3)
abline(v=19.9,col="yellow",lty=2,lwd=3)
mu<-mean(muhat.Bootstrap)
sigma<-sd(muhat.Bootstrap)
curve(dnorm(x,mu,sigma),col=colors()[49],add=TRUE)
B=1500 
n<-length(y)
muhat.Bootstrap1<-numeric(B)
for(i in 1:B){
  ystar1<-sample(y,n,replace=TRUE)
  muhat.Bootstrap1[i]<-MLE.Norm(0.5,ystar1)$par
}
muhat.Bootstrap1
hist(muhat.Bootstrap1, main = "sampling distribution of Bootrapped MLE for NORMAL", xlab = "estimated value", breaks = 20, prob = TRUE,col="yellow")
abline(v=mean(muhat.Bootstrap1),col=2,lty=2,lwd=2)
lines(density(muhat.Bootstrap1),col="green",lty=3,lwd=4)
mu<-mean(muhat.Bootstrap1)
sigma<-sd(muhat.Bootstrap1)
curve(dnorm(x,mu,sigma),col=colors()[79],add=TRUE)
mean(muhat.Bootstrap1)
plot(density(muhat.Bootstrap),col="blue",lty=2)
lines(density(muhat.Bootstrap1),col="red",lty=4)
abline(v=mean(muhat.Bootstrap),col= "blue",lty=2)
abline(v=mean(muhat.Bootstrap1),col= "red",lty=3)
par(mfrow=c(2,1),mar=c(2,2,2,2))
ts.plot(muhat.Bootstrap,col="gray70")
abline(h=mean(muhat.Bootstrap),col="red",lty=2)
ts.plot(muhat.Bootstrap1,col="gray70")
abline(h=mean(muhat.Bootstrap1),col="red",lty=2)
Re<-MLE.Norm(0.5,y)
Re
mu.hat<-Re$par
SE.Norm<-sqrt((Re$hessian)^(-1))
SE.Norm
alpha=0.05
L<-mu.hat-qnorm(1-alpha/2)*SE.Norm
L
U<-mu.hat+qnorm(1-alpha/2)*SE.Norm
U
c(L,U)
conf.boot<-quantile(muhat.Bootstrap, probs=c(0.025,0.975), names=FALSE) 
conf.boot
conf.boot1<-quantile(muhat.Bootstrap1, probs=c(0.025,0.975), names=FALSE) 
conf.boot1
par(mfrow=c(2,1),mar=c(2,2,2,2))
ts.plot(muhat.Bootstrap,col="gray70")
abline(h=mean(muhat.Bootstrap),col="red",lty=2)
abline(h=c(L,U),lty=3,col="orange")
ts.plot(muhat.Bootstrap1,col="gray70")
abline(h=mean(muhat.Bootstrap1),col="red",lty=2)
abline(h=conf.boot,lty=3,col="orange")
abline(h=conf.boot1,lty=3,col="green")
polygon(c(1:B, rev(1:B)), c(rep(conf.boot[1],B), rev(rep(conf.boot[2],B))), col = rgb(.9,.2,.8,alpha=.2), border = NA)



Nloglike.Norm<- function(mu) {
  f<- outer(y,mu,FUN= function(y,mu) dnorm(y,mu,1,log = TRUE))
  f<-(-1)*apply(f,2,sum)
  return(f)
}
n=1000
y<-rnorm(n,20,1)
y
result<-optim(0.5, function(x) Nloglike.Norm(x), method = "Brent", lower = 0.1, upper = 50, hessian = TRUE)
result
mu.hat<-result$par
mu.hat
se.mle<-sqrt(result$hessian^(-1))
se.mle
mu.hat
print(cbind(direct=c(mean=mean(y), sd=sqrt(mean(y))), optim =c(result$par,se.mle)),digits = 3)
MLE.Norm<-function(mu0,y){  
  Nloglike.Norm<- function(mu,y) {
    f<- outer(y,mu,FUN= function(y,mu) dnorm(y,mu,1,log = TRUE))
    f<-(-1)*apply(f,2,sum)
    return(f)
  }
  result<-optim(mu0,Nloglike.Norm,y=y,method = "Brent", lower = 0.1, upper = 50, hessian = TRUE)
  return(result)
}
Re<-MLE.Norm(.5,y)
Re
mu.hat<-Re$par
mu.hat
B=1500
n<-length(y)
muhat.Bootstrap<-numeric(B)
for(i in 1:B){
  ystar<-rnorm(n,mu.hat,1)
  muhat.Bootstrap[i]<-MLE.Norm(0.5,ystar)$par
}
muhat.Bootstrap
hist(muhat.Bootstrap, main = "sampling distribution of Bootrapped MLE for NORMAL", xlab = "estimated value", breaks = 30, prob = TRUE,col="gray")



abline(v=mean(muhat.Bootstrap),col=2,lty=2,lwd=2)
lines(density(muhat.Bootstrap),col="blue",lty=3)
abline(v=19.9,col="yellow",lty=2,lwd=3)
mu<-mean(muhat.Bootstrap)
sigma<-sd(muhat.Bootstrap)
curve(dnorm(x,mu,sigma),col=colors()[49],add=TRUE)
B=1500 
n<-length(y)
muhat.Bootstrap1<-numeric(B)
for(i in 1:B){
  ystar1<-sample(y,n,replace=TRUE)
  muhat.Bootstrap1[i]<-MLE.Norm(0.5,ystar1)$par
}
muhat.Bootstrap1
hist(muhat.Bootstrap1, main = "sampling distribution of Bootrapped MLE for NORMAL", xlab = "estimated value", breaks = 20, prob = TRUE,col="yellow")
abline(v=mean(muhat.Bootstrap1),col=2,lty=2,lwd=2)
lines(density(muhat.Bootstrap1),col="green",lty=3,lwd=4)
mu<-mean(muhat.Bootstrap1)
sigma<-sd(muhat.Bootstrap1)
curve(dnorm(x,mu,sigma),col=colors()[79],add=TRUE)
mean(muhat.Bootstrap1)
plot(density(muhat.Bootstrap),col="blue",lty=2)
lines(density(muhat.Bootstrap1),col="red",lty=4)
abline(v=mean(muhat.Bootstrap),col= "blue",lty=2)
abline(v=mean(muhat.Bootstrap1),col= "red",lty=3)
par(mfrow=c(2,1),mar=c(2,2,2,2))
ts.plot(muhat.Bootstrap,col="gray70")
abline(h=mean(muhat.Bootstrap),col="red",lty=2)
ts.plot(muhat.Bootstrap1,col="gray70")
abline(h=mean(muhat.Bootstrap1),col="red",lty=2)
Re<-MLE.Norm(0.5,y)
Re
mu.hat<-Re$par
SE.Norm<-sqrt((Re$hessian)^(-1))
SE.Norm
alpha=0.05
L<-mu.hat-qnorm(1-alpha/2)*SE.Norm
L
U<-mu.hat+qnorm(1-alpha/2)*SE.Norm
U
c(L,U)
conf.boot<-quantile(muhat.Bootstrap, probs=c(0.025,0.975), names=FALSE) 
conf.boot
conf.boot1<-quantile(muhat.Bootstrap1, probs=c(0.025,0.975), names=FALSE) 
conf.boot1
par(mfrow=c(2,1),mar=c(2,2,2,2))
ts.plot(muhat.Bootstrap,col="gray70")
abline(h=mean(muhat.Bootstrap),col="red",lty=2)
abline(h=c(L,U),lty=3,col="orange")
ts.plot(muhat.Bootstrap1,col="gray70")
abline(h=mean(muhat.Bootstrap1),col="red",lty=2)
abline(h=conf.boot,lty=3,col="orange")
abline(h=conf.boot1,lty=3,col="green")
polygon(c(1:B, rev(1:B)), c(rep(conf.boot[1],B), rev(rep(conf.boot[2],B))), col = rgb(.9,.2,.8,alpha=.2), border = NA)



Nloglike.Norm<- function(mu) {
  f<- outer(y,mu,FUN= function(y,mu) dnorm(y,mu,1,log = TRUE))
  f<-(-1)*apply(f,2,sum)
  return(f)
}
n=10000
y<-rnorm(n,20,1)
y
result<-optim(0.5, function(x) Nloglike.Norm(x), method = "Brent", lower = 0.1, upper = 50, hessian = TRUE)
result
mu.hat<-result$par
mu.hat
se.mle<-sqrt(result$hessian^(-1))
se.mle
mu.hat
print(cbind(direct=c(mean=mean(y), sd=sqrt(mean(y))), optim =c(result$par,se.mle)),digits = 3)
MLE.Norm<-function(mu0,y){  
  Nloglike.Norm<- function(mu,y) {
    f<- outer(y,mu,FUN= function(y,mu) dnorm(y,mu,1,log = TRUE))
    f<-(-1)*apply(f,2,sum)
    return(f)
  }
  result<-optim(mu0,Nloglike.Norm,y=y,method = "Brent", lower = 0.1, upper = 50, hessian = TRUE)
  return(result)
}
Re<-MLE.Norm(.5,y)
Re
mu.hat<-Re$par
mu.hat
B=1500
n<-length(y)
muhat.Bootstrap<-numeric(B)
for(i in 1:B){
  ystar<-rnorm(n,mu.hat,1)
  muhat.Bootstrap[i]<-MLE.Norm(0.5,ystar)$par
}
muhat.Bootstrap
hist(muhat.Bootstrap, main = "sampling distribution of Bootrapped MLE for NORMAL", xlab = "estimated value", breaks = 30, prob = TRUE,col="gray")
abline(v=mean(muhat.Bootstrap),col=2,lty=2,lwd=2)
lines(density(muhat.Bootstrap),col="blue",lty=3)
abline(v=19.9,col="yellow",lty=2,lwd=3)
mu<-mean(muhat.Bootstrap)
sigma<-sd(muhat.Bootstrap)
curve(dnorm(x,mu,sigma),col=colors()[49],add=TRUE)
B=1500 
n<-length(y)
muhat.Bootstrap1<-numeric(B)
for(i in 1:B){
  ystar1<-sample(y,n,replace=TRUE)
  muhat.Bootstrap1[i]<-MLE.Norm(0.5,ystar1)$par
}
muhat.Bootstrap1
hist(muhat.Bootstrap1, main = "sampling distribution of Bootrapped MLE for NORMAL", xlab = "estimated value", breaks = 20, prob = TRUE,col="yellow")
abline(v=mean(muhat.Bootstrap1),col=2,lty=2,lwd=2)
lines(density(muhat.Bootstrap1),col="green",lty=3,lwd=4)
mu<-mean(muhat.Bootstrap1)
sigma<-sd(muhat.Bootstrap1)
curve(dnorm(x,mu,sigma),col=colors()[79],add=TRUE)
mean(muhat.Bootstrap1)
plot(density(muhat.Bootstrap),col="blue",lty=2)
lines(density(muhat.Bootstrap1),col="red",lty=4)
abline(v=mean(muhat.Bootstrap),col= "blue",lty=2)
abline(v=mean(muhat.Bootstrap1),col= "red",lty=3)
par(mfrow=c(2,1),mar=c(2,2,2,2))
ts.plot(muhat.Bootstrap,col="gray70")
abline(h=mean(muhat.Bootstrap),col="red",lty=2)
ts.plot(muhat.Bootstrap1,col="gray70")
abline(h=mean(muhat.Bootstrap1),col="red",lty=2)
Re<-MLE.Norm(0.5,y)
Re
mu.hat<-Re$par
SE.Norm<-sqrt((Re$hessian)^(-1))
SE.Norm
alpha=0.05
L<-mu.hat-qnorm(1-alpha/2)*SE.Norm
L
U<-mu.hat+qnorm(1-alpha/2)*SE.Norm
U
c(L,U)
conf.boot<-quantile(muhat.Bootstrap, probs=c(0.025,0.975), names=FALSE) 
conf.boot
conf.boot1<-quantile(muhat.Bootstrap1, probs=c(0.025,0.975), names=FALSE) 
conf.boot1
par(mfrow=c(2,1),mar=c(2,2,2,2))
ts.plot(muhat.Bootstrap,col="gray70")
abline(h=mean(muhat.Bootstrap),col="red",lty=2)
abline(h=c(L,U),lty=3,col="orange")
ts.plot(muhat.Bootstrap1,col="gray70")
abline(h=mean(muhat.Bootstrap1),col="red",lty=2)
abline(h=conf.boot,lty=3,col="orange")
abline(h=conf.boot1,lty=3,col="green")
polygon(c(1:B, rev(1:B)), c(rep(conf.boot[1],B), rev(rep(conf.boot[2],B))), col = rgb(.9,.2,.8,alpha=.2), border = NA)
