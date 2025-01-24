################### likelihood fisher  #############################
set.seed(143)
n=10
y<-rf(10,0.3,0.4)
df2<-2
fisher<-function(df1,y){
  f<-0
  for(i in 1:length(df1)){
    f[i]<-prod(df(y,df1[i],df2))
  }
  return(f)
}
df1<-c(1,2)
fisher(df1,y)
curve(fisher(x,y),0,5,col=4)

##################### likelihood fisher without for  ########################
lik.fisher1<-function(df1,x){
  f<-outer(df1,x,FUN=function(df1,x) df(x,df1,2))
  f<-apply(f,1,prod)
  return(f)
  
}
par(mfrow = c(2,2))
df1<-c(1,3,5)
set.seed(143)
n=10
y<-rf(n,1,2)
lik.fisher1(df1,y)
result<-optim(1,function(x) (-1)*lik.fisher1(df1=x,y),method="Brent",lower = .5,upper = 5)
result
result$par
curve(lik.fisher1(x,y),0.1,4)
abline(v=c(1,result$par),lty=5,col=4:5)
title("n=10")

set.seed(143)
n=30
y<-rf(n,1,2)
lik.fisher1(df1,y)
result<-optim(1,function(x) (-1)*lik.fisher1(df1=x,y),method="Brent",lower = .5,upper = 5)
result
result$par
curve(lik.fisher1(x,y),0.1,4)
abline(v=c(1,result$par),lty=2,col=2:3)
title("n=30")

set.seed(143)
n=100
y<-rf(n,1,2)
lik.fisher1(df1,y)
result<-optim(1,function(x) (-1)*lik.fisher1(df1=x,y),method="Brent",lower = .5,upper = 5)
result
result$par
curve(lik.fisher1(x,y),0.1,4)
abline(v=c(1,result$par),lty=6,col=2:3)
title("n=100")

set.seed(143)
n=500
y<-rf(n,1,2)
lik.fisher1(df1,y)
result<-optim(1,function(x) (-1)*lik.fisher1(df1=x,y),method="Brent",lower = .5,upper = 5)
result
result$par
curve(lik.fisher1(x,y),0.1,4)
abline(v=c(1,result$par),lty=4,col=2:3)
title("n=500")

######################  log likelihoode  fisher #######################
loglik.fisher<-function(df1,y){
  f<-0
  for(i in 1:length(df1)){
    f[i]<-sum(df(y,df1[i],2,log =TRUE))
  }
  return(f)
}
set.seed(143)
n=10
y<-rf(n,1,2)
df1<-c(1,2)
loglik.fisher(df1,y)
result<-optim(1,function(x) (-1)*loglik.fisher(df1=x,y),method="Brent",lower = .5,upper = 5)
result
curve(loglik.fisher(x,y),0.1,4,col=4)
abline(v=c(1,result$par),lty=4,col=2:3)

######################  log likelihood  fisher without for #######################
Loglik.fisherl<-function(df1,x){
  f<-outer(df1,x,FUN=function(df1,x) df(x,df1,2,log=TRUE))
  f<-apply(f,1,sum)
  return(f)
  
}
par(mfrow = c(2,2))
df1<-c(1,3,5)
set.seed(143)
n=10
y<-rf(n,1,2)
Loglik.fisherl(df1,y)
result<-optim(1,function(x) (-1)*Loglik.fisherl(df1=x,y),method="Brent",lower = .5,upper = 5)
result
result$par
curve(Loglik.fisherl(x,y),0.1,4)
abline(v=c(1,result$par),lty=4,col=2:3)
title("n=10")

set.seed(143)
n=500
y<-rf(n,1,2)
Loglik.fisherl(df1,y)
result<-optim(1,function(x) (-1)*Loglik.fisherl(df1=x,y),method="Brent",lower = .5,upper = 5)
result
result$par
curve(Loglik.fisherl(x,y),0.1,4)
abline(v=c(1,result$par),lty=4,col=2:3)
title("n=500")


set.seed(143)
n=1000
y<-rf(n,1,2)
Loglik.fisherl(df1,y)
result<-optim(1,function(x) (-1)*Loglik.fisherl(df1=x,y),method="Brent",lower = .5,upper = 5)
result
result$par
curve(Loglik.fisherl(x,y),0.1,4)
abline(v=c(1,result$par),lty=4,col=2:3)
title("n=1000")


set.seed(143)
n=5000
y<-rf(n,1,2)
Loglik.fisherl(df1,y)
result<-optim(1,function(x) (-1)*Loglik.fisherl(df1=x,y),method="Brent",lower = .5,upper = 5)
result
result$par
curve(Loglik.fisherl(x,y),0.1,4)
abline(v=c(1,result$par),lty=4,col=2:3)
title("n=5000")

