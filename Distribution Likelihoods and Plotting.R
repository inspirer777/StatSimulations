###### Exp #####
Exp<-function(lambda,y){
  set.seed(143)
  f=0
  y<-rexp(10,4)
  
  for (i in 1:length(lambda)){
    f[i]<-prod(lambda[i]*exp(-lambda[i]*y))
    
  }
  return(f)
}
lambda<-c(1:8)
Exp(lambda)
curve(Exp(x,y),1,10)

###### Binomial ######

binomial<-function(p,y){
  set.seed(143)
  f<-1
  y<-rbinom(10,2,1)
  
  for(i in 1:length(p)){
    f[i]<-prod(choose(n,y)(p[i]^y)((1-p[i])^(n-y)))
    
  }
  return(f)
}
p<-c(2,4,6)
n<-20
binomial(p,y)
curve(binomial(x,y),8,10)


######    chisq     ######

chisq<-function(df,y){
  set.seed(143)
  f=1
  df<-rchisq(100,25)
  for(i in 1:length(df)){
    f[i]<-prod(1/(2^(df[i]/2)*gamma(df[i]/2))*y^(df[i]/2-1)*exp(-y/2))
  }
  return(f)
}
n<-100
y<-c(1:10)
y
chisq(df,y)
curve(chisq(x,y),25,100)



####   Beta   ####


Beta<-function(b,y){
  set.seed(143)
  f=1
  a=2
  y<-rbeta(3,0.1,0.2)
  
  for(i in 1:length(b)){
    f[i]<-prod((gamma(a+b[i])/(gamma(a)gamma(b[i])))(y^(a-1)*(1-y))^(b[i]-1))
  }
  return(f)
}

b<-c(1:3)
Beta(b,y)
curve(Beta(x,y),0,1)


### cauchy  ####


cauchy<-function(theta){
  set.seed(143)
  f<-0
  y<-rcauchy(10,5,1)
  for(i in 1:length(theta)){
    f[i]<-prod(1/(3.14*sigma)*(1+((y-theta[i])/sigma^2)))
  }
  return(f)
}
sigma<-.1
n<-100
theta<-c(1:3)
cauchy(theta,y)
curve(cauchy(x,y),0,1)


####  Uniform  ####


t<- function(x){
  set.seed(143)
  f<-0
  
  for(i in 1:lenght(x)){
    f[i]<-prod(1/max[i]-min[i])
    
  }
  return(f)
}
n<-5
x<-runif(5,min = 10, max = 15)

x
runif(x)
curve(runif(x),0.25,0.30)
