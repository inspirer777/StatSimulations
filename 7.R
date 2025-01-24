####   Beta   ####
set.seed(143)
a=2
f=1
Beta<-function(b,y){
  for(i in 1:length(b)){
    f[i]<-prod((gamma(a+b[i])/(gamma(a)*gamma(b[i])))*(y^(a-1)*(1-y))^(b[i]-1))
  }
  return(f)
}

y<-rbeta(3,0.1,0.2)
y
b<-c(1:3)
Beta(b,y)
curve(Beta(x,y),0,1)

##  log beta ###

log.Beta<-function(b,y){
  for(i in 1:length(b)){
    f[i]<-sum(log(gamma(a+b[i]))-(log(gamma(a))+log(gamma(b[i]))+(b[i]-1)*(log((a-1)*(y)+log(1-y))
  }
  return(f)
}

y<-rbeta(3,0.1,0.2)
y
b<-c(1:3)
log.Beta(b,y)
curve(log.Beta(x,y),0,1)
