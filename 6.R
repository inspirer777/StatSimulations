set.seed(143)
n = 30
y<-rbinom(n,1,.5)
y
Bernuli<-function(p,x){
  f<-0
  for(i in 1:length(p)){
    f[i]<-sum(x*log(p[i])+(1-x)*log(1-p[i]))
    
  }
  return(f)
}


p<-(c(.3,.8,.1))
Bernuli(p,y)
curve(Bernuli(x,y),0,1)
abline(v = .5,lty = 2,col = "red")
