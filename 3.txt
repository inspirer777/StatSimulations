##############################################
# likelihood function of Poisson distribution
#f(x,lambda)=exp(-lambda)*lambda^(x)/factorial(x)
set.seed(143)
y<-rpois(10,4)
y


Pois<-function(lambda,y){
	f<-0
	for(i in 1:length(lambda)){
		f[i]<-prod(exp(-lambda[i])*lambda[i]^(y)/factorial(y))
	}
	return(f)
}
lambda<-c(1,2,3)
Pois(lambda,y)

curve(Pois(x,y),1,10)

### log poisson


log.Pois<-function(lambda,y){
	f<-0
	for(i in 1:length(lambda)){
		f[i]<-sum(-lambda[i]+y*log(lambda[i])-log(factorial(y)))
	}
	return(f)
}
lambda<-c(1,2,3)
log.Pois(lambda,y)

par(mfrow=c(1,2))
curve(Pois(x,y),1,10)
curve(log.Pois(x,y),1,10)

