#############################################
# class 8
##############################################

################################
# gamma distribution Likelihood
###############################
# f(x)= 1/(beta^alpha*Gamma(alpha))* x^(alpha-1)*e^-(x/beta)  ---  x~ G(alpha , beta)
# E(X)=alpha*beta

# beta is fixed in one (beta=1)
L.Gamma <- function(alpha){
	beta=1
	n=100
	set.seed(143)
	y<-rgamma(n,10,1)
	f<-0
	for(i in 1:length(alpha)){
	f[i]<-prod(1/(beta^alpha[i]*gamma(alpha[i]))*y^(alpha[i]-1)*exp(-y/beta))
	}
	return(f)
}
	
alpha<-c(1,4,7)
L.Gamma(alpha)

curve(L.Gamma(x),1,20)
	

# log likelihood of gamma distribution
# logf(x)=-alpha*log(beta)-log(gamma(alpha))+(alpha-1)*log(x)-x/beta

# beta is fixed in one (beta=1)
Log.Gamma <- function(alpha){
	beta=1
	n=100
	set.seed(143)
	y<-rgamma(n,10,1) # real alpha is 10
	f<-0
	for(i in 1:length(alpha)){
	f[i]<-sum(-alpha[i]*log(beta)-log(gamma(alpha[i]))+(alpha[i]-1)*log(y)-y/beta)
	}
	return(f)
}
	
alpha<-c(1,4,7)
Log.Gamma(alpha)

curve(Log.Gamma(x),1,20)

par(mfrow=c(1,2))
curve(Log.Gamma(x),5,15,xlab="alpha")
title("Log likelihood of gamma distribution")
text(9,-110,"real alpha",col=2,cex=.8)
abline(v=10,lty=2,col=2,lwd=1.5)
curve(L.Gamma(x),5,15,xlab="alpha")
title("Likelihood of gamma distribution")
text(9,5e-34,"real alpha",col=2,cex=.8)
abline(v=10,lty=2,col=2,lwd=1.5)


# minimize logf
a1<-optimize(function(alpha) (-1)*L.Gamma(alpha), interval=c(9,12))
a1
a2<-optimize(function(alpha) (-1)*Log.Gamma(alpha), interval=c(9,12))

alphahat1<-a1$minimum
alphahat2<-a2$minimum

optim(9.5,function(alpha) (-1)*Log.Gamma(alpha),method="Brent",lower=9,upper=11)

optim(9.5,function(alpha) (-1)*Log.Gamma(alpha),method="BFGS")

optim(9.5,function(alpha) (-1)*Log.Gamma(alpha),method="Nelder-Mead")

optim(9.5,function(alpha) (-1)*Log.Gamma(alpha),method="CG")

optim(9.5,function(alpha) (-1)*Log.Gamma(alpha),method="L-BFGS-B",lower=9,upper=11) 

optim(9.5,function(alpha) (-1)*Log.Gamma(alpha),method="SANN")


par(mfrow=c(1,2))
curve(Log.Gamma(x),5,15,xlab="alpha")
title("Log likelihood of gamma distribution")
text(9,-110,"real alpha",col=2,cex=.8)
text(11.5,-110,"alphahat",col=4,cex=.8)
abline(v=10,lty=2,col=2,lwd=1.5)
abline(v=alphahat2,,lty=3,col=4,lwd=1.5)
curve(L.Gamma(x),5,15,xlab="alpha")
title("Likelihood of gamma distribution")
text(9,5e-34,"real alpha",col=2,cex=.8)
text(11.5,5e-34,"alphahat",col=4,cex=.8)
abline(v=10,lty=2,col=2,lwd=1.5)
abline(v=alphahat1,,lty=3,col=4,lwd=1.5)




