
#############################################
# class 9
##############################################
## two parameters , computing likelihood

# Example: X~gamma(alpha , beta) , we have two parameters as (alpha, beta)
# log likelihood of gamma distribution
# logf(x)=-alpha*log(beta)-log(gamma(alpha))+(alpha-1)*log(x)-x/beta

Log.Gamma2<- function(alpha,beta){
	n=100
	set.seed(143)
	y<-rgamma(n,10,1) 
	f<-matrix(0,ncol=length(beta),nrow=length(alpha))
	for(i in 1:length(alpha)){
		for(j in 1:length(beta)){
		 f[i,j]<-sum(-alpha[i]*log(beta[j])-log(gamma(alpha[i]))+(alpha[i]-1)*log(y)-y/beta[j])
		}
	}
	return(f)
}
	
alpha<-c(1,4,7)
beta<-c(1,5,8,10)
Log.Gamma2(alpha,beta)

curve(Log.Gamma2(x,beta=1),1,20)

par(mfrow=c(2,2))
curve(Log.Gamma2(x,beta=1),1,20)
curve(Log.Gamma2(x,beta=2),1,20)
curve(Log.Gamma2(x,beta=4),1,20)
curve(Log.Gamma2(x,beta=.5),1,20)


#ploting surface
# by persp and image
alpha<-seq(1,25,l=20)
beta<-seq(.1,5,l=20)

f<-Log.Gamma2(alpha,beta)

persp(alpha,beta,f)

par(mfrow=c(1,2))
persp(alpha, beta, f, theta = 45, phi = 25,r=2,col = "springgreen", shade = 0.5, nticks=5, ticktype="detailed")

image(alpha, beta, f)
contour(alpha, beta, f,add=T)

### optim is general optimization
## nlm is Non-Linear Minimization by a Newton-type algorithm 
## mle library(stats4) is Maximum Likelihood Estimation
### mle is similar to optim

optim(c(1,1),function(x) (-1)*Log.Gamma2(alpha=x[1],beta=x[2]),method="Nelder-Mead")

nlm(function(x) (-1)*Log.Gamma2(alpha=x[1],beta=x[2]),c(1,1))

mle ?


#problem= beta distribution, F distribution , normal distribution

