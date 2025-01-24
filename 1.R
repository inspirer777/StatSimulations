##########################
#### package AR
##########################
# install.packages("AR")
library(AR)

# Example 1:f(x)=beta(2,2) ? 0<x<1
data = AR.Sim( n = 100, f_X = function(y){dbeta(y,2,2)}, Y.dist = "unif", Y.dist.par = c(0,1), Rej.Num = TRUE, Rej.Rate = TRUE, Acc.Rate = FALSE )
# QQ-plot
q <- qbeta(ppoints(100), 2, 2)
qqplot(q, data, cex=0.6, xlab="Quantiles of Beta(2,2)",
       ylab="Empirical Quantiles of simulated data")
abline(0, 1, col=2)


# Example 2:f(x)=beta(2,2) ? 0<x<1,  g(x)=U(0,0.5)
data1 = AR.Sim( n = 100, f_X = function(y){dbeta(y,2,2)}, S_X=c(0,1),Y.dist = "unif", Y.dist.par = c(0,0.5), Rej.Num = TRUE, Rej.Rate = TRUE, Acc.Rate = FALSE )
# QQ-plot
q <- qbeta(ppoints(100), 2, 2)
qqplot(q, data1, cex=0.6, xlab="Quantiles of Beta(2,2)",
       ylab="Empirical Quantiles of simulated data")
abline(0, 1, col=2)

# Example 3:f(x)=beta(2,2) ? 0<x<1   g(x)=N(0.5,0.3)
data2= AR.Sim( n = 100, f_X = function(y){dbeta(y,2,2)}, Y.dist = "norm", Y.dist.par = c(.5,.3), Rej.Num = TRUE, Rej.Rate = TRUE, Acc.Rate = FALSE )
# QQ-plot
q <- qbeta(ppoints(100), 2, 2)
qqplot(q, data2, cex=0.6, xlab="Quantiles of Beta(2,2)",
       ylab="Empirical Quantiles of simulated data")
abline(0, 1, col=2)


## 

#Example2: f(x)=1/2 exp(-|x|)   -inf<x<inf
f<-function(x) 1/2*exp(-abs(x))

#g?

curve(f,-6,6,col=4) 
curve(dnorm,-6,6,col=2,add=T)
curve(dt(x,3),-6,6,col=6,add=T)

#g=U(-6,6)
data1 = AR.Sim( n = 100, f_X = function(x) {1/2*exp(-abs(x))}, Y.dist = "unif", Y.dist.par = c(-6,6), xlim=c(-6,6),Rej.Num = TRUE, Rej.Rate = TRUE, Acc.Rate = FALSE )
# Checking if it went well
hist(data1, breaks=30, freq=FALSE, main="Sample vs true Density")
curve(f,-6,6,col="Orange1",add=TRUE) 


#g=U(-10,10)
data2 = AR.Sim( n = 100, f_X = function(x) {1/2*exp(-abs(x))}, Y.dist = "unif", Y.dist.par = c(-10,10), xlim=c(-10,10),Rej.Num = TRUE, Rej.Rate = TRUE, Acc.Rate = FALSE )
# Checking if it went well
hist(data2, breaks=30, freq=FALSE, main="Sample vs true Density")
curve(f,-6,6,col="Orange1",add=TRUE) 


#g=N(0,10)
data3 = AR.Sim( n = 100, f_X = function(x) {1/2*exp(-abs(x))}, Y.dist = "norm", Y.dist.par = c(0,10), xlim=c(-6,6),Rej.Num = TRUE, Rej.Rate = TRUE, Acc.Rate = FALSE )
# Checking if it went well
hist(data3, breaks=30, freq=FALSE, main="Sample vs true Density")
curve(f,-10,10,col="Orange1",add=TRUE) 



#g=t(3)
data4 = AR.Sim( n = 1000, f_X = function(x) {1/2*exp(-abs(x))}, Y.dist = "t", Y.dist.par = 3, xlim=c(-10,10),Rej.Num = TRUE, Rej.Rate = TRUE, Acc.Rate = FALSE )
# Checking if it went well
hist(data4, breaks=30, freq=FALSE, main="Sample vs true Density")
curve(f,-10,10,col="Orange1",add=TRUE) 

######################################################################
#######  class 22 #################
#####################################################################




# Example:given two standard normal iid random vars X1,X2 estimate E[|X1-X2|]
# Monte Carlo
n <- 1e5  # is 10000
X1 <- rnorm(n,0,1)
X2 <- rnorm(n,0,1)

thetas <-abs(X1-X2)
mean(thetas)

mean(abs(X1-X2))

mean(X1)
mean(X2)
var(X1)
# standard error = sqrt( var(|X1-X2|)/n )
sqrt(var(thetas)/n)


