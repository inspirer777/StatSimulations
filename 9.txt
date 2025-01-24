########################################################
###### problem :
# Create likelihood functions by 15 random samples
# x is c(0.41,0.91,-0.61,0.38,0.37,0.36,0.01,-0.28,-0.33,0.99,-0.35,0.1,0.31,0.75,-0.34) 

#Example1: f(x)=(1+a*x)/2     -1<x<1    -1<a<1


:

answer 
   
x<- c(.41,.91,-.61,.38,.37,.36,.01,-.28,-.33,.99,-.35,.1,.31,.75,-.34)

 f<- function(a){
   z<-0
   for(i in 1:length(a)){
     z[i]<-prod((1+a[i]*x)/2)
     
   }
   return(z)
 }
 

 f(c(-.5,.5,.7))
 curve(f(x),.5,1)
  abline(v = ".8",lty = 2,col = 2)





