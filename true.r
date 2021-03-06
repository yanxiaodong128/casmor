
###############define space matrix##################
area<-c("non-Hubei","Wuhan","Italy","Spain","Germany","France","Korea","Canada","America","New York","non-New York")
narea<-length(area)
Data<-array(0,dim=c(narea,100,3))
#quantil<-c(1/2)
#knots=quantile(0:99,quantil)
#a<-bs(0:99, knots=knots, Boundary.knots=c(0, 99), degree=1, intercept=T)
#a<-array(0,dim=c(100,4))
age<-0:99
#for(j in 1:ncol(a)){
##a[,j]<-(scaleage)^j
#}
#a<-apply(a,2,scale)
#Theta<-array(0,dim=c(narea,ncol(a)+1))
Hatpi<-array(0,dim=c(narea,100))
Kernellnorm<-array(0,dim=c(narea,2))

GAM<-list()
Kernel<-list()
