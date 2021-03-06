setwd("C:/Users/yanxi/Documents/我的坚果云/yanxd/杨书/疫情数据/kernel_spline20200730/kernel_spline20200420")
library(openxlsx)
library(splines)
library(MASS)
library(xtable)
library(mgcv)
library(plyr)
library(ggplot2)
library(gcookbook)
library(broom)
library(plotly)
library(reshape)
source("true.r")
source("function.r")
source("readdata.r")
source("estimation.r")


#######################Figure 2########################
#######################Figure 2########################
#######################Figure 2########################
#######################Figure 2########################
#######################Figure 2########################
dataset <- data.frame(age = 0:99,Wuhan=round(Hatpi[2,],2), non_Hubei=round(Hatpi[1,],2), Italy=round(Hatpi[3,],2), Spanish=round(Hatpi[4,],2),Germany=round(Hatpi[5,],2), France=round(Hatpi[6,],2), Korea=round(Hatpi[7,],2), Canada=round(Hatpi[8,],2),
America=round(Hatpi[9,],2),Newyork=round(Hatpi[10,],2))
#convert data to long format
library(reshape)
Molten <- melt(dataset, id.vars = "age")
colnames(Molten)<-c("age","area","value")
ggplot(Molten, aes(x =age, y = value, colour = area))  +coord_cartesian(ylim=c(0,1))+ 
    geom_smooth(se=FALSE)+ylab(expression(paste(hat(pi),"(","age",")")))


j<-6
aa<-predict(GAM[[j]], se.fit=TRUE) 
fit<-exp(aa$fit)/(1+exp(aa$fit))
se.fit<- (aa$se.fit)*exp(aa$fit)/(1+exp(aa$fit))^2
logitData<-logitdata(Data[j,,])
logitData<-logitdata(Data[j,,])
colnames(logitData)<-c("y","age")
logitData<-as.data.frame(logitData) 

plotdata <- data.frame(age=logitData[,2], fit=fit, lower =fit-se.fit, upper = fit+se.fit)
p <- ggplot(plotdata)+labs(title =area[j], x = "age", y =" ")+geom_line(aes(y=fit, x=age, colour ="CFR function"))+geom_ribbon(aes(ymin=lower, ymax=upper, x=age, fill = "band"), alpha = 0.3)+
    scale_colour_manual("",values="blue")+
    scale_fill_manual("",values="grey12")
fig <- ggplotly() 
fig



###############whole CFR Table 2##############
###############whole CFR Table 2##############
###############whole CFR Table 2##############
###############whole CFR Table 2##############
crf<-array(0,dim=c(2,narea))
for(k in 1:narea){
crf[2,k]<-integrate(integrand,GAM,k=k,Kernel,l=k,   lower =min(age), upper =max(age))$value
crf[1,k]<-sum(Data[k,,2])/sum(Data[k,,3]) 
}
round(crf,4)



###############Figure 1##############
###############Figure 1##############
###############Figure 1##############
# dev.new()
pdf('figure1.pdf', width = 8, height = 16)
cfr = c(0.0094, 0.0516, 0.1242, 0.0777, 0.0247, 0.1527, 0.0209 ,0.0316,  0.0406, 0.0609 )
ratio = c(1.2, 1.035, 1.35, 1.55,1.63,1.56,1.05,1.75,1.15,1.25)
par(mfrow=c(5,2))
for(k in 1:(narea-1)){
  # k<-7
  dieage<-rep(0,sum(Data[k,,2]))
  index<-which(Data[k,,2]!=0)
  indexdienumber<-cumsum(Data[k,index,2] )
  cuindex<-c(0,indexdienumber)
  for(j in 1:length(index)){
    dieage[(cuindex[j]+1):(cuindex[j+1])]<-Data[k,index[j],1] 
  }
  
  dieage1<-rep(0,sum(Data[k,,3]))
  index1<-which(Data[k,,3]!=0)
  indexdienumber1<-cumsum(Data[k,index1,3] )
  cuindex1<-c(0,indexdienumber1)
  for(j in 1:length(index1)){
    dieage1[(cuindex1[j]+1):(cuindex1[j+1])]<-Data[k,index1[j],1] 
  }
  df1<-data.frame(x=dieage1)
  
  a <-data.frame(age=dieage, Category=rep('die', length(dieage)))
  b <-data.frame(age=dieage1, Category=rep('confirmed', length(dieage1)))
  many_distros <- do.call('rbind', list(a,b))
  #options(repr.plot.width = 1, repr.plot.height =6)
  #windows.options(width=10, height=6)
  # dev.off()
# windows.options(reset=TRUE)
  # plot_multi_histogram(many_distros,area,k,'age', 'Category')
  plot_multi_histogram1(many_distros,area,k,'age', 'Category',cfr, ratio)
}
dev.off()

##################ratio  nonHubei Figure 4##################################
combnarea<-combn(area,2)
for(j in 2:9){
pdf(paste(c(combnarea[1,j],"-",combnarea[2,j],".pdf"),collapse=""))
par(mfrow=c(1,1))

nhatp<-c(which(area==combnarea[1,j]),which(area==combnarea[2,j]))
NH<-2
#ssh<-Hatpi[nhatp[setdiff(1:2,NH)],]
#chabu<-mean(ssh[c(min(which(is.na(ssh)==TRUE))-1,max(which(is.na(ssh)==TRUE))+1)])
#Hatpi[nhatp[setdiff(1:2,NH)],which(is.na(ssh)==TRUE)]<-chabu
hatpiratio1<-Hatpi[nhatp[NH],]/Hatpi[nhatp[setdiff(1:2,NH)],]
a<-hatpiratio1-c(0,hatpiratio1[1:(length(hatpiratio1)-1)])
a1<-hatpiratio1-1
b<-a[1:(length(a)-1)]*a[2:length(a)]
b1<-a1[1:(length(a1)-1)]*a1[2:length(a1)]
zhuanzhe<-which(b<0)
zhuanzhe1<-which(b1<0)
plot(0:99,hatpiratio1,type="l",col=2,xlab=expression("age"),ylab=
expression(paste(hat(pi)[a],"(","age",")","/",hat(pi)[b],"(","age",")")),lwd=2,main=substitute(paste("a=",a,",","b=",b),list(a=area[nhatp[NH]],b=area[nhatp[setdiff(1:2,NH)]])),ylim=c(0,max(hatpiratio1)))
if(length(zhuanzhe)>0){
points(zhuanzhe, hatpiratio1[zhuanzhe],col=5,lwd=2)
text(zhuanzhe, hatpiratio1[zhuanzhe], paste("age=",zhuanzhe),cex =1,col=4,lwd=2)
}
if(length(zhuanzhe1)>0){
points(zhuanzhe1, rep(1,length(zhuanzhe1)),col=5,lwd=2)
text(zhuanzhe1, rep(1,length(zhuanzhe1)), paste("age=",zhuanzhe1),cex =1,col=4,lwd=2)
}

lines(0:100,rep(1,101),type="l",col=3,lwd=2)

dev.off()
}


##################ratio  Wuhan Figure 3##################################
area1<-c("Wuhan","non-Hubei","Italy","Spain","Germany","France","Korea","Canada","America","New York")
combnarea<-combn(area1,2)
for(j in 1:9){
pdf(paste(c(combnarea[1,j],"-",combnarea[2,j],".pdf"),collapse=""))
par(mfrow=c(1,1))

nhatp<-c(which(area==combnarea[1,j]),which(area==combnarea[2,j]))
NH<-2
hatpiratio1<-Hatpi[nhatp[NH],]/Hatpi[nhatp[setdiff(1:2,NH)],]
a<-hatpiratio1-c(0,hatpiratio1[1:(length(hatpiratio1)-1)])
a1<-hatpiratio1-1
b<-a[1:(length(a)-1)]*a[2:length(a)]
b1<-a1[1:(length(a1)-1)]*a1[2:length(a1)]
zhuanzhe<-which(b<0)
zhuanzhe1<-which(b1<0)
plot(0:99,hatpiratio1,type="l",col=2,xlab=expression("age"),ylab=
expression(paste(hat(pi)[a],"(","age",")","/",hat(pi)[b],"(","age",")")),lwd=2,main=substitute(paste("a=",a,",","b=",b),list(a=area[nhatp[NH]],b=area[nhatp[setdiff(1:2,NH)]])),ylim=c(0,max(hatpiratio1)))
if(length(zhuanzhe)>0){
points(zhuanzhe, hatpiratio1[zhuanzhe],col=5,lwd=2)
text(zhuanzhe, hatpiratio1[zhuanzhe], paste("age=",zhuanzhe),cex =1,col=4,lwd=2)
}
if(length(zhuanzhe1)>0){
points(zhuanzhe1, rep(1,length(zhuanzhe1)),col=5,lwd=2)
text(zhuanzhe1, rep(1,length(zhuanzhe1)), paste("age=",zhuanzhe1),cex =1,col=4,lwd=2)
}

lines(0:100,rep(1,101),type="l",col=3,lwd=2)

dev.off()
}


##################Table 1##################################
 ##################Table 1##################################
 ##################Table 1##################################
Kernellnorm
Lossratio<-array(0,dim=c(4*ncol(combnarea),5))
for(j in 1:ncol(combnarea)){
JJ<-((j-1)*4+1):(j*4)
nhatp<-c(which(area==combnarea[1,j]),which(area==combnarea[2,j]))
for(k in 1:length(nhatp)){


for(l in 1:length(nhatp)){
Lossratio[JJ[2*(k-1)+l],1:3]<-c(area[nhatp[k]],area[nhatp[l]],area[nhatp[setdiff(1:2,l)]])
Lossratio[JJ[2*(k-1)+l],4]<-round(integrate(integrand,GAM,nhatp[l],mu=Kernellnorm[nhatp[k],1],sigma=Kernellnorm[nhatp[k],2],  lower =0, upper =99)$value/integrate(integrand,GAM,nhatp[setdiff(1:2,l)],mu=Kernellnorm[nhatp[k],1],sigma=Kernellnorm[nhatp[k],2],  lower =0, upper =99)$value,3)
a<- as.numeric(Lossratio[JJ[2*(k-1)+l],4])


if(a>1){
Lossratio[JJ[2*(k-1)+l],5]<-round((a-1)/a,3)
}

}
}
}
Lossratio[which(Lossratio[,5]==0),5]<-c("*")
colnames(Lossratio)<-c("a","b","c","H","LR")
print(xtable(Lossratio))

###############MSE Table 1##############
cfr<-array(0,dim=c(narea,100))
for(k in 1:narea){
for(j in 1:length(age)){
cfr[k,j]<-Hatpi[k,j]-Data[k,j,2]/Data[k,j,3]
}
}
round(apply(cfr^2,1,mean),4)



###############Table 3##############
###############Table 3##############
##Wuhan 漏诊率
t1<-0
t2<-45
sa<-1
a1<-integrate(louzhen_integrand,GAM,k=2,kk=sa,Kernel=Kernel,l=2,  lower =age[t1+1],
upper =age[t2+1])$value
b1<-integrate(function(y)densit(y,Kernel,2), lower =age[t1+1], upper =age[t2+1])$value
print(1-(b1/a1))


t1<-46
t2<-84
sa<-1
a1<-integrate(louzhen_integrand,GAM,k=2,kk=sa,Kernel=Kernel,l=2,  lower =age[t1+1],
upper =age[t2+1])$value
b1<-integrate(function(y)densit(y,Kernel,2), lower =age[t1+1], upper =age[t2+1])$value
print(1-(b1/a1))


t1<-85
t2<-99
sa<-1
a1<-integrate(louzhen_integrand,GAM,k=2,kk=sa,Kernel=Kernel,l=2,  lower =age[t1+1],
upper =age[t2+1])$value
b1<-integrate(function(y)densit(y,Kernel,2), lower =age[t1+1], upper =age[t2+1])$value
print(1-(b1/a1))


 t1<-0
t2<-99
sa<-1
a1<-integrate(louzhen_integrand,GAM,k=2,kk=sa,Kernel=Kernel,l=2,  lower =age[t1+1],
upper =age[t2+1])$value
b1<-integrate(function(y)densit(y,Kernel,2), lower =age[t1+1], upper =age[t2+1])$value
print(1-(b1/a1))


###############Table 3##############
###############Table 3##############
##NewYork 漏诊率
area1<-c("non-New York","New York")
combnarea<-combn(area1,2)
j<-1
nhatp<-c(which(area==combnarea[1,j]),which(area==combnarea[2,j]))
NH<-2
hatpiratio1<-Hatpi[nhatp[NH],]/Hatpi[nhatp[setdiff(1:2,NH)],]
a<-hatpiratio1-c(0,hatpiratio1[1:(length(hatpiratio1)-1)])
a1<-hatpiratio1-1
b<-a[1:(length(a)-1)]*a[2:length(a)]
b1<-a1[1:(length(a1)-1)]*a1[2:length(a1)]
zhuanzhe<-which(b<0)
zhuanzhe1<-which(b1<0)
plot(0:99,hatpiratio1,type="l",col=2,xlab=expression("age"),ylab=
expression(paste(hat(pi)[a],"(","age",")","/",hat(pi)[b],"(","age",")")),lwd=2,main=substitute(paste("a=",a,",","b=",b),list(a=area[nhatp[NH]],b=area[nhatp[setdiff(1:2,NH)]])),ylim=c(0,max(hatpiratio1)))
if(length(zhuanzhe)>0){
points(zhuanzhe, hatpiratio1[zhuanzhe],col=5,lwd=2)
text(zhuanzhe, hatpiratio1[zhuanzhe], paste("age=",zhuanzhe),cex =1,col=4,lwd=2)
}
if(length(zhuanzhe1)>0){
points(zhuanzhe1, rep(1,length(zhuanzhe1)),col=5,lwd=2)
text(zhuanzhe1, rep(1,length(zhuanzhe1)), paste("age=",zhuanzhe1),cex =1,col=4,lwd=2)
}

lines(0:100,rep(1,101),type="l",col=3,lwd=2)


t1<-0
t2<-24
sa<-11
a1<-integrate(louzhen_integrand,GAM,k=10,kk=sa,Kernel=Kernel,l=10,  lower =age[t1+1],
upper =age[t2+1])$value
b1<-integrate(function(y)densit(y,Kernel,10), lower =age[t1+1], upper =age[t2+1])$value
print(1-(b1/a1))


t1<-25
t2<-36
sa<-11
a1<-integrate(louzhen_integrand,GAM,k=10,kk=sa,Kernel=Kernel,l=10,  lower =age[t1+1],
upper =age[t2+1])$value
b1<-integrate(function(y)densit(y,Kernel,10), lower =age[t1+1], upper =age[t2+1])$value
print(1-(b1/a1))


t1<-37
t2<-45
sa<-11
a1<-integrate(louzhen_integrand,GAM,k=10,kk=sa,Kernel=Kernel,l=10,  lower =age[t1+1],
upper =age[t2+1])$value
b1<-integrate(function(y)densit(y,Kernel,10), lower =age[t1+1], upper =age[t2+1])$value
print(1-(b1/a1))

t1<-45
t2<-68
sa<-11
a1<-integrate(louzhen_integrand,GAM,k=10,kk=sa,Kernel=Kernel,l=10,  lower =age[t1+1],
upper =age[t2+1])$value
b1<-integrate(function(y)densit(y,Kernel,10), lower =age[t1+1], upper =age[t2+1])$value
print(1-(b1/a1))

 t1<-69
t2<-99
sa<-11
a1<-integrate(louzhen_integrand,GAM,k=10,kk=sa,Kernel=Kernel,l=10,  lower =age[t1+1],
upper =age[t2+1])$value
b1<-integrate(function(y)densit(y,Kernel,10), lower =age[t1+1], upper =age[t2+1])$value
print(1-(b1/a1))

t1<-0
t2<-99
sa<-11
a1<-integrate(louzhen_integrand,GAM,k=10,kk=sa,Kernel=Kernel,l=10,  lower =age[t1+1],
upper =age[t2+1])$value
b1<-integrate(function(y)densit(y,Kernel,10), lower =age[t1+1], upper =age[t2+1])$value
print(1-(b1/a1))




###############whole CFR Table 2#############################whole CFR Table 2##############
###############whole CFR Table 2############## ###############whole CFR Table 2##############
crf<-array(0,dim=c(2,narea))
for(k in 1:narea){
crf[2,k]<-integrate(integrand,GAM,k=k,Kernel,l=k,   lower =min(age), upper =max(age))$value
crf[1,k]<-sum(Data[k,,2])/sum(Data[k,,3]) 
}
round(crf,4)




############Wuhan and others SMR##########
############Wuhan and others SMR##########
############Wuhan and others SMR##########
SMR_Wuhan<-c()
for(jk in 1:narea){
WI<-round(integrate(integrand,GAM,k=jk,Kernel=Kernel,l=2,   lower =min(age), upper =max(age))$value/integrate(integrand,GAM,k=2,Kernel=Kernel,l=2,   lower =min(age), upper =max(age))$value,3)
SMR_Wuhan[jk]<-WI
}
SMR_Wuhan
############Wuhan and others SMR##########
SMR_feihubei<-c()
for(jk in 1:narea){
WI<-round(integrate(integrand,GAM,k=jk,Kernel=Kernel,l=1,   lower =min(age), upper =max(age))$value/integrate(integrand,GAM,k=1,Kernel=Kernel,l=1,   lower =min(age), upper =max(age))$value,3)
SMR_feihubei[jk]<-WI
}
SMR_feihubei

