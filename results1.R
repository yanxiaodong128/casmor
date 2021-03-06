#######################results########################
pdf(paste(c("death_rate1",".pdf"),collapse=""))
par(mfrow=c(1,1))
plot(Data[1,,1],Hatpi[1,],type="l",col=2,xlab=expression("age"),ylab=expression(paste(hat(pi),"(","age",")")),ylim=c(0,1),lwd=2)
for(j in 2:narea){
lines(Data[j,,1],Hatpi[j,],type="l",col=j+1,lwd=2)
}
legend("topleft",area, lty=rep(1,narea),lwd=rep(2,narea),col=2:(narea+1))
dev.off()





combnarea<-combn(area,2)
Factor<-factors(ncol(combnarea))
hang<-Factor[floor(length(Factor)/2)+1]
lie<-ncol(combnarea)/hang


for(j in 1:ncol(combnarea)){
pdf(paste(c(combnarea[1,j],"-",combnarea[2,j],".pdf"),collapse=""))
par(mfrow=c(1,1))

nhatp<-c(which(area==combnarea[1,j]),which(area==combnarea[2,j]))
NH<-which.min(apply(Hatpi[nhatp,],1,sum))
if(max(Hatpi[nhatp[NH],]/Hatpi[nhatp[setdiff(1:2,NH)],])<max(Hatpi[nhatp[setdiff(1:2,NH)],]/Hatpi[nhatp[NH],])){
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
}else{
hatpiratio2<-Hatpi[nhatp[setdiff(1:2,NH)],]/Hatpi[nhatp[NH],]
a<-hatpiratio2-c(0,hatpiratio2[1:(length(hatpiratio2)-1)])
a1<-hatpiratio2-1
b1<-a1[1:(length(a1)-1)]*a1[2:length(a1)]
zhuanzhe1<-which(b1<0)
b<-a[1:(length(a)-1)]*a[2:length(a)]
zhuanzhe<-which(b<0)
plot(0:99,hatpiratio2,type="l",col=2,xlab=expression("age"),ylab=
expression(paste(hat(pi)[a],"(","age",")","/",hat(pi)[b],"(","age",")")),lwd=2,main=substitute(paste("a=",b,",","b=",a),list(a=area[nhatp[NH]],b=area[nhatp[setdiff(1:2,NH)]])),ylim=c(0,max(hatpiratio2)))
if(length(zhuanzhe)>0){
points(zhuanzhe, hatpiratio2[zhuanzhe],col=5,lwd=2)
text(zhuanzhe, hatpiratio2[zhuanzhe], paste("age=",zhuanzhe),cex =1,col=4,lwd=2)
}
if(length(zhuanzhe1)>0){
points(zhuanzhe1, rep(1,length(zhuanzhe1)),col=5,lwd=2)
text(zhuanzhe1, rep(1,length(zhuanzhe1)), paste("age=",zhuanzhe1),cex =1,col=4,lwd=2)
}
}
lines(0:100,rep(1,101),type="l",col=3,lwd=2)

dev.off()
}


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



t1<-0
t2<-44
sa<-1
a1<-integrate(louzhen_integrand,GAM,k=2,kk=sa,Kernel=Kernel,l=2,  lower =age[t1+1],
upper =age[t2+1])$value
b1<-integrate(function(y)densit(y,Kernel,2), lower =age[t1+1], upper =age[t2+1])$value
print(1-(b1/a1))


t1<-45
t2<-81
sa<-1
a1<-integrate(louzhen_integrand,GAM,k=2,kk=sa,Kernel=Kernel,l=2,  lower =age[t1+1],
upper =age[t2+1])$value
b1<-integrate(function(y)densit(y,Kernel,2), lower =age[t1+1], upper =age[t2+1])$value
print(1-(b1/a1))


t1<-82
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


###############whole CFR Table 2##############
crf<-array(0,dim=c(2,narea))
for(k in 1:narea){
crf[2,k]<-integrate(integrand,GAM,k=k,Kernel,l=k,   lower =min(age), upper =max(age))$value
crf[1,k]<-sum(Data[k,,2])/sum(Data[k,,3]) 
}
round(crf,4)



###############MSE Table 1##############
cfr<-array(0,dim=c(narea,100))
for(k in 1:narea){
for(j in 1:length(age)){
cfr[k,j]<-Hatpi[k,j]-Data[k,j,2]/Data[k,j,3]
}
}
round(apply(cfr^2,1,mean),4)



##################ratio  Outside-of-Hubei##################################
##################ratio  Outside-of-Hubei##################################
##################ratio  Outside-of-Hubei##################################
##################ratio  Outside-of-Hubei##################################
##################ratio  Outside-of-Hubei##################################
for(j in 2:7){
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


##################ratio  Outside-of-Newyork##################################

