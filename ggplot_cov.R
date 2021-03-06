###ggplot#


cov_DATA<-array(0,dim=c(10*narea,5))
dATA<-array(0,dim=c(narea,10,2))
for(j in 1:narea){
for(i in 1:10){
dATA[j,i,1]<-sum(Data[j,(((i-1)*10+1):(i*10)),2])
dATA[j,i,2]<-sum(Data[j,(((i-1)*10+1):(i*10)),3])
}
}

for(j in 1:narea){
cov_DATA[seq(j,nrow(cov_DATA),by=narea),1]<-c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90-99")
cov_DATA[seq(j,nrow(cov_DATA),by=narea),2]<-rep(area[j],10)
cov_DATA[seq(j,nrow(cov_DATA),by=narea),3]<-dATA[j,,1]
cov_DATA[seq(j,nrow(cov_DATA),by=narea),4]<-dATA[j,,2]
cov_DATA[seq(j,nrow(cov_DATA),by=narea),5]<-round(dATA[j,,1]/dATA[j,,2],2)
}
colname<-c("age","area","Death_number","Confirmed_number","Death_rate")
colnames(cov_DATA)<-colname
cov_Data<-as.data.frame(cov_DATA)

cov_Data[,5]<=round(cov_Data[,5],2)

#pdf(paste(c("Death_number",".pdf"),collapse=""))
#par(mfrow=c(1,1))
#ggplot(cov_Data[,c(1,2,3)],aes(x=age,y=Death_number,fill=area,order=desc(area)))+geom_bar(stat="identity")
#dev.off()

#pdf(paste(c("Confirmed_number",".pdf"),collapse=""))
#par(mfrow=c(1,1))
#ggplot(cov_Data[,c(1,2,4)],aes(x=age,y=Confirmed_number,fill=area,order=desc(area)))+geom_bar(stat="identity")
#dev.off()

#pdf(paste(c("Death_rate",".pdf"),collapse=""))
par(mfrow=c(1,1))
ggplot(cov_Data[,c(1,2,5)],aes(x=age,y=Death_rate,fill=area,order=desc(area)))+ylab("death rate")+geom_bar(stat="identity")
#dev.off()

######################病死率曲线图################################
j<-9
aa<-predict(GAM[[j]], se.fit=TRUE) 
fit<-exp(aa$fit)/(1+exp(aa$fit))
se.fit<- (aa$se.fit)*exp(aa$fit)/(1+exp(aa$fit))^2
logitData<-logitdata(Data[j,,])
logitData<-logitdata(Data[j,,])
colnames(logitData)<-c("y","age")
logitData<-as.data.frame(logitData) 

plotdata <- data.frame(age=logitData[,2], fit=fit, lower =fit-se.fit, upper = fit+se.fit)
p <- ggplot(plotdata)+labs(title =area[j], x = "age", y =" "    )+geom_line(aes(y=fit, x=age, colour ="CFR function"))+geom_ribbon(aes(ymin=lower, ymax=upper, x=age, fill = "band"), alpha = 0.3)+
    scale_colour_manual("",values="blue")+
    scale_fill_manual("",values="grey12")
fig <- ggplotly() 
fig

    
    


#################联合起来###########################
dataset <- data.frame(age = 0:99,Wuhan=round(Hatpi[2,],2),outside_of_Hubei=round(Hatpi[1,],2),  Italy=round(Hatpi[3,],2), Spanish=round(Hatpi[4,],2), 
America=round(Hatpi[5,],2),France=round(Hatpi[6,],2), Korea=round(Hatpi[7,],2),New_York=round(Hatpi[8,],2))
#convert data to long format
library(reshape)
Molten <- melt(dataset, id.vars = "age")
colnames(Molten)<-c("age","area","value")
ggplot(Molten, aes(x =age, y = value, colour = area)) + 
    geom_smooth(se=FALSE)+ylab(expression(paste(hat(pi),"(","age",")")))






library(itsadug)
gamtabs(GAM[[1]], caption='Summary of m1')
library(stargazer)
stargazer(GAM[[1]], type = "text", summary = T, colnames=T, rownames = T, df=T, digits=3)



#########louzhen###########
n1<-5
n2<-2
hatpiratio1<-Hatpi[n1,]/Hatpi[n2,]
plot(0:99,hatpiratio1,type="l",col=2,xlab=expression("age"),ylab=
expression(paste(hat(pi)[a],"(","age",")","/",hat(pi)[b],"(","age",")")),lwd=2,main=substitute(paste("a=",a,",","b=",b),list(a=area[n1],b=area[n2])),ylim=c(0,max(hatpiratio1)))
zhuanzhe<-c(67)
points(zhuanzhe, hatpiratio1[zhuanzhe],col=5,lwd=2)
text(zhuanzhe, hatpiratio1[zhuanzhe], paste("age=",zhuanzhe),cex =1,col=4,lwd=2)
lines(0:100,rep(1,101),type="l",col=3,lwd=2)


 #########死亡人数的频率直方图####################louzhen###########diedensity
 #########死亡人数的频率直方图####################louzhen###########diedensity
 #########死亡人数的频率直方图####################louzhen###########diedensity
k<-2
dieage<-rep(0,sum(Data[k,,2]))
index<-which(Data[k,,2]!=0)
indexdienumber<-cumsum(Data[k,index,2] )
cuindex<-c(0,indexdienumber)
for(j in 1:length(index)){
dieage[(cuindex[j]+1):(cuindex[j+1])]<-Data[k,index[j],1] 
}
df<-data.frame(x=dieage)
g<-ggplot(df,aes(x))+
      geom_histogram(binwidth = 2.5)+
      geom_density(aes(y=2.5 * ..count..))       
g+labs(title =area[k], x = "age", y ="die number ")




 #########多个直方图####################louzhen###########diedensity
 #########多个直方图####################louzhen###########diedensity
 #########多个直方图####################louzhen###########diedensity
plot_multi_histogram <- function(df,area,k,feature, label_column) {
    plt <- ggplot(df, aes(x=eval(parse(text=feature)), fill=eval(parse(text=label_column)))) +
        geom_histogram(alpha=0.7, position="identity", aes(y = ..density..), color="black") +
        geom_density(alpha=0.7) +
        geom_vline(aes(xintercept=mean(eval(parse(text=feature)))), color="black", linetype="dashed", size=1) +
        labs(title =area[k],x=feature, y = "Density")
    plt + guides(fill=guide_legend(title=label_column))
}



k<-1
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
options(repr.plot.width = 20, repr.plot.height = 8)
plot_multi_histogram(many_distros,area,k,'age', 'Category')



####################佳伟############################
plot_multi_histogram1 <- function(df,area,k,feature, label_column, cfr, ratio)
{
  cfr_ = cfr[k]
  ratio_ = ratio[k]
  par(mar=c(4.5,4.5,2,1), xaxs='i', yaxs='i')
  fac<-unique(df[[label_column]])
  p <- hist(df[[feature]], 
            breaks = seq(0,100,5),
            plot = F)
  p1<-hist(df[[feature]][which(df[[label_column]]==fac[1])], breaks = seq(0,100,5),
           xlim=c(0,100), ylim=c(0,max(p$counts)*1.1),
           col='#ff6666', axes=F, ann=F)
  Tdens<-list()
  for(i in 1:length(fac))
  {
    Tdens[[i]]<-density(df[[feature]][which(df[[label_column]]==fac[i])])
  }
  maxden <- max(unlist(lapply(Tdens, function(x) return(max(x$y)))))
  p2<-hist(df[[feature]][which(df[[label_column]]==fac[2])], breaks = seq(0,100,5),
           col='#6666ff77', axes=F, ann=F, add=T)
  
  axis(2, at=seq(0,ceiling(max(p$counts)*1.1),floor(max(p$counts)*1.1/5)), las=1)
  
  opar<-par(usr=c(0,100,0,maxden*1.1))
  dens<-density(df[[feature]][which(df[[label_column]]==fac[1])])
  if(fac[1] == 'die') dens$y <- dens$y * cfr_
  polygon(x = c(dens$x, dens$x[1]),
          y = c(dens$y*ratio_, dens$y[length(dens$y)])*ratio_,col = '#ff666677',lty = 1)
  
  dens<-density(df[[feature]][which(df[[label_column]]==fac[2])])
  polygon(x = c(dens$x, dens$x[1])  ,
          y = c(dens$y*ratio_, dens$y[length(dens$y)])*ratio_,col = '#6666ff33',lty = 1)
  par(opar)
  opar<-par(usr=c(0,100,0,maxden*1.1), cex=0.75)
  axis(1, at=seq(0,100,10))
  grid()
  # axis(2, at=seq(0,maxden*1.1,0.005), las=1)
  par(opar)
  # box()
  abline(v=mean(df[[feature]]), lty=2, lwd=2)
  mtext(text = 'age', side = 1,line = 2.5)
  mtext(text = 'Frequency', side = 2,line = 3.5)
  opar<-par(usr=c(0,100,0,maxden*1.1),xpd=NA)
  text(x = 0, y = maxden*1.12, adj = c(0,0), labels = area[k])
  par(opar)
  box()
}



cfr = c(0.0092, 0.0460, 0.1065, 0.0643, 0.0180, 0.0263, 0.0174,0.0332)
ratio = c(1.2, 1.035, 1.35, 1.52,1.09,1.5,1,1.3)
# dev.new()
par(mfrow=c(4,2))
for(k in 1:narea){
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
  options(repr.plot.width = 20, repr.plot.height = 8)
  # plot_multi_histogram(many_distros,area,k,'age', 'Category')
  plot_multi_histogram1(many_distros,area,k,'age', 'Category',cfr, ratio)
}







