
###############estimation###########################   
###############estimation###########################   
###############estimation###########################   
###############estimation###########################                                                            

for(j in 1:narea){
logitData<-logitdata(Data[j,,])
colnames(logitData)<-c("y","age")
logitData<-as.data.frame(logitData) 
gam1<-gam(y~s(age), family = binomial,data=logitData,method="REML")
GAM[[j]]<-gam1
for(i in 1:ncol(Hatpi)){
Hatpi[j,i]<-((exp(gam1$linear.predictors)/(1+exp(gam1$linear.predictors)))[which(logitData[,2]==(i-1))])[1]
}
Kernel[[j]]<-density(logitData[,2])
}
rownames(Hatpi)<-area
write.table(t(Hatpi),"hatpi.txt")