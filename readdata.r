
###############Read Data#################
###############Read Data##################
###############Read Data##################
##read nohubei_data.xlsx##
#weight<-array(0,dim=c(narea-1,10))
#for(j in 2:narea){
#logitData<-logitdata(Data[j,,])
#for(i in 1:10){
#dex<-intersect(which(logitData[,2]>=((i-1)*10+1)), which(logitData[,2]<((i*10))))
#weight[j-1,i]<-sum((logitData[dex,2]))
#}    
#}
#sweight<-apply(weight,2,sum)
#sweight/sum(sweight)
#nia<-floor((sweight/sum(sweight))*20)
nia<-c(0, 0, 0, 1, 2, 3, 4, 5, 3, 2) 
#nia<-c(0, 0, 0, 0, 0, 1, 2, 6, 6, 5) 
dat<-read.xlsx("nohubei_die.xlsx", sheet = 1)
dat<-dat[-c(39,40,67),]
maxit<-1000
sample20<-array(0,dim=c(maxit,20))
for(j in 1:maxit){
sample20[j,]<-sort(sample(dat[,ncol(dat)],20,replace=F))
}
#miss_location<-apply(sample20,2,median)
#miss_location<-90:99
feihubei_data<- read.xlsx("nohubei_data.xlsx", sheet = 1)
for(j in 1:ncol(feihubei_data)){
feihubei_data[,j]<-as.numeric(feihubei_data[,j])
} 
#for(i in miss_location){
#feihubei_data[i,2]<-feihubei_data[i,2]+1
#feihubei_data[i,3]<-feihubei_data[i,3]+1 
#}
for(i in 1:10){
ad<-sort(rand_vect(10, nia[i]))
feihubei_data[((i-1)*10+1):(10*i),2]<-ad+feihubei_data[((i-1)*10+1):(10*i),2]
feihubei_data[((i-1)*10+1):(10*i),3]<-ad+feihubei_data[((i-1)*10+1):(10*i),3]
}

feihubei_data<-as.matrix(feihubei_data)
Data[1,,]<-feihubei_data



##read wuhan_data.xlsx##
Wuhan_data<- read.xlsx("Wuhan_data.xlsx", sheet = 1)
for(j in 1:ncol(Wuhan_data)){
 Wuhan_data[,j]<-as.numeric(Wuhan_data[,j])
 }

Wuhan_cov<-array(0,dim=c(100,3))
Wuhan_cov[,1]<-0:99
nelement<-cumsum(c(0,as.numeric(Wuhan_data[,4])))
for(i in 1:nrow(Wuhan_data)){
Wuhan_cov[(nelement[i]+1):(nelement[i+1]),2]<-sort(rand_vect(Wuhan_data[i,4], Wuhan_data[i,2]))
Wuhan_cov[(nelement[i]+1):(nelement[i+1]),3]<-sort(rand_vect(Wuhan_data[i,4], Wuhan_data[i,3]),decreasing =FALSE)
}
Data[2,,]<-Wuhan_cov


##read italy_data.xlsx##
Italy_data<- read.xlsx("Italy_data.xlsx", sheet = 1)
for(j in 1:ncol(Italy_data)){
 Italy_data[,j]<-as.numeric(Italy_data[,j])
 }

Italy_cov<-array(0,dim=c(100,3))
Italy_cov[,1]<-0:99
nelement<-cumsum(c(0,as.numeric(Italy_data[,4])))
for(i in 1:nrow(Italy_data)){
Italy_cov[(nelement[i]+1):(nelement[i+1]),2]<-sort(rand_vect(Italy_data[i,4], Italy_data[i,2]))
Italy_cov[(nelement[i]+1):(nelement[i+1]),3]<-sort(rand_vect(Italy_data[i,4], Italy_data[i,3]),decreasing =TRUE)
}

Data[3,,]<-Italy_cov



##read spanish_data.xlsx##
Span_data<- read.xlsx("Span_data.xlsx", sheet = 1)
for(j in 1:ncol(Span_data)){
 Span_data[,j]<-as.numeric(Span_data[,j])
 }

Span_cov<-array(0,dim=c(100,3))
Span_cov[,1]<-0:99
nelement<-cumsum(c(0,as.numeric(Span_data[,4])))
for(i in 1:nrow(Span_data)){
Span_cov[(nelement[i]+1):(nelement[i+1]),2]<-sort(rand_vect(Span_data[i,4], Span_data[i,2]))
Span_cov[(nelement[i]+1):(nelement[i+1]),3]<-sort(rand_vect(Span_data[i,4], Span_data[i,3]),decreasing =TRUE)
}

Data[4,,]<-Span_cov


##read germany_data.xlsx##
germany_data<- read.xlsx("germany_data.xlsx", sheet = 3)
for(j in 1:ncol(germany_data)){
 germany_data[,j]<-as.numeric(germany_data[,j])
 }

germany_cov<-array(0,dim=c(100,3))
germany_cov[,1]<-0:99
nelement<-cumsum(c(0,as.numeric(germany_data[,4])))    
for(i in 1:nrow(germany_data)){
germany_cov[(nelement[i]+1):(nelement[i+1]),2]<-sort(rand_vect(germany_data[i,4], germany_data[i,2]))
germany_cov[(nelement[i]+1):(nelement[i+1]),3]<-sort(rand_vect(germany_data[i,4], germany_data[i,3]),decreasing =TRUE)
}
Data[5,,]<-germany_cov



##read france_data.xlsx##
france_data<- read.xlsx("france_data.xlsx", sheet = 1)
for(j in 1:ncol(france_data)){
 france_data[,j]<-as.numeric(france_data[,j])
 }

france_cov<-array(0,dim=c(100,3))
france_cov[,1]<-0:99
nelement<-cumsum(c(0,as.numeric(france_data[,4])))
for(i in 1:nrow(france_data)){
france_cov[(nelement[i]+1):(nelement[i+1]),2]<-sort(rand_vect(france_data[i,4], france_data[i,2]))
france_cov[(nelement[i]+1):(nelement[i+1]),3]<-sort(rand_vect(france_data[i,4], france_data[i,3]),decreasing =TRUE)
}

Data[6,,]<-france_cov


##read korea_data.xlsx##
korea_data<- read.xlsx("korea_data.xlsx", sheet = 1)
for(j in 1:ncol(korea_data)){
 korea_data[,j]<-as.numeric(korea_data[,j])
 }

korea_cov<-array(0,dim=c(100,3))
korea_cov[,1]<-0:99
nelement<-cumsum(c(0,as.numeric(korea_data[,4])))
for(i in 1:nrow(korea_data)){
korea_cov[(nelement[i]+1):(nelement[i+1]),2]<-sort(rand_vect(korea_data[i,4], korea_data[i,2]))
korea_cov[(nelement[i]+1):(nelement[i+1]),3]<-sort(rand_vect(korea_data[i,4], korea_data[i,3]),decreasing =TRUE)
}

Data[7,,]<-korea_cov


##read spanish_data.xlsx##
canada_data<- read.xlsx("canada_data.xlsx", sheet = 1)
for(j in 1:ncol(canada_data)){
 canada_data[,j]<-as.numeric(canada_data[,j])
 }

canada_cov<-array(0,dim=c(100,3))
canada_cov[,1]<-0:99
nelement<-cumsum(c(0,as.numeric(canada_data[,4])))    
for(i in 1:nrow(canada_data)){
canada_cov[(nelement[i]+1):(nelement[i+1]),2]<-sort(rand_vect(canada_data[i,4], canada_data[i,2]))
canada_cov[(nelement[i]+1):(nelement[i+1]),3]<-sort(rand_vect(canada_data[i,4], canada_data[i,3]),decreasing=FALSE)
}
Data[8,,]<-canada_cov

##read america_data.xlsx##
america_data<- read.xlsx("america_data.xlsx", sheet = 3)
for(j in 1:ncol(america_data)){
 america_data[,j]<-as.numeric(america_data[,j])
 }

america_cov<-array(0,dim=c(100,3))
america_cov[,1]<-0:99
nelement<-cumsum(c(0,as.numeric(america_data[,4])))
for(i in 1:nrow(america_data)){
america_cov[(nelement[i]+1):(nelement[i+1]),2]<-sort(rand_vect(america_data[i,4], america_data[i,2]))
america_cov[(nelement[i]+1):(nelement[i+1]),3]<-sort(rand_vect(america_data[i,4], america_data[i,3]),decreasing =TRUE)
}

Data[9,,]<-america_cov

##read newyork_data.xlsx##
newyork_data<- read.xlsx("newyork_data.xlsx", sheet = 1)
for(j in 1:ncol(newyork_data)){
 newyork_data[,j]<-as.numeric(newyork_data[,j])
 }

newyork_cov<-array(0,dim=c(100,3))
newyork_cov[,1]<-0:99
nelement<-cumsum(c(0,as.numeric(newyork_data[,4])))
for(i in 1:nrow(newyork_data)){
newyork_cov[(nelement[i]+1):(nelement[i+1]),2]<-sort(rand_vect(newyork_data[i,4], newyork_data[i,2]))
newyork_cov[(nelement[i]+1):(nelement[i+1]),3]<-sort(rand_vect(newyork_data[i,4], newyork_data[i,3]),decreasing =TRUE)
}

Data[10,,]<-newyork_cov

Data[11,,1]<-0:99
Data[11,,c(2,3)]<-abs(america_cov[,c(2,3)]-newyork_cov[,c(2,3)])
