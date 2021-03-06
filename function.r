###############function##################
###############function##################
logitdata<-function(data){
ncumsum_confirmed<-cumsum(data[,3])
ncumsum_confirmed<-c(0,ncumsum_confirmed)
y<-array(0,dim=c(ncumsum_confirmed[length(ncumsum_confirmed)],2))
for(i in 1:nrow(data)){
y[(ncumsum_confirmed[i]+1):ncumsum_confirmed[i+1],1]<-0
if(data[i,3]==0){
data[i,3]<-1
}
y[(ncumsum_confirmed[i]+1):ncumsum_confirmed[i+1],2]<-rep(data[i,1],data[i,3])
if(data[i,2]!=0){
y[(ncumsum_confirmed[i]+1):(ncumsum_confirmed[i]+data[i,2]),1]<-1
}
}
return(y)
}



rand_vect <- function(N, M, sd = 3, pos.only = TRUE) {
  vec <- rnorm(N, M/N, sd)
  if (abs(sum(vec)) < 0.01) vec <- vec + 1
  vec <- round(vec / sum(vec) * M)
  deviation <- M - sum(vec)
  for (. in seq_len(abs(deviation))) {
    vec[i] <- vec[i <- sample(N, 1)] + sign(deviation)
  }
  if (pos.only) while (any(vec < 0)) {
    negs <- vec < 0
    pos  <- vec > 0
    vec[negs][i] <- vec[negs][i <- sample(sum(negs), 1)] + 1
    vec[pos][i]  <- vec[pos ][i <- sample(sum(pos ), 1)] - 1
  }
  vec
}


 rand_vec1 <- function(N,M,vec, sd = 2, pos.only = TRUE) {
  if (abs(sum(vec)) < 0.01) vec <- vec + 1
  vec <- round(vec / sum(vec) * M)
  deviation <- M - sum(vec)
  for (. in seq_len(abs(deviation))) {
    vec[i] <- vec[i <- sample(N, 1)] + sign(deviation)
  }
  if (pos.only) while (any(vec < 0)) {
    negs <- vec < 0
    pos  <- vec > 0
    vec[negs][i] <- vec[negs][i <- sample(sum(negs), 1)] + 1
    vec[pos][i]  <- vec[pos ][i <- sample(sum(pos ), 1)] - 1
  }
  vec
}






rand_vect_cont <- function(N, M, sd = 1) {
  vec <- rnorm(N, M/N, sd)
  vec / sum(vec) * M
}

objf.norm1 <- function(theta, x){
#x[,1]<-scale(x[,1])
mu <- theta[1]
s2 <- exp(theta[2])
res<-rep(0,nrow(x))
for(i in 1:nrow(x)){
res[i] <- x[i,3]*log(s2) + x[i,3]/s2*(x[i,1] - mu)^2
}
sum(res)
}

objec <- function(theta, x){
n <- nrow(x)
#a=bs(x[,1], knots=knots, Boundary.knots=c(0, 99), degree=1, intercept=T)
nolinear_effect<-cbind(rep(1,n),a)%*%theta
nl_effect<-exp(nolinear_effect)/(1+exp(nolinear_effect))
#nl_effect<-exp(theta[1]+theta[2]*x[,1])/(1+exp(theta[1]+theta[2]*x[,1]))
res<-c()
for(i in 1:n){
#res[i]<-dbinom(x[i,2], prob =nl_effect[i], size =x[i,3], log = T)
res[i]<-x[i,2]*log(nl_effect[i])+(x[i,3]-x[i,2])*log(1-nl_effect[i])
}
-sum(res)
}


find.nearest <- function(vec, x) {    # here `vec` is sorted
  nearest.idx <- which.min(abs(vec - x))[1]
  return(nearest.idx)
}

integrand <- function(y,GAM,k,Kernel,l) {
newd <- data.frame(age=y)
nolinear_effect<-predict(GAM[[k]],newd)
hatp<-exp(nolinear_effect)/(1+exp(nolinear_effect)) 
df<-approxfun(Kernel[[l]])
df(y)*hatp
} 


louzhen_integrand <- function(y,GAM,k,kk,Kernel,l) {
#knots=quantile(0:99,c(1/3,2/3))
#a<-bs(t, knots=knots, Boundary.knots=c(0, 99), degree=1, intercept=T) 
newd <- data.frame(age=y)
nolinear_effect<-predict(GAM[[k]],newd)
xxnolinear_effect<-predict(GAM[[kk]],newd)
#nolinear_effect<-c(cbind(1,matrix(y^seq(1,length(theta)-1,by=1),nrow=1))%*%theta )
hatp<-exp(nolinear_effect)/(1+exp(nolinear_effect)) 
xxhatp<-exp(xxnolinear_effect)/(1+exp(xxnolinear_effect))
df<-approxfun(Kernel[[l]])
df(y)*hatp/(xxhatp)
}
 
densit<-function(y,Kernel,l){
df<-approxfun(Kernel[[l]])
df(y)
}



factors <- function(x) {
fact<-c()
for(i in 1:x) {
if((x %% i) == 0) {
 fact[i]<-i
}
}
return(fact[which(fact!=0)])
}



####################¼ÑÎ°############################
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