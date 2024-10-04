sink("C:\\Users\\Geoff\\Dropbox\\D\\projects\\scf_biz_assets\\programs\\_LOGS\\27_create_figure_E2_log.txt")

##########################################################
##########################################################
##                                                      ##
## PROGRAM NAME: 27_create_figure_E2                    ##
## DESCRIPTION:                                         ##
##                                                      ##  
##  plots log-normal models for effects of credit       ##
##                                                      ##
##########################################################
##########################################################

### LOAD LIBRARIES ###
rm(list=ls())
library(foreign)
library(dplyr)
library(tidyr)
library(ggplot2)
library(mgcv)
library(data.table)
library(quantreg)

### INPUT DATA ###
scfmi<-read.dta("C:\\Users\\Geoff\\Dropbox\\D\\projects\\scf_biz_assets\\data\\v01_scf_final_merged.dta")
year<-c(seq(1989,2019,by=3),seq(1989,2019,by=3))
cred<-c(rep("no_credit",length(year)/2),rep("credit",length(year)/2))
cmatrix<-data.frame(nyear=year,nbizcred=cred)
set.seed(60657)
nsim<-10000000
nboot<-200
smooth<-10
adjust<-0.9

### COMPUTE MODEL-BASED TRENDS ###

miestpx<-miestpd<-miestmu<-miestsg<-matrix(data=NA,nrow=length(cmatrix[,1]),ncol=5)
miest1pct.com<-miest1pct.btw<-mivar1pct.com<-mivar1pct.btw<-matrix(data=NA,nrow=length(seq(1989,2019,by=3)),ncol=5)
for (i in 1:5) {

  scfpop<-scfmi[(which(scfmi$"nminum"==i)),]
  scfbiz<-scfmi[which(scfmi$"nminum"==i & scfmi$"nbizvalpos"==1 & scfmi$"nbizcred"!="NA"),]
  dsmooth<-gam(nbizvalpos~s(nyear),family=quasibinomial(link="probit"),gamma=smooth,data=scfpop,weights=nsampwt)
  pd<-predict(dsmooth,newdata=cmatrix,type="response")
  xsmooth<-gam(nbizcred~s(nyear),family=quasibinomial(link="probit"),gamma=smooth,data=scfbiz,weights=nsampwt)
  px<-predict(xsmooth,newdata=cmatrix,type="response")
  ysmooth<-gam(nlogbizval~nbizcred+s(nyear,by=nbizcred),family=gaussian(link="identity"),gamma=smooth,data=scfbiz,weights=nsampwt)
  mu<-predict(ysmooth,newdata=cmatrix,type="response")
  scfbiz$ehatsq<-residuals(ysmooth,type="response")^2
  esmooth<-gam(ehatsq~nbizcred+s(nyear,by=nbizcred),family=Gamma(link="log"),gamma=smooth,data=scfbiz,weights=nsampwt)
  sigmasq<-exp(predict(esmooth,newdata=cmatrix))
  params<-cbind(cmatrix,px,pd,mu,sigmasq)

  top1pct.mod<-top1pct.com<-top1pct.btw<-NULL
  for (j in seq(1989,2019,by=3)) {
    sim<-data.frame(matrix(data=NA,nrow=nsim,ncol=3))
    setnames(sim,old=c("X1","X2","X3"),new=c("x","d","y"))
    d<-rbinom(nsim,1,params[which(params$nyear==j & params$nbizcred=="credit"),"pd"])
    sim$d<-d
    xd1<-rbinom(sum(d),1,params[which(params$nyear==j & params$nbizcred=="credit"),"px"])
    sim$x[sim$d==0]<-0
    sim$x[sim$d==1]<-xd1
    yposx0<-exp(rnorm(sum(d)-sum(xd1),params[which(params$nyear==j & params$nbizcred=="no_credit"),"mu"],sqrt(params[which(params$nyear==j & params$nbizcred=="no_credit"),"sigmasq"])*adjust))
    yposx1<-exp(rnorm(sum(xd1),params[which(params$nyear==j & params$nbizcred=="credit"),"mu"],sqrt(params[which(params$nyear==j & params$nbizcred=="credit"),"sigmasq"])*adjust))
    sim$y[sim$d==0]<-0
    sim$y[sim$d==1 & sim$x==0]<-yposx0
    sim$y[sim$d==1 & sim$x==1]<-yposx1
    q99<-quantile(sim$y,probs=0.99)
    toty<-sum(sim$y)
    totyq99<-sum(sim$y[sim$y>=q99])
    share1pct<-(totyq99/toty)
    top1pct.mod<-c(top1pct.mod,share1pct)
    }
  for (j in seq(1989,2019,by=3)) {
    sim<-data.frame(matrix(data=NA,nrow=nsim,ncol=3))
    setnames(sim,old=c("X1","X2","X3"),new=c("x","d","y"))
    d<-rbinom(nsim,1,params[which(params$nyear==j & params$nbizcred=="credit"),"pd"])
    sim$d<-d
    xd1<-rbinom(sum(d),1,params[which(params$nyear==1989 & params$nbizcred=="credit"),"px"])
    sim$x[sim$d==0]<-0
    sim$x[sim$d==1]<-xd1
    yposx0<-exp(rnorm(sum(d)-sum(xd1),params[which(params$nyear==j & params$nbizcred=="no_credit"),"mu"],sqrt(params[which(params$nyear==j & params$nbizcred=="no_credit"),"sigmasq"])*adjust))
    yposx1<-exp(rnorm(sum(xd1),params[which(params$nyear==j & params$nbizcred=="credit"),"mu"],sqrt(params[which(params$nyear==j & params$nbizcred=="credit"),"sigmasq"])*adjust))
    sim$y[sim$d==0]<-0
    sim$y[sim$d==1 & sim$x==0]<-yposx0
    sim$y[sim$d==1 & sim$x==1]<-yposx1
    q99<-quantile(sim$y,probs=0.99)
    toty<-sum(sim$y)
    totyq99<-sum(sim$y[sim$y>=q99])
    share1pct<-(totyq99/toty)
    top1pct.com<-c(top1pct.com,share1pct)
    }
  for (j in seq(1989,2019,by=3)) {
    sim<-data.frame(matrix(data=NA,nrow=nsim,ncol=3))
    setnames(sim,old=c("X1","X2","X3"),new=c("x","d","y"))
    d<-rbinom(nsim,1,params[which(params$nyear==j & params$nbizcred=="credit"),"pd"])
    sim$d<-d
    xd1<-rbinom(sum(d),1,params[which(params$nyear==1989 & params$nbizcred=="credit"),"px"])
    sim$x[sim$d==0]<-0
    sim$x[sim$d==1]<-xd1
    muPopYR<-(1-params[which(params$nyear==j & params$nbizcred=="no_credit"),"px"])*params[which(params$nyear==j & params$nbizcred=="no_credit"),"mu"]+params[which(params$nyear==j & params$nbizcred=="credit"),"px"]*params[which(params$nyear==j & params$nbizcred=="credit"),"mu"]
    muPop89<-(1-params[which(params$nyear==1989 & params$nbizcred=="no_credit"),"px"])*params[which(params$nyear==1989 & params$nbizcred=="no_credit"),"mu"]+params[which(params$nyear==1989 & params$nbizcred=="credit"),"px"]*params[which(params$nyear==1989 & params$nbizcred=="credit"),"mu"]
    mu0<-params[which(params$nyear==1989 & params$nbizcred=="no_credit"),"mu"]+(muPopYR-muPop89)
    mu1<-params[which(params$nyear==1989 & params$nbizcred=="credit"),"mu"]+(muPopYR-muPop89)
    yposx0<-exp(rnorm(sum(d)-sum(xd1),mu0,sqrt(params[which(params$nyear==j & params$nbizcred=="no_credit"),"sigmasq"])*adjust))
    yposx1<-exp(rnorm(sum(xd1),mu1,sqrt(params[which(params$nyear==j & params$nbizcred=="credit"),"sigmasq"])*adjust))
    sim$y[sim$d==0]<-0
    sim$y[sim$d==1 & sim$x==0]<-yposx0
    sim$y[sim$d==1 & sim$x==1]<-yposx1
    q99<-quantile(sim$y,probs=0.99)
    toty<-sum(sim$y)
    totyq99<-sum(sim$y[sim$y>=q99])
    share1pct<-(totyq99/toty)
    top1pct.btw<-c(top1pct.btw,share1pct)
    }
  miest1pct.com[,i]<-top1pct.com-top1pct.mod
  miest1pct.btw[,i]<-top1pct.btw-top1pct.mod
  miestpx[,i]<-px
  miestpd[,i]<-pd
  miestmu[,i]<-mu
  miestsg[,i]<-sigmasq
  
  boot.dist.com1<-boot.dist.btw1<-matrix(data=NA,nrow=length(seq(1989,2019,by=3)),ncol=nboot)
  for (k in 60:(nboot+59)) {
    boot.dsmooth<-gam(nbizvalpos~s(nyear),family=quasibinomial(link="probit"),gamma=smooth,data=scfpop,weights=scfpop[,k])
    boot.pd<-predict(boot.dsmooth,newdata=cmatrix,type="response")
    boot.xsmooth<-gam(nbizcred~s(nyear),family=quasibinomial(link="probit"),gamma=smooth,data=scfbiz,weights=scfbiz[,k])
    boot.px<-predict(boot.xsmooth,newdata=cmatrix,type="response")
    boot.ysmooth<-gam(nlogbizval~nbizcred+s(nyear,by=nbizcred),family=gaussian(link="identity"),gamma=smooth,data=scfbiz,weights=scfbiz[,k])
    boot.mu<-predict(boot.ysmooth,newdata=cmatrix,type="response")
    scfbiz$boot.ehatsq<-residuals(boot.ysmooth,type="response")^2
    boot.esmooth<-gam(boot.ehatsq~nbizcred+s(nyear,by=nbizcred),family=Gamma(link="log"),gamma=smooth,data=scfbiz,weights=scfbiz[,k])
    boot.sigmasq<-exp(predict(boot.esmooth,newdata=cmatrix))
    boot.params<-cbind(cmatrix,boot.px,boot.pd,boot.mu,boot.sigmasq)
    boot.top1pct.mod<-boot.top1pct.com<-boot.top1pct.btw<-NULL
    for (j in seq(1989,2019,by=3)) {
      boot.sim<-data.frame(matrix(data=NA,nrow=nsim,ncol=3))
      setnames(boot.sim,old=c("X1","X2","X3"),new=c("x","d","y"))
      boot.d<-rbinom(nsim,1,boot.params[which(boot.params$nyear==j & boot.params$nbizcred=="credit"),"boot.pd"])
      boot.sim$d<-boot.d
      boot.xd1<-rbinom(sum(boot.d),1,boot.params[which(boot.params$nyear==j & boot.params$nbizcred=="credit"),"boot.px"])
      boot.sim$x[boot.sim$d==0]<-0
      boot.sim$x[boot.sim$d==1]<-boot.xd1
      boot.yposx0<-exp(rnorm(sum(boot.d)-sum(boot.xd1),boot.params[which(boot.params$nyear==j & boot.params$nbizcred=="no_credit"),"boot.mu"],sqrt(boot.params[which(boot.params$nyear==j & boot.params$nbizcred=="no_credit"),"boot.sigmasq"])*adjust))
      boot.yposx1<-exp(rnorm(sum(boot.xd1),boot.params[which(boot.params$nyear==j & boot.params$nbizcred=="credit"),"boot.mu"],sqrt(boot.params[which(boot.params$nyear==j & boot.params$nbizcred=="credit"),"boot.sigmasq"])*adjust))
      boot.sim$y[boot.sim$d==0]<-0
      boot.sim$y[boot.sim$d==1 & boot.sim$x==0]<-boot.yposx0
      boot.sim$y[boot.sim$d==1 & boot.sim$x==1]<-boot.yposx1
      boot.q99<-quantile(boot.sim$y,probs=0.99)
      boot.toty<-sum(boot.sim$y)
      boot.totyq99<-sum(boot.sim$y[boot.sim$y>=boot.q99])
      boot.share1pct<-(boot.totyq99/boot.toty)
      boot.top1pct.mod<-c(boot.top1pct.mod,boot.share1pct)
      }  
    for (j in seq(1989,2019,by=3)) {
      boot.sim<-data.frame(matrix(data=NA,nrow=nsim,ncol=3))
      setnames(boot.sim,old=c("X1","X2","X3"),new=c("x","d","y"))
      boot.d<-rbinom(nsim,1,boot.params[which(boot.params$nyear==j & boot.params$nbizcred=="credit"),"boot.pd"])
      boot.sim$d<-boot.d
      boot.xd1<-rbinom(sum(boot.d),1,boot.params[which(boot.params$nyear==1989 & boot.params$nbizcred=="credit"),"boot.px"])
      boot.sim$x[boot.sim$d==0]<-0
      boot.sim$x[boot.sim$d==1]<-boot.xd1
      boot.yposx0<-exp(rnorm(sum(boot.d)-sum(boot.xd1),boot.params[which(boot.params$nyear==j & boot.params$nbizcred=="no_credit"),"boot.mu"],sqrt(boot.params[which(boot.params$nyear==j & boot.params$nbizcred=="no_credit"),"boot.sigmasq"])*adjust))
      boot.yposx1<-exp(rnorm(sum(boot.xd1),boot.params[which(boot.params$nyear==j & boot.params$nbizcred=="credit"),"boot.mu"],sqrt(boot.params[which(boot.params$nyear==j & boot.params$nbizcred=="credit"),"boot.sigmasq"])*adjust))
      boot.sim$y[boot.sim$d==0]<-0
      boot.sim$y[boot.sim$d==1 & boot.sim$x==0]<-boot.yposx0
      boot.sim$y[boot.sim$d==1 & boot.sim$x==1]<-boot.yposx1
      boot.q99<-quantile(boot.sim$y,probs=0.99)
      boot.toty<-sum(boot.sim$y)
      boot.totyq99<-sum(boot.sim$y[boot.sim$y>=boot.q99])
      boot.share1pct<-(boot.totyq99/boot.toty)
      boot.top1pct.com<-c(boot.top1pct.com,boot.share1pct)
      }  
    for (j in seq(1989,2019,by=3)) {
      boot.sim<-data.frame(matrix(data=NA,nrow=nsim,ncol=3))
      setnames(boot.sim,old=c("X1","X2","X3"),new=c("x","d","y"))
      boot.d<-rbinom(nsim,1,boot.params[which(boot.params$nyear==j & boot.params$nbizcred=="credit"),"boot.pd"])
      boot.sim$d<-boot.d
      boot.xd1<-rbinom(sum(boot.d),1,boot.params[which(boot.params$nyear==1989 & boot.params$nbizcred=="credit"),"boot.px"])
      boot.sim$x[boot.sim$d==0]<-0
      boot.sim$x[boot.sim$d==1]<-boot.xd1
      boot.muPopYR<-(1-boot.params[which(boot.params$nyear==j & boot.params$nbizcred=="no_credit"),"boot.px"])*boot.params[which(boot.params$nyear==j & boot.params$nbizcred=="no_credit"),"boot.mu"]+boot.params[which(boot.params$nyear==j & boot.params$nbizcred=="credit"),"boot.px"]*boot.params[which(boot.params$nyear==j & boot.params$nbizcred=="credit"),"boot.mu"]
      boot.muPop89<-(1-boot.params[which(boot.params$nyear==1989 & boot.params$nbizcred=="no_credit"),"boot.px"])*boot.params[which(boot.params$nyear==1989 & boot.params$nbizcred=="no_credit"),"boot.mu"]+boot.params[which(boot.params$nyear==1989 & boot.params$nbizcred=="credit"),"boot.px"]*boot.params[which(boot.params$nyear==1989 & boot.params$nbizcred=="credit"),"boot.mu"]
      boot.mu0<-boot.params[which(boot.params$nyear==1989 & boot.params$nbizcred=="no_credit"),"boot.mu"]+(boot.muPopYR-boot.muPop89)
      boot.mu1<-boot.params[which(boot.params$nyear==1989 & boot.params$nbizcred=="credit"),"boot.mu"]+(boot.muPopYR-boot.muPop89)
      boot.yposx0<-exp(rnorm(sum(boot.d)-sum(boot.xd1),boot.mu0,sqrt(boot.params[which(boot.params$nyear==j & boot.params$nbizcred=="no_credit"),"boot.sigmasq"])*adjust))
      boot.yposx1<-exp(rnorm(sum(boot.xd1),boot.mu1,sqrt(boot.params[which(boot.params$nyear==j & boot.params$nbizcred=="credit"),"boot.sigmasq"])*adjust))
      boot.sim$y[boot.sim$d==0]<-0
      boot.sim$y[boot.sim$d==1 & boot.sim$x==0]<-boot.yposx0
      boot.sim$y[boot.sim$d==1 & boot.sim$x==1]<-boot.yposx1
      boot.q99<-quantile(boot.sim$y,probs=0.99)
      boot.toty<-sum(boot.sim$y)
      boot.totyq99<-sum(boot.sim$y[boot.sim$y>=boot.q99])
      boot.share1pct<-(boot.totyq99/boot.toty)
      boot.top1pct.btw<-c(boot.top1pct.btw,boot.share1pct)
      }  
    boot.dist.com1[,(k-59)]<-boot.top1pct.com-boot.top1pct.mod
    boot.dist.btw1[,(k-59)]<-boot.top1pct.btw-boot.top1pct.mod
    }
  for (m in 1:length(seq(1989,2019,by=3))) { 
    mivar1pct.com[m,i]<-var(boot.dist.com1[m,]) 
    mivar1pct.btw[m,i]<-var(boot.dist.btw1[m,]) 
    }
  }

micompx<-micompd<-micommu<-micomsg<-matrix(data=NA,nrow=length(cmatrix[,1]),ncol=1)
for (i in 1:length(cmatrix[,1])) { 
	micompx[i,1]<-mean(miestpx[i,])
	micompd[i,1]<-mean(miestpd[i,])
	micommu[i,1]<-mean(miestmu[i,])
	micomsg[i,1]<-mean(miestsg[i,])
	}
trends.parm<-data.frame(cbind(cmatrix,micompx,micompd,micommu,micomsg))
setnames(trends.parm,old=c("nyear","nbizcred","micompx","micompd","micommu","micomsg"),new=c("year","credit","px","pd","mu","sigmasq"))

micom1pct.com<-micom1pct.btw<-matrix(data=NA,nrow=length(seq(1989,2019,by=3)),ncol=3)
for (i in 1:length(seq(1989,2019,by=3))) { 
  micom1pct.com[i,1]<-mean(miest1pct.com[i,])
  micom1pct.com[i,2]<-sqrt(mean(mivar1pct.com[i,])+(var(miest1pct.com[i,])*(1+(1/5))))
  micom1pct.com[i,3]<-pnorm((micom1pct.com[i,1]/micom1pct.com[i,2]))
  micom1pct.btw[i,1]<-mean(miest1pct.btw[i,])
  micom1pct.btw[i,2]<-sqrt(mean(mivar1pct.btw[i,])+(var(miest1pct.btw[i,])*(1+(1/5))))
  micom1pct.btw[i,3]<-pnorm((micom1pct.btw[i,1]/micom1pct.btw[i,2]))
  }

micom1pct.com[1,1]<-micom1pct.btw[1,1]<-0
micom1pct.com[1,2]<-micom1pct.btw[1,2]<-0
micom1pct.com[1,3]<-micom1pct.btw[1,3]<-1
trends.com<-data.frame(cbind(seq(1989,2019,by=3),micom1pct.com))
setnames(trends.com,old=c("X1","X2","X3","X4"),new=c("year","shr1pct","shr1pct.se","shr1pct.pval"))
trends.btw<-data.frame(cbind(seq(1989,2019,by=3),micom1pct.btw))
setnames(trends.btw,old=c("X1","X2","X3","X4"),new=c("year","shr1pct","shr1pct.se","shr1pct.pval"))

print(trends.parm)
print(trends.com)
print(trends.btw)

### PLOT TRENDS ###
tiff("C:\\Users\\Geoff\\Dropbox\\D\\projects\\scf_biz_assets\\figures\\figure_E2.tiff",
     width=9,
     height=9,
     units='in',
     res=600)

par(mfrow=c(2,2))

plot(trends.parm$year[trends.parm$credit=="no_credit"],trends.parm$px[trends.parm$credit=="no_credit"],
     type="l",
     ylim=c(0.0,0.5),
     xlab="Year",
     ylab="P(Z=1|Y>0)",
     main="Credit Access among Business Owners")

plot(trends.parm$year[trends.parm$credit=="no_credit"],trends.parm$mu[trends.parm$credit=="no_credit"],
     type="l",
     lty="dotted",
     ylim=c(10,14),
     xlab="Year",
     ylab="E(log(Y)|Y>0,Z)",
     main="Mean Asset Value by Credit Access")
lines(trends.parm$year[trends.parm$credit=="credit"],trends.parm$mu[trends.parm$credit=="credit"],type="l",lty="dashed")
labels<-c("Did Not Access Credit (Z=0)","Accessed Credit (Z=1)")
type<-c("dotted","dashed")
legend("topleft",inset=0.03,labels,lty=type,seg.len=2,cex=1.0)

plot(trends.parm$year[trends.parm$credit=="no_credit"],trends.parm$sigmasq[trends.parm$credit=="no_credit"],
     type="l",
     lty="dotted",
     ylim=c(2,7),
     xlab="Year",
     ylab="Var(log(Y)|Y>0,Z)",
     main="Asset Variance by Credit Access")
lines(trends.parm$year[trends.parm$credit=="credit"],trends.parm$sigmasq[trends.parm$credit=="credit"],type="l",lty="dashed")
labels<-c("Did Not Access Credit (Z=0)","Accessed Credit (Z=1)")
type<-c("dotted","dashed")
legend("topleft",inset=0.03,labels,lty=type,seg.len=2,cex=1.0)

plot(trends.com$year,trends.com$shr1pct,
     type="l",
     lty="dotdash",
     ylim=c(-0.05,0.05),
     xlab="Year",
     ylab="Difference in Quantile Shares",
     main="Counterfactual Differences in the Top 1% Share")
abline(h=0,lty="solid",col="gray85")
lines(trends.btw$year,trends.btw$shr1pct,type="l",lty="longdash")
with(trends.com[which(trends.com$shr1pct.pval<0.05),], points(year,shr1pct,pch=8,cex=1.2))
with(trends.btw[which(trends.btw$shr1pct.pval<0.05),], points(year,shr1pct,pch=8,cex=1.2))
labels<-c("Compositional Effect","Compositional + Between Group Effect")
type<-c("dotdash","longdash")
legend("topleft",inset=0.03,labels,lty=type,seg.len=2,cex=1)

dev.off()

sink()
