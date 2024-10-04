sink("C:\\Users\\Geoff\\Dropbox\\D\\projects\\scf_biz_assets\\programs\\_LOGS\\28_create_figure_E3_log.txt")

##########################################################
##########################################################
##                                                      ##
## PROGRAM NAME: 28_create_figure_E3                    ##
## DESCRIPTION:                                         ##
##                                                      ##  
##  plots log-normal models for effects of firm scale   ##
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
year<-c(seq(1989,2019,by=3),seq(1989,2019,by=3),seq(1989,2019,by=3))
fsize<-c(rep("0",length(year)/3),rep("1",length(year)/3),rep("2",length(year)/3))
cmatrix<-data.frame(nyear=year,nfirmsize=fsize)
set.seed(60657)
nsim<-10000000
nboot<-200
smooth<-10
adjust<-0.9

### COMPUTE MODEL-BASED TRENDS ###
miestpx0<-miestpx1<-miestpx2<-miestpd<-miestmu<-miestsg<-matrix(data=NA,nrow=length(cmatrix[,1]),ncol=5)
miest1pct.com<-miest1pct.btw<-mivar1pct.com<-mivar1pct.btw<-matrix(data=NA,nrow=length(seq(1989,2019,by=3)),ncol=5)
for (i in 1:5) {

  scfpop<-scfmi[(which(scfmi$"nminum"==i)),]
  scfbiz<-scfmi[which(scfmi$"nminum"==i & scfmi$"nbizvalpos"==1 & scfmi$"nfirmsize"!="NA"),]
  dsmooth<-gam(nbizvalpos~s(nyear),family=quasibinomial(link="probit"),gamma=smooth,data=scfpop,weights=nsampwt)
  pd<-predict(dsmooth,newdata=cmatrix,type="response")
  scfbiz$nfirmsize<-as.numeric(scfbiz$nfirmsize)-1
  xsmooth<-gam(list(nfirmsize~s(nyear),~s(nyear)),family=multinom(K=2),gamma=smooth,data=scfbiz,weights=nsampwt)
  px<-predict(xsmooth,newdata=cmatrix,type="response")
  px0<-px[,1]	
  px1<-px[,2]
  px2<-px[,3]
  scfbiz$nfirmsize<-as.factor(scfbiz$nfirmsize)
  ysmooth<-gam(nlogbizval~nfirmsize+s(nyear,by=nfirmsize),family=gaussian(link="identity"),gamma=smooth,data=scfbiz,weights=nsampwt)
  mu<-predict(ysmooth,newdata=cmatrix,type="response")
  scfbiz$ehatsq<-residuals(ysmooth,type="response")^2
  esmooth<-gam(ehatsq~nfirmsize+s(nyear,by=nfirmsize),family=Gamma(link="log"),gamma=smooth,data=scfbiz,weights=nsampwt)
  sigmasq<-exp(predict(esmooth,newdata=cmatrix))
  params<-cbind(cmatrix,px0,px1,px2,pd,mu,sigmasq)

  top1pct.mod<-top1pct.com<-top1pct.btw<-NULL
  for (j in seq(1989,2019,by=3)) {
    sim<-data.frame(matrix(data=NA,nrow=nsim,ncol=3))
    setnames(sim,old=c("X1","X2","X3"),new=c("x","d","y"))
    d<-rbinom(nsim,1,params[which(params$nyear==j & params$nfirmsize=="0"),"pd"])
    sim$d<-d
    xcounts<-rmultinom(1,sum(d),params[which(params$nyear==j & params$nfirmsize=="0"),c("px0","px1","px2")])
    x1d1<-rep(1,xcounts[1,1])
    x2d1<-rep(2,xcounts[2,1])
    x3d1<-rep(3,xcounts[3,1])
    xd1<-c(x1d1,x2d1,x3d1)
    sim$x[sim$d==0]<-0
    sim$x[sim$d==1]<-xd1
    yposx1<-exp(rnorm(sum(d)-(xcounts[2,1]+xcounts[3,1]),params[which(params$nyear==j & params$nfirmsize=="0"),"mu"],sqrt(params[which(params$nyear==j & params$nfirmsize=="0"),"sigmasq"])*adjust))
    yposx2<-exp(rnorm(sum(d)-(xcounts[1,1]+xcounts[3,1]),params[which(params$nyear==j & params$nfirmsize=="1"),"mu"],sqrt(params[which(params$nyear==j & params$nfirmsize=="1"),"sigmasq"])*adjust))
    yposx3<-exp(rnorm(sum(d)-(xcounts[1,1]+xcounts[2,1]),params[which(params$nyear==j & params$nfirmsize=="2"),"mu"],sqrt(params[which(params$nyear==j & params$nfirmsize=="2"),"sigmasq"])*adjust))
    sim$y[sim$d==0]<-0
    sim$y[sim$d==1 & sim$x==1]<-yposx1
    sim$y[sim$d==1 & sim$x==2]<-yposx2
    sim$y[sim$d==1 & sim$x==3]<-yposx3
    q99<-quantile(sim$y,probs=0.99)
    toty<-sum(sim$y)
    totyq99<-sum(sim$y[sim$y>=q99])
    share1pct<-(totyq99/toty)
    top1pct.mod<-c(top1pct.mod,share1pct)
    }
  for (j in seq(1989,2019,by=3)) {
    sim<-data.frame(matrix(data=NA,nrow=nsim,ncol=3))
    setnames(sim,old=c("X1","X2","X3"),new=c("x","d","y"))
    d<-rbinom(nsim,1,params[which(params$nyear==j & params$nfirmsize=="0"),"pd"])
    sim$d<-d
    xcounts<-rmultinom(1,sum(d),params[which(params$nyear==1989 & params$nfirmsize=="0"),c("px0","px1","px2")])
    x1d1<-rep(1,xcounts[1,1])
    x2d1<-rep(2,xcounts[2,1])
    x3d1<-rep(3,xcounts[3,1])
    xd1<-c(x1d1,x2d1,x3d1)
    sim$x[sim$d==0]<-0
    sim$x[sim$d==1]<-xd1
    yposx1<-exp(rnorm(sum(d)-(xcounts[2,1]+xcounts[3,1]),params[which(params$nyear==j & params$nfirmsize=="0"),"mu"],sqrt(params[which(params$nyear==j & params$nfirmsize=="0"),"sigmasq"])*adjust))
    yposx2<-exp(rnorm(sum(d)-(xcounts[1,1]+xcounts[3,1]),params[which(params$nyear==j & params$nfirmsize=="1"),"mu"],sqrt(params[which(params$nyear==j & params$nfirmsize=="1"),"sigmasq"])*adjust))
    yposx3<-exp(rnorm(sum(d)-(xcounts[1,1]+xcounts[2,1]),params[which(params$nyear==j & params$nfirmsize=="2"),"mu"],sqrt(params[which(params$nyear==j & params$nfirmsize=="2"),"sigmasq"])*adjust))
    sim$y[sim$d==0]<-0
    sim$y[sim$d==1 & sim$x==1]<-yposx1
    sim$y[sim$d==1 & sim$x==2]<-yposx2
    sim$y[sim$d==1 & sim$x==3]<-yposx3
    q99<-quantile(sim$y,probs=0.99)
    toty<-sum(sim$y)
    totyq99<-sum(sim$y[sim$y>=q99])
    share1pct<-(totyq99/toty)
    top1pct.com<-c(top1pct.com,share1pct)
    }
  for (j in seq(1989,2019,by=3)) {
    sim<-data.frame(matrix(data=NA,nrow=nsim,ncol=3))
    setnames(sim,old=c("X1","X2","X3"),new=c("x","d","y"))
    d<-rbinom(nsim,1,params[which(params$nyear==j & params$nfirmsize=="0"),"pd"])
    sim$d<-d
    xcounts<-rmultinom(1,sum(d),params[which(params$nyear==1989 & params$nfirmsize=="0"),c("px0","px1","px2")])
    x1d1<-rep(1,xcounts[1,1])
    x2d1<-rep(2,xcounts[2,1])
    x3d1<-rep(3,xcounts[3,1])
    xd1<-c(x1d1,x2d1,x3d1)
    sim$x[sim$d==0]<-0
    sim$x[sim$d==1]<-xd1
    muPopYR<-
	(params[which(params$nyear==j & params$nfirmsize=="0"),"px0"]*params[which(params$nyear==j & params$nfirmsize=="0"),"mu"])+
	(params[which(params$nyear==j & params$nfirmsize=="1"),"px1"]*params[which(params$nyear==j & params$nfirmsize=="1"),"mu"])+
	(params[which(params$nyear==j & params$nfirmsize=="2"),"px2"]*params[which(params$nyear==j & params$nfirmsize=="2"),"mu"])
    muPop89<-
	(params[which(params$nyear==1989 & params$nfirmsize=="0"),"px0"]*params[which(params$nyear==1989 & params$nfirmsize=="0"),"mu"])+
	(params[which(params$nyear==1989 & params$nfirmsize=="1"),"px1"]*params[which(params$nyear==1989 & params$nfirmsize=="1"),"mu"])+
	(params[which(params$nyear==1989 & params$nfirmsize=="2"),"px2"]*params[which(params$nyear==1989 & params$nfirmsize=="2"),"mu"])
    mu1<-params[which(params$nyear==1989 & params$nfirmsize=="0"),"mu"]+(muPopYR-muPop89)
    mu2<-params[which(params$nyear==1989 & params$nfirmsize=="1"),"mu"]+(muPopYR-muPop89)
    mu3<-params[which(params$nyear==1989 & params$nfirmsize=="2"),"mu"]+(muPopYR-muPop89)
    yposx1<-exp(rnorm(sum(d)-(xcounts[2,1]+xcounts[3,1]),mu1,sqrt(params[which(params$nyear==j & params$nfirmsize=="0"),"sigmasq"])*adjust))
    yposx2<-exp(rnorm(sum(d)-(xcounts[1,1]+xcounts[3,1]),mu2,sqrt(params[which(params$nyear==j & params$nfirmsize=="1"),"sigmasq"])*adjust))
    yposx3<-exp(rnorm(sum(d)-(xcounts[1,1]+xcounts[2,1]),mu3,sqrt(params[which(params$nyear==j & params$nfirmsize=="2"),"sigmasq"])*adjust))
    sim$y[sim$d==0]<-0
    sim$y[sim$d==1 & sim$x==1]<-yposx1
    sim$y[sim$d==1 & sim$x==2]<-yposx2
    sim$y[sim$d==1 & sim$x==3]<-yposx3
    q99<-quantile(sim$y,probs=0.99)
    toty<-sum(sim$y)
    totyq99<-sum(sim$y[sim$y>=q99])
    share1pct<-(totyq99/toty)
    top1pct.btw<-c(top1pct.btw,share1pct)
    }
  miest1pct.com[,i]<-top1pct.com-top1pct.mod
  miest1pct.btw[,i]<-top1pct.btw-top1pct.mod
  miestpx0[,i]<-px0
  miestpx1[,i]<-px1
  miestpx2[,i]<-px2
  miestpd[,i]<-pd
  miestmu[,i]<-mu
  miestsg[,i]<-sigmasq

  boot.dist.com1<-boot.dist.btw1<-matrix(data=NA,nrow=length(seq(1989,2019,by=3)),ncol=nboot)
  for (k in 60:(nboot+59)) {
    boot.dsmooth<-gam(nbizvalpos~s(nyear),family=quasibinomial(link="probit"),gamma=smooth,data=scfpop,weights=scfpop[,k])
    boot.pd<-predict(boot.dsmooth,newdata=cmatrix,type="response")
    scfbiz$nfirmsize<-as.numeric(scfbiz$nfirmsize)-1
    boot.xsmooth<-gam(list(nfirmsize~s(nyear),~s(nyear)),family=multinom(K=2),gamma=smooth,data=scfbiz,weights=scfbiz[,k])
    boot.px<-predict(boot.xsmooth,newdata=cmatrix,type="response")
    boot.px0<-boot.px[,1]	
    boot.px1<-boot.px[,2]
    boot.px2<-boot.px[,3]
    scfbiz$nfirmsize<-as.factor(scfbiz$nfirmsize)
    boot.ysmooth<-gam(nlogbizval~nfirmsize+s(nyear,by=nfirmsize),family=gaussian(link="identity"),gamma=smooth,data=scfbiz,weights=scfbiz[,k])
    boot.mu<-predict(boot.ysmooth,newdata=cmatrix,type="response")
    scfbiz$boot.ehatsq<-residuals(boot.ysmooth,type="response")^2
    boot.esmooth<-gam(boot.ehatsq~nfirmsize+s(nyear,by=nfirmsize),family=Gamma(link="log"),gamma=smooth,data=scfbiz,weights=scfbiz[,k])
    boot.sigmasq<-exp(predict(boot.esmooth,newdata=cmatrix))
    boot.params<-cbind(cmatrix,boot.px0,boot.px1,boot.px2,boot.pd,boot.mu,boot.sigmasq)
    boot.top1pct.mod<-boot.top1pct.com<-boot.top1pct.btw<-NULL
    for (j in seq(1989,2019,by=3)) {
      boot.sim<-data.frame(matrix(data=NA,nrow=nsim,ncol=3))
      setnames(boot.sim,old=c("X1","X2","X3"),new=c("x","d","y"))
      boot.d<-rbinom(nsim,1,boot.params[which(boot.params$nyear==j & boot.params$nfirmsize=="0"),"boot.pd"])
      boot.sim$d<-boot.d
      boot.xcounts<-rmultinom(1,sum(boot.d),boot.params[which(boot.params$nyear==j & boot.params$nfirmsize=="0"),c("boot.px0","boot.px1","boot.px2")])
      boot.x1d1<-rep(1,boot.xcounts[1,1])
      boot.x2d1<-rep(2,boot.xcounts[2,1])
      boot.x3d1<-rep(3,boot.xcounts[3,1])
      boot.xd1<-c(boot.x1d1,boot.x2d1,boot.x3d1)
      boot.sim$x[boot.sim$d==0]<-0
      boot.sim$x[boot.sim$d==1]<-boot.xd1
      boot.yposx1<-exp(rnorm(sum(boot.d)-(boot.xcounts[2,1]+boot.xcounts[3,1]),boot.params[which(boot.params$nyear==j & boot.params$nfirmsize=="0"),"boot.mu"],sqrt(boot.params[which(boot.params$nyear==j & boot.params$nfirmsize=="0"),"boot.sigmasq"])*adjust))
      boot.yposx2<-exp(rnorm(sum(boot.d)-(boot.xcounts[1,1]+boot.xcounts[3,1]),boot.params[which(boot.params$nyear==j & boot.params$nfirmsize=="1"),"boot.mu"],sqrt(boot.params[which(boot.params$nyear==j & boot.params$nfirmsize=="1"),"boot.sigmasq"])*adjust))
      boot.yposx3<-exp(rnorm(sum(boot.d)-(boot.xcounts[1,1]+boot.xcounts[2,1]),boot.params[which(boot.params$nyear==j & boot.params$nfirmsize=="2"),"boot.mu"],sqrt(boot.params[which(boot.params$nyear==j & boot.params$nfirmsize=="2"),"boot.sigmasq"])*adjust))
      boot.sim$y[boot.sim$d==0]<-0
      boot.sim$y[boot.sim$d==1 & boot.sim$x==1]<-boot.yposx1
      boot.sim$y[boot.sim$d==1 & boot.sim$x==2]<-boot.yposx2
      boot.sim$y[boot.sim$d==1 & boot.sim$x==3]<-boot.yposx3
      boot.q99<-quantile(boot.sim$y,probs=0.99)
      boot.toty<-sum(boot.sim$y)
      boot.totyq99<-sum(boot.sim$y[boot.sim$y>=boot.q99])
      boot.share1pct<-(boot.totyq99/boot.toty)
      boot.top1pct.mod<-c(boot.top1pct.mod,boot.share1pct)
      }  
    for (j in seq(1989,2019,by=3)) {
      boot.sim<-data.frame(matrix(data=NA,nrow=nsim,ncol=3))
      setnames(boot.sim,old=c("X1","X2","X3"),new=c("x","d","y"))
      boot.d<-rbinom(nsim,1,boot.params[which(boot.params$nyear==j & boot.params$nfirmsize=="0"),"boot.pd"])
      boot.sim$d<-boot.d
      boot.xcounts<-rmultinom(1,sum(boot.d),boot.params[which(boot.params$nyear==1989 & boot.params$nfirmsize=="0"),c("boot.px0","boot.px1","boot.px2")])
      boot.x1d1<-rep(1,boot.xcounts[1,1])
      boot.x2d1<-rep(2,boot.xcounts[2,1])
      boot.x3d1<-rep(3,boot.xcounts[3,1])
      boot.xd1<-c(boot.x1d1,boot.x2d1,boot.x3d1)
      boot.sim$x[boot.sim$d==0]<-0
      boot.sim$x[boot.sim$d==1]<-boot.xd1
      boot.yposx1<-exp(rnorm(sum(boot.d)-(boot.xcounts[2,1]+boot.xcounts[3,1]),boot.params[which(boot.params$nyear==j & boot.params$nfirmsize=="0"),"boot.mu"],sqrt(boot.params[which(boot.params$nyear==j & boot.params$nfirmsize=="0"),"boot.sigmasq"])*adjust))
      boot.yposx2<-exp(rnorm(sum(boot.d)-(boot.xcounts[1,1]+boot.xcounts[3,1]),boot.params[which(boot.params$nyear==j & boot.params$nfirmsize=="1"),"boot.mu"],sqrt(boot.params[which(boot.params$nyear==j & boot.params$nfirmsize=="1"),"boot.sigmasq"])*adjust))
      boot.yposx3<-exp(rnorm(sum(boot.d)-(boot.xcounts[1,1]+boot.xcounts[2,1]),boot.params[which(boot.params$nyear==j & boot.params$nfirmsize=="2"),"boot.mu"],sqrt(boot.params[which(boot.params$nyear==j & boot.params$nfirmsize=="2"),"boot.sigmasq"])*adjust))
      boot.sim$y[boot.sim$d==0]<-0
      boot.sim$y[boot.sim$d==1 & boot.sim$x==1]<-boot.yposx1
      boot.sim$y[boot.sim$d==1 & boot.sim$x==2]<-boot.yposx2
      boot.sim$y[boot.sim$d==1 & boot.sim$x==3]<-boot.yposx3
      boot.totyq99<-sum(boot.sim$y[boot.sim$y>=boot.q99])
      boot.share1pct<-(boot.totyq99/boot.toty)
      boot.top1pct.com<-c(boot.top1pct.com,boot.share1pct)
      }  
    for (j in seq(1989,2019,by=3)) {
      boot.sim<-data.frame(matrix(data=NA,nrow=nsim,ncol=3))
      setnames(boot.sim,old=c("X1","X2","X3"),new=c("x","d","y"))
      boot.d<-rbinom(nsim,1,boot.params[which(boot.params$nyear==j & boot.params$nfirmsize=="0"),"boot.pd"])
      boot.sim$d<-boot.d
      boot.xcounts<-rmultinom(1,sum(boot.d),boot.params[which(boot.params$nyear==1989 & boot.params$nfirmsize=="0"),c("boot.px0","boot.px1","boot.px2")])
      boot.x1d1<-rep(1,boot.xcounts[1,1])
      boot.x2d1<-rep(2,boot.xcounts[2,1])
      boot.x3d1<-rep(3,boot.xcounts[3,1])
      boot.xd1<-c(boot.x1d1,boot.x2d1,boot.x3d1)
      boot.sim$x[boot.sim$d==0]<-0
      boot.sim$x[boot.sim$d==1]<-boot.xd1
      boot.muPopYR<-
        (boot.params[which(boot.params$nyear==j & boot.params$nfirmsize=="0"),"boot.px0"]*boot.params[which(boot.params$nyear==j & boot.params$nfirmsize=="0"),"boot.mu"])+
        (boot.params[which(boot.params$nyear==j & boot.params$nfirmsize=="1"),"boot.px1"]*boot.params[which(boot.params$nyear==j & boot.params$nfirmsize=="1"),"boot.mu"])+
        (boot.params[which(boot.params$nyear==j & boot.params$nfirmsize=="2"),"boot.px2"]*boot.params[which(boot.params$nyear==j & boot.params$nfirmsize=="2"),"boot.mu"])
      boot.muPop89<-
        (boot.params[which(boot.params$nyear==1989 & boot.params$nfirmsize=="0"),"boot.px0"]*boot.params[which(boot.params$nyear==1989 & boot.params$nfirmsize=="0"),"boot.mu"])+
        (boot.params[which(boot.params$nyear==1989 & boot.params$nfirmsize=="1"),"boot.px1"]*boot.params[which(boot.params$nyear==1989 & boot.params$nfirmsize=="1"),"boot.mu"])+
        (boot.params[which(boot.params$nyear==1989 & boot.params$nfirmsize=="2"),"boot.px2"]*boot.params[which(boot.params$nyear==1989 & boot.params$nfirmsize=="2"),"boot.mu"])
      boot.mu1<-boot.params[which(boot.params$nyear==1989 & boot.params$nfirmsize=="0"),"boot.mu"]+(boot.muPopYR-boot.muPop89)
      boot.mu2<-boot.params[which(boot.params$nyear==1989 & boot.params$nfirmsize=="1"),"boot.mu"]+(boot.muPopYR-boot.muPop89)
      boot.mu3<-boot.params[which(boot.params$nyear==1989 & boot.params$nfirmsize=="2"),"boot.mu"]+(boot.muPopYR-boot.muPop89)
      boot.yposx1<-exp(rnorm(sum(boot.d)-(boot.xcounts[2,1]+boot.xcounts[3,1]),boot.mu1,sqrt(boot.params[which(boot.params$nyear==j & boot.params$nfirmsize=="0"),"boot.sigmasq"])*adjust))
      boot.yposx2<-exp(rnorm(sum(boot.d)-(boot.xcounts[1,1]+boot.xcounts[3,1]),boot.mu2,sqrt(boot.params[which(boot.params$nyear==j & boot.params$nfirmsize=="1"),"boot.sigmasq"])*adjust))
      boot.yposx3<-exp(rnorm(sum(boot.d)-(boot.xcounts[1,1]+boot.xcounts[2,1]),boot.mu3,sqrt(boot.params[which(boot.params$nyear==j & boot.params$nfirmsize=="2"),"boot.sigmasq"])*adjust))
      boot.sim$y[boot.sim$d==0]<-0
      boot.sim$y[boot.sim$d==1 & boot.sim$x==1]<-boot.yposx1
      boot.sim$y[boot.sim$d==1 & boot.sim$x==2]<-boot.yposx2
      boot.sim$y[boot.sim$d==1 & boot.sim$x==3]<-boot.yposx3
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

# COMBINE MI ESTIMATES #
micompx0<-micompx1<-micompx2<-micompd<-micommu<-micomsg<-matrix(data=NA,nrow=length(cmatrix[,1]),ncol=1)
for (i in 1:length(cmatrix[,1])) { 
	micompx0[i,1]<-mean(miestpx0[i,])
	micompx1[i,1]<-mean(miestpx1[i,])
	micompx2[i,1]<-mean(miestpx2[i,])
	micompd[i,1]<-mean(miestpd[i,])
	micommu[i,1]<-mean(miestmu[i,])
	micomsg[i,1]<-mean(miestsg[i,])
	}
trends.parm<-data.frame(cbind(cmatrix,micompx0,micompx1,micompx2,micompd,micommu,micomsg))
setnames(trends.parm,old=c("nyear","nfirmsize","micompx0","micompx1","micompx2","micompd","micommu","micomsg"),new=c("year","firmsize","px0","px1","px2","pd","mu","sigmasq"))

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
tiff("C:\\Users\\Geoff\\Dropbox\\D\\projects\\scf_biz_assets\\figures\\figure_E3.tiff",
     width=9,
     height=9,
     units='in',
     res=600)

par(mfrow=c(2,2))

plot(trends.parm$year[trends.parm$firmsize=="0"],trends.parm$px0[trends.parm$firmsize=="0"],
     type="l",
     ylim=c(0.0,1.0),
     xlab="Year",
     ylab="P(A|Y>0)",
     main="Distribution of Firm Size")
lines(trends.parm$year[trends.parm$firmsize=="0"],trends.parm$px1[trends.parm$firmsize=="0"],type="l",lty="dotted")
lines(trends.parm$year[trends.parm$firmsize=="0"],trends.parm$px2[trends.parm$firmsize=="0"],type="l",lty="dashed")
labels<-c("0-9 Employees (A=1)","10-99 Employees (A=2)","100+ Employees (A=3)")
type<-c("solid","dotted","dashed")
legend("topleft",inset=0.03,labels,lty=type,seg.len=2,cex=0.85)

plot(trends.parm$year[trends.parm$firmsize=="0"],trends.parm$mu[trends.parm$firmsize=="0"],
     type="l",
     ylim=c(10.0,16.0),
     xlab="Year",
     ylab="E(log(Y)|Y>0,A)",
     main="Mean Asset Value by Firm Size")
lines(trends.parm$year[trends.parm$firmsize=="1"],trends.parm$mu[trends.parm$firmsize=="1"],type="l",lty="dotted")
lines(trends.parm$year[trends.parm$firmsize=="2"],trends.parm$mu[trends.parm$firmsize=="2"],type="l",lty="dashed")
labels<-c("0-9 Employees (A=1)","10-99 Employees (A=2)","100+ Employees (A=3)")
type<-c("solid","dotted","dashed")
legend("topleft",inset=0.03,labels,lty=type,seg.len=2,cex=0.85)

plot(trends.parm$year[trends.parm$firmsize=="0"],trends.parm$sigmasq[trends.parm$firmsize=="0"],
     type="l",
     ylim=c(2,7),
     xlab="Year",
     ylab="Var(log(Y)|Y>0,A)",
     main="Asset Variance by Firm Size")
lines(trends.parm$year[trends.parm$firmsize=="1"],trends.parm$sigmasq[trends.parm$firmsize=="1"],type="l",lty="dotted")
lines(trends.parm$year[trends.parm$firmsize=="2"],trends.parm$sigmasq[trends.parm$firmsize=="2"],type="l",lty="dashed")
labels<-c("0-9 Employees (A=1)","10-99 Employees (A=2)","100+ Employees (A=3)")
type<-c("solid","dotted","dashed")
legend("topleft",inset=0.03,labels,lty=type,seg.len=2,cex=0.85)

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
with(trends.com[which(trends.com$shr1pct.pval>=0.05 & trends.com$shr1pct.pval<0.1),], points(year,shr1pct,pch=2,cex=1.2))
with(trends.btw[which(trends.btw$shr1pct.pval>=0.05 & trends.btw$shr1pct.pval<0.1),], points(year,shr1pct,pch=2,cex=1.2))
labels<-c("Compositional Effect","Compositional + Between Group Effect")
type<-c("dotdash","longdash")
legend("topleft",inset=0.03,labels,lty=type,seg.len=2,cex=0.85)

dev.off()

sink()
