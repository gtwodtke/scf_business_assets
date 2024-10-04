sink("C:\\Users\\Geoff\\Dropbox\\D\\projects\\scf_biz_assets\\programs\\_LOGS\\26_create_figure_E1_log.txt")

##########################################################
##########################################################
##                                                      ##
## PROGRAM NAME: 26_create_figure_E1                    ##
## DESCRIPTION:                                         ##
##                                                      ##  
##  plots log-normal hurdle models for effects of educ  ##
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
educ<-c(rep("lt_udegree",length(year)/2),rep("udegree",length(year)/2))
cmatrix<-data.frame(nyear=year,neducyr=educ)
set.seed(60657)
nsim<-10000000
nboot<-200
smooth<-10
adjust<-0.9

### COMPUTE HURDLE MODELS ###
miestpx<-miestpd<-miestmu<-miestsg<-matrix(data=NA,nrow=length(cmatrix[,1]),ncol=5)

for (i in 1:5) {
  scfpop<-scfmi[(which(scfmi$"nminum"==i)),]
  scfbiz<-scfmi[which(scfmi$"nminum"==i & scfmi$"nbizvalpos"==1),]
  xsmooth<-gam(neducyr~s(nyear),family=quasibinomial(link="probit"),gamma=smooth,data=scfpop,weights=nsampwt)
  px<-predict(xsmooth,newdata=cmatrix,type="response")
  dsmooth<-gam(nbizvalpos~neducyr+s(nyear,by=neducyr),family=quasibinomial(link="probit"),gamma=smooth,data=scfpop,weights=nsampwt)
  pd<-predict(dsmooth,newdata=cmatrix,type="response")
  ysmooth<-gam(nlogbizval~neducyr+s(nyear,by=neducyr),family=gaussian(link="identity"),gamma=smooth,data=scfbiz,weights=nsampwt)
  mu<-predict(ysmooth,newdata=cmatrix,type="response")
  scfbiz$ehatsq<-residuals(ysmooth,type="response")^2
  esmooth<-gam(ehatsq~neducyr+s(nyear,by=neducyr),family=Gamma(link="log"),gamma=smooth,data=scfbiz,weights=nsampwt)
  sigmasq<-exp(predict(esmooth,newdata=cmatrix))
  params<-cbind(cmatrix,px,pd,mu,sigmasq)
  miestpx[,i]<-px
  miestpd[,i]<-pd
  miestmu[,i]<-mu
  miestsg[,i]<-sigmasq
  }

# COMBINE MI ESTIMATES #
micompx<-micompd<-micommu<-micomsg<-matrix(data=NA,nrow=length(cmatrix[,1]),ncol=1)

for (i in 1:length(cmatrix[,1])) { 
	micompx[i,1]<-mean(miestpx[i,])
	micompd[i,1]<-mean(miestpd[i,])
	micommu[i,1]<-mean(miestmu[i,])
	micomsg[i,1]<-mean(miestsg[i,])
	}
	
trends.parm<-data.frame(cbind(cmatrix,micompx,micompd,micommu,micomsg))
setnames(trends.parm,old=c("nyear","neducyr","micompx","micompd","micommu","micomsg"),new=c("year","educ","px","pd","mu","sigmasq"))
print(trends.parm)

### COMPUTE MODEL-BASED TRENDS IN QUANTILE SHARE ###
miest1pct.com<-miest1pct.btw<-mivar1pct.com<-mivar1pct.btw<-matrix(data=NA,nrow=length(seq(1989,2019,by=3)),ncol=5)

for (i in 1:5) {

  scfpop<-scfmi[(which(scfmi$"nminum"==i)),]
  scfbiz<-scfmi[which(scfmi$"nminum"==i & scfmi$"nbizvalpos"==1),]
  xsmooth<-gam(neducyr~s(nyear),family=quasibinomial(link="probit"),gamma=smooth,data=scfpop,weights=nsampwt)
  px<-predict(xsmooth,newdata=cmatrix,type="response")
  dsmooth<-gam(nbizvalpos~neducyr+s(nyear,by=neducyr),family=quasibinomial(link="probit"),gamma=smooth,data=scfpop,weights=nsampwt)
  pd<-predict(dsmooth,newdata=cmatrix,type="response")
  ysmooth<-gam(nlogbizval~neducyr+s(nyear,by=neducyr),family=gaussian(link="identity"),gamma=smooth,data=scfbiz,weights=nsampwt)
  mu<-predict(ysmooth,newdata=cmatrix,type="response")
  scfbiz$ehatsq<-residuals(ysmooth,type="response")^2
  esmooth<-gam(ehatsq~neducyr+s(nyear,by=neducyr),family=Gamma(link="log"),gamma=smooth,data=scfbiz,weights=nsampwt)
  sigmasq<-exp(predict(esmooth,newdata=cmatrix))
  params<-cbind(cmatrix,px,pd,mu,sigmasq)
  
  top1pct.mod<-top1pct.com<-top1pct.btw<-NULL
  for (j in seq(1989,2019,by=3)) {
    sim<-data.frame(matrix(data=NA,nrow=nsim,ncol=3))
    setnames(sim,old=c("X1","X2","X3"),new=c("x","d","y"))
    x<-rbinom(nsim,1,params[which(params$nyear==j & params$neducyr=="udegree"),"px"])
    sim$x<-x
    dx0<-rbinom(nsim-sum(x),1,params[which(params$nyear==j & params$neducyr=="lt_udegree"),"pd"])
    dx1<-rbinom(sum(x),1,params[which(params$nyear==j & params$neducyr=="udegree"),"pd"])
    sim$d[sim$x==0]<-dx0
    sim$d[sim$x==1]<-dx1
    yposx0<-exp(rnorm(sum(dx0),params[which(params$nyear==j & params$neducyr=="lt_udegree"),"mu"],sqrt(params[which(params$nyear==j & params$neducyr=="lt_udegree"),"sigmasq"])*adjust))
    yposx1<-exp(rnorm(sum(dx1),params[which(params$nyear==j & params$neducyr=="udegree"),"mu"],sqrt(params[which(params$nyear==j & params$neducyr=="udegree"),"sigmasq"])*adjust))
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
    x<-rbinom(nsim,1,params[which(params$nyear==1989 & params$neducyr=="udegree"),"px"])
    sim$x<-x
    dx0<-rbinom(nsim-sum(x),1,params[which(params$nyear==j & params$neducyr=="lt_udegree"),"pd"])
    dx1<-rbinom(sum(x),1,params[which(params$nyear==j & params$neducyr=="udegree"),"pd"])
    sim$d[sim$x==0]<-dx0
    sim$d[sim$x==1]<-dx1
    yposx0<-exp(rnorm(sum(dx0),params[which(params$nyear==j & params$neducyr=="lt_udegree"),"mu"],sqrt(params[which(params$nyear==j & params$neducyr=="lt_udegree"),"sigmasq"])*adjust))
    yposx1<-exp(rnorm(sum(dx1),params[which(params$nyear==j & params$neducyr=="udegree"),"mu"],sqrt(params[which(params$nyear==j & params$neducyr=="udegree"),"sigmasq"])*adjust))
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
    x<-rbinom(nsim,1,params[which(params$nyear==1989 & params$neducyr=="udegree"),"px"])
    sim$x<-x
    pdPopYR<-(1-params[which(params$nyear==j & params$neducyr=="lt_udegree"),"px"])*params[which(params$nyear==j & params$neducyr=="lt_udegree"),"pd"]+params[which(params$nyear==j & params$neducyr=="udegree"),"px"]*params[which(params$nyear==j & params$neducyr=="udegree"),"pd"]
    muPopYR<-(1-params[which(params$nyear==j & params$neducyr=="lt_udegree"),"px"])*params[which(params$nyear==j & params$neducyr=="lt_udegree"),"mu"]+params[which(params$nyear==j & params$neducyr=="udegree"),"px"]*params[which(params$nyear==j & params$neducyr=="udegree"),"mu"]
    pdPop89<-(1-params[which(params$nyear==1989 & params$neducyr=="lt_udegree"),"px"])*params[which(params$nyear==1989 & params$neducyr=="lt_udegree"),"pd"]+params[which(params$nyear==1989 & params$neducyr=="udegree"),"px"]*params[which(params$nyear==1989 & params$neducyr=="udegree"),"pd"]
    muPop89<-(1-params[which(params$nyear==1989 & params$neducyr=="lt_udegree"),"px"])*params[which(params$nyear==1989 & params$neducyr=="lt_udegree"),"mu"]+params[which(params$nyear==1989 & params$neducyr=="udegree"),"px"]*params[which(params$nyear==1989 & params$neducyr=="udegree"),"mu"]
    pd0<-params[which(params$nyear==1989 & params$neducyr=="lt_udegree"),"pd"]+(pdPopYR-pdPop89)
    pd1<-params[which(params$nyear==1989 & params$neducyr=="udegree"),"pd"]+(pdPopYR-pdPop89)
    mu0<-params[which(params$nyear==1989 & params$neducyr=="lt_udegree"),"mu"]+(muPopYR-muPop89)
    mu1<-params[which(params$nyear==1989 & params$neducyr=="udegree"),"mu"]+(muPopYR-muPop89)
    dx0<-rbinom(nsim-sum(x),1,pd0)
    dx1<-rbinom(sum(x),1,pd1)
    sim$d[sim$x==0]<-dx0
    sim$d[sim$x==1]<-dx1
    yposx0<-exp(rnorm(sum(dx0),mu0,sqrt(params[which(params$nyear==j & params$neducyr=="lt_udegree"),"sigmasq"])*adjust))
    yposx1<-exp(rnorm(sum(dx1),mu1,sqrt(params[which(params$nyear==j & params$neducyr=="udegree"),"sigmasq"])*adjust))
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
  
  boot.dist.com<-boot.dist.btw<-matrix(data=NA,nrow=length(seq(1989,2019,by=3)),ncol=nboot)
  for (k in 60:(nboot+59)) {
    boot.xsmooth<-gam(neducyr~s(nyear),family=quasibinomial(link="probit"),gamma=smooth,data=scfpop,weights=scfpop[,k])
    boot.px<-predict(boot.xsmooth,newdata=cmatrix,type="response")
    boot.dsmooth<-gam(nbizvalpos~neducyr+s(nyear,by=neducyr),family=quasibinomial(link="probit"),gamma=smooth,data=scfpop,weights=scfpop[,k])
    boot.pd<-predict(boot.dsmooth,newdata=cmatrix,type="response")
    boot.ysmooth<-gam(nlogbizval~neducyr+s(nyear,by=neducyr),family=gaussian(link="identity"),gamma=smooth,data=scfbiz,weights=scfbiz[,k])
    boot.mu<-predict(boot.ysmooth,newdata=cmatrix,type="response")
    scfbiz$boot.ehatsq<-residuals(boot.ysmooth,type="response")^2
    boot.esmooth<-gam(boot.ehatsq~neducyr+s(nyear,by=neducyr),family=Gamma(link="log"),gamma=smooth,data=scfbiz,weights=scfbiz[,k])
    boot.sigmasq<-exp(predict(boot.esmooth,newdata=cmatrix))
    boot.params<-cbind(cmatrix,boot.px,boot.pd,boot.mu,boot.sigmasq)
    boot.top1pct.mod<-boot.top1pct.com<-boot.top1pct.btw<-NULL
    for (j in seq(1989,2019,by=3)) {
      boot.sim<-data.frame(matrix(data=NA,nrow=nsim,ncol=3))
      setnames(boot.sim,old=c("X1","X2","X3"),new=c("x","d","y"))
      boot.x<-rbinom(nsim,1,boot.params[which(boot.params$nyear==j & boot.params$neducyr=="udegree"),"boot.px"])
      boot.sim$x<-boot.x
      boot.dx0<-rbinom(nsim-sum(boot.x),1,boot.params[which(boot.params$nyear==j & boot.params$neducyr=="lt_udegree"),"boot.pd"])
      boot.dx1<-rbinom(sum(boot.x),1,boot.params[which(boot.params$nyear==j & boot.params$neducyr=="udegree"),"boot.pd"])
      boot.sim$d[boot.sim$x==0]<-boot.dx0
      boot.sim$d[boot.sim$x==1]<-boot.dx1
      boot.yposx0<-exp(rnorm(sum(boot.dx0),boot.params[which(boot.params$nyear==j & boot.params$neducyr=="lt_udegree"),"boot.mu"],sqrt(boot.params[which(boot.params$nyear==j & boot.params$neducyr=="lt_udegree"),"boot.sigmasq"])*adjust))
      boot.yposx1<-exp(rnorm(sum(boot.dx1),boot.params[which(boot.params$nyear==j & boot.params$neducyr=="udegree"),"boot.mu"],sqrt(boot.params[which(boot.params$nyear==j & boot.params$neducyr=="udegree"),"boot.sigmasq"])*adjust))
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
      boot.x<-rbinom(nsim,1,boot.params[which(boot.params$nyear==1989 & boot.params$neducyr=="udegree"),"boot.px"])
      boot.sim$x<-boot.x
      boot.dx0<-rbinom(nsim-sum(boot.x),1,boot.params[which(boot.params$nyear==j & boot.params$neducyr=="lt_udegree"),"boot.pd"])
      boot.dx1<-rbinom(sum(boot.x),1,boot.params[which(boot.params$nyear==j & boot.params$neducyr=="udegree"),"boot.pd"])
      boot.sim$d[boot.sim$x==0]<-boot.dx0
      boot.sim$d[boot.sim$x==1]<-boot.dx1
      boot.yposx0<-exp(rnorm(sum(boot.dx0),boot.params[which(boot.params$nyear==j & boot.params$neducyr=="lt_udegree"),"boot.mu"],sqrt(boot.params[which(boot.params$nyear==j & boot.params$neducyr=="lt_udegree"),"boot.sigmasq"])*adjust))
      boot.yposx1<-exp(rnorm(sum(boot.dx1),boot.params[which(boot.params$nyear==j & boot.params$neducyr=="udegree"),"boot.mu"],sqrt(boot.params[which(boot.params$nyear==j & boot.params$neducyr=="udegree"),"boot.sigmasq"])*adjust))
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
      boot.x<-rbinom(nsim,1,boot.params[which(boot.params$nyear==1989 & boot.params$neducyr=="udegree"),"boot.px"])
      boot.sim$x<-boot.x
      boot.pdPopYR<-(1-boot.params[which(boot.params$nyear==j & boot.params$neducyr=="lt_udegree"),"boot.px"])*boot.params[which(boot.params$nyear==j & boot.params$neducyr=="lt_udegree"),"boot.pd"]+boot.params[which(boot.params$nyear==j & boot.params$neducyr=="udegree"),"boot.px"]*boot.params[which(boot.params$nyear==j & boot.params$neducyr=="udegree"),"boot.pd"]
      boot.muPopYR<-(1-boot.params[which(boot.params$nyear==j & boot.params$neducyr=="lt_udegree"),"boot.px"])*boot.params[which(boot.params$nyear==j & boot.params$neducyr=="lt_udegree"),"boot.mu"]+boot.params[which(boot.params$nyear==j & boot.params$neducyr=="udegree"),"boot.px"]*boot.params[which(boot.params$nyear==j & boot.params$neducyr=="udegree"),"boot.mu"]
      boot.pdPop89<-(1-boot.params[which(boot.params$nyear==1989 & boot.params$neducyr=="lt_udegree"),"boot.px"])*boot.params[which(boot.params$nyear==1989 & boot.params$neducyr=="lt_udegree"),"boot.pd"]+boot.params[which(boot.params$nyear==1989 & boot.params$neducyr=="udegree"),"boot.px"]*boot.params[which(boot.params$nyear==1989 & boot.params$neducyr=="udegree"),"boot.pd"]
      boot.muPop89<-(1-boot.params[which(boot.params$nyear==1989 & boot.params$neducyr=="lt_udegree"),"boot.px"])*boot.params[which(boot.params$nyear==1989 & boot.params$neducyr=="lt_udegree"),"boot.mu"]+boot.params[which(boot.params$nyear==1989 & boot.params$neducyr=="udegree"),"boot.px"]*boot.params[which(boot.params$nyear==1989 & boot.params$neducyr=="udegree"),"boot.mu"]
      boot.pd0<-boot.params[which(boot.params$nyear==1989 & boot.params$neducyr=="lt_udegree"),"boot.pd"]+(boot.pdPopYR-boot.pdPop89)
      boot.pd1<-boot.params[which(boot.params$nyear==1989 & boot.params$neducyr=="udegree"),"boot.pd"]+(boot.pdPopYR-boot.pdPop89)
      boot.mu0<-boot.params[which(boot.params$nyear==1989 & boot.params$neducyr=="lt_udegree"),"boot.mu"]+(boot.muPopYR-boot.muPop89)
      boot.mu1<-boot.params[which(boot.params$nyear==1989 & boot.params$neducyr=="udegree"),"boot.mu"]+(boot.muPopYR-boot.muPop89)
      boot.dx0<-rbinom(nsim-sum(boot.x),1,boot.pd0)
      boot.dx1<-rbinom(sum(boot.x),1,boot.pd1)
      boot.sim$d[boot.sim$x==0]<-boot.dx0
      boot.sim$d[boot.sim$x==1]<-boot.dx1
      boot.yposx0<-exp(rnorm(sum(boot.dx0),boot.mu0,sqrt(boot.params[which(boot.params$nyear==j & boot.params$neducyr=="lt_udegree"),"boot.sigmasq"])*adjust))
      boot.yposx1<-exp(rnorm(sum(boot.dx1),boot.mu1,sqrt(boot.params[which(boot.params$nyear==j & boot.params$neducyr=="udegree"),"boot.sigmasq"])*adjust))
      boot.sim$y[boot.sim$d==0]<-0
      boot.sim$y[boot.sim$d==1 & boot.sim$x==0]<-boot.yposx0
      boot.sim$y[boot.sim$d==1 & boot.sim$x==1]<-boot.yposx1
      boot.q99<-quantile(boot.sim$y,probs=0.99)
      boot.toty<-sum(boot.sim$y)
      boot.totyq99<-sum(boot.sim$y[boot.sim$y>=boot.q99])
      boot.share1pct<-(boot.totyq99/boot.toty)
      boot.top1pct.btw<-c(boot.top1pct.btw,boot.share1pct)
      }  
    boot.dist.com[,(k-59)]<-boot.top1pct.com-boot.top1pct.mod
    boot.dist.btw[,(k-59)]<-boot.top1pct.btw-boot.top1pct.mod
    }
  for (m in 1:length(seq(1989,2019,by=3))) { 
    mivar1pct.com[m,i]<-var(boot.dist.com[m,]) 
    mivar1pct.btw[m,i]<-var(boot.dist.btw[m,]) 
    }
  }

# COMBINE MI ESTIMATES #
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

print(trends.com)
print(trends.btw)

### PLOT TRENDS ###
tiff("C:\\Users\\Geoff\\Dropbox\\D\\projects\\scf_biz_assets\\figures\\figure_E1.tiff",
     width=9,
     height=9,
     units='in',
     res=600)

par(mfrow=c(2,2))

plot(trends.parm$year[trends.parm$educ=="lt_udegree"],trends.parm$px[trends.parm$educ=="lt_udegree"],
     type="l",
     ylim=c(0.0,0.5),
     xlab="Year",
     ylab="P(X=1)",
     main="Educational Composition of Total Pop.")

plot(trends.parm$year[trends.parm$educ=="lt_udegree"],trends.parm$mu[trends.parm$educ=="lt_udegree"],
     type="l",
     lty="dotted",
     ylim=c(10,13),
     xlab="Year",
     ylab="E(log(Y)|Y>0,X)",
     main="Mean Asset Value by Education")
lines(trends.parm$year[trends.parm$educ=="udegree"],trends.parm$mu[trends.parm$educ=="udegree"],type="l",lty="dashed")
labels<-c("< College Degree (X=0)",">= College Degree (X=1)")
type<-c("dotted","dashed")
legend("topleft",inset=0.03,labels,lty=type,seg.len=2,cex=1.0)

plot(trends.parm$year[trends.parm$educ=="lt_udegree"],trends.parm$sigmasq[trends.parm$educ=="lt_udegree"],
     type="l",
     lty="dotted",
     ylim=c(2,7),
     xlab="Year",
     ylab="Var(log(Y)|Y>0,X)",
     main="Asset Variance by Education")
lines(trends.parm$year[trends.parm$educ=="udegree"],trends.parm$sigmasq[trends.parm$educ=="udegree"],type="l",lty="dashed")
labels<-c("< College Degree (X=0)",">= College Degree (X=1)")
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
legend("topleft",inset=0.03,labels,lty=type,seg.len=2,cex=0.9)

dev.off()

sink()
