sink("C:\\Users\\Geoff\\Dropbox\\D\\projects\\scf_biz_assets\\programs\\_LOGS\\24_create_figure_C1_log.txt")

##########################################################
##########################################################
##                                                      ##
## PROGRAM NAME: 24_create_figure_C1                    ##
## DESCRIPTION:                                         ##
##                                                      ##  
##  trends in distn of nonmissing business assets       ##
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
library(weights)
library(Hmisc)

### INPUT DATA ###
scfmi<-read.dta("C:\\Users\\Geoff\\Dropbox\\D\\projects\\scf_biz_assets\\data\\v01_scf_final_merged.dta")
scfmi<-scfmi[which(scfmi$"ibizval"==0),]
nyear<-data.frame(nyear=c(1989,1992,1995,1998,2001,2004,2007,2010,2013,2016,2019))
nsim<-10000000
adjust<-0.9
smooth<-10

### COMPUTE LOG-NORMAL HURDLE MODEL ESTIMATES ###
miestpr.mod<-miestmu.mod<-miestsg.mod<-matrix(data=NA,nrow=length(nyear[,1]),ncol=5)
miestpr.obs<-miestmu.obs<-miestsg.obs<-matrix(data=NA,nrow=length(nyear[,1]),ncol=5)

for (i in 1:5) {

	scfpop<-scfmi[which(scfmi$"nminum"==i),]
	scfbiz<-scfmi[which(scfmi$"nminum"==i & scfmi$"nbizvalpos"==1),]
	m1<-gam(nbizvalpos~s(nyear),family=quasibinomial(link="probit"),gamma=smooth,data=scfpop,weights=nsampwt)
      p<-predict(m1,newdata=nyear,type="response")
      m2<-gam(nlogbizval~s(nyear),family=gaussian(link="identity"),gamma=smooth,data=scfbiz,weights=nsampwt)
      mu<-predict(m2,newdata=nyear,type="response")
      scfbiz$ehatsq<-residuals(m2,type="response")^2
      m3<-gam(ehatsq~s(nyear),family=Gamma(link="log"),gamma=smooth,data=scfbiz,weights=nsampwt)
      sigmasq<-exp(predict(m3,newdata=nyear))
      miestpr.mod[,i]<-p
      miestmu.mod[,i]<-mu
      miestsg.mod[,i]<-sigmasq

	m1.sat<-gam(nbizvalpos~factor(nyear),family=quasibinomial(link="probit"),data=scfpop,weights=nsampwt)
      p.sat<-predict(m1.sat,newdata=nyear,type="response")
      m2.sat<-gam(nlogbizval~factor(nyear),family=gaussian(link="identity"),data=scfbiz,weights=nsampwt)
      mu.sat<-predict(m2.sat,newdata=nyear,type="response")
      scfbiz$ehatsq<-residuals(m2.sat,type="response")^2
      m3.sat<-gam(ehatsq~factor(nyear),family=Gamma(link="log"),data=scfbiz,weights=nsampwt)
      sigmasq.sat<-exp(predict(m3.sat,newdata=nyear))
      miestpr.obs[,i]<-p.sat
      miestmu.obs[,i]<-mu.sat
      miestsg.obs[,i]<-sigmasq.sat
	}

micompr.mod<-micommu.mod<-micomsg.mod<-matrix(data=NA,nrow=length(nyear[,1]),ncol=1)
micompr.obs<-micommu.obs<-micomsg.obs<-matrix(data=NA,nrow=length(nyear[,1]),ncol=1)

for (i in 1:length(nyear[,1])) { 

	micompr.mod[i,1]<-mean(miestpr.mod[i,])
	micommu.mod[i,1]<-mean(miestmu.mod[i,])
	micomsg.mod[i,1]<-mean(miestsg.mod[i,])

	micompr.obs[i,1]<-mean(miestpr.obs[i,])
	micommu.obs[i,1]<-mean(miestmu.obs[i,])
	micomsg.obs[i,1]<-mean(miestsg.obs[i,])

	}

trends.mod<-data.frame(cbind(nyear,micompr.mod,micommu.mod,micomsg.mod))
trends.obs<-data.frame(cbind(nyear,micompr.obs,micommu.obs,micomsg.obs))

setnames(trends.mod,old=c("nyear","micompr.mod","micommu.mod","micomsg.mod"),new=c("year","pr","mu","sigmasq"))
setnames(trends.obs,old=c("nyear","micompr.obs","micommu.obs","micomsg.obs"),new=c("year","pr","mu","sigmasq"))

print(trends.mod)
print(trends.obs)

### COMPUTE OBSERVED TRENDS IN TOP 1% SHARE ###
miestbiz1<-matrix(data=NA,nrow=length(nyear[,1]),ncol=5)

for (i in 1:5) {

	scf<-scfmi[which(scfmi$"nminum"==i),]
	scfbizown<-scfmi[which(scfmi$"nminum"==i & scfmi$"nbizown"==1),]
	q99fitbiz<-rq(ntotbizval~factor(nyear),tau=0.99,data=scf,weights=nsampwt)
	q99hatbiz<-predict(q99fitbiz)
	totbiz<-q99totbiz<-NULL
	for (j in 1:length(nyear[,1])) {
		totbiz<-c(totbiz,sum(scf[which(scf$"nyear"==nyear[j,1]),"nsampwt"]*scf[which(scf$"nyear"==nyear[j,1]),"ntotbizval"]))
		q99totbiz<-c(q99totbiz,sum(scf[which(scf$"nyear"==nyear[j,1] & scf$"ntotbizval">=q99hatbiz),"nsampwt"]*scf[which(scf$"nyear"==nyear[j,1] & scf$"ntotbizval">=q99hatbiz),"ntotbizval"]))
		}
	miestbiz1[,i]<-q99totbiz/totbiz

	}

### COMPUTE MODEL-BASED TRENDS IN TOP 1% SHARE ###
miestbiz1m<-matrix(data=NA,nrow=length(nyear[,1]),ncol=5)

for (i in 1:5) {

  scfpop<-scfmi[(which(scfmi$"nminum"==i)),]
  scfbiz<-scfmi[which(scfmi$"nminum"==i & scfmi$"nbizvalpos"==1),]
  m1<-gam(nbizvalpos~s(nyear),family=quasibinomial(link="probit"),gamma=smooth,data=scfpop,weights=nsampwt)
  p<-predict(m1,newdata=nyear,type="response")
  m2<-gam(nlogbizval~s(nyear),family=gaussian(link="identity"),gamma=smooth,data=scfbiz,weights=nsampwt)
  mu<-predict(m2,newdata=nyear,type="response")
  scfbiz$ehatsq<-residuals(m2,type="response")^2
  m3<-gam(ehatsq~s(nyear),family=Gamma(link="log"),gamma=smooth,data=scfbiz,weights=nsampwt)
  sigmasq<-exp(predict(m3,newdata=nyear))
  params<-cbind(nyear,p,mu,sigmasq)
  top1pct<-NULL
  for (j in c(1989,1992,1995,1998,2001,2004,2007,2010,2013,2016,2019)) {
    sim<-data.frame(matrix(data=NA,nrow=nsim,ncol=2))
    setnames(sim,old=c("X1","X2"),new=c("d","y"))
    d<-rbinom(nsim, 1, params[nyear==j,"p"])
    ypos<-exp(rnorm(sum(d),params[nyear==j,"mu"],sqrt(params[nyear==j,"sigmasq"])*adjust))
    yzeros<-rep(0,nsim-sum(d))
    y<-c(ypos,yzeros)
    sim$d<-d
    sim<-sim[order(-d),]
    sim$y<-y
    q99<-quantile(sim$y,probs=0.99)
    toty<-sum(sim$y)
    totyq99<-sum(sim$y[sim$y>=q99])
    share1pct<-(totyq99/toty)
    top1pct<-c(top1pct,share1pct)
    }
  miestbiz1m[,i]<-top1pct

  }

# COMBINE MI ESTIMATES #
micombiz1<-matrix(data=NA,nrow=length(nyear[,1]),ncol=1)
micombiz1m<-matrix(data=NA,nrow=length(nyear[,1]),ncol=1)

for (i in 1:length(nyear[,1])) { 

	micombiz1[i,1]<-mean(miestbiz1[i,])
	micombiz1m[i,1]<-mean(miestbiz1m[i,])

	}

asset.trends.obs<-data.frame(cbind(nyear,micombiz1))
asset.trends.mod<-data.frame(cbind(nyear,micombiz1m))

setnames(asset.trends.obs,old=c("nyear","micombiz1"),new=c("nyear","biz1"))
setnames(asset.trends.mod,old=c("nyear","micombiz1m"),new=c("nyear","biz1m"))

print(asset.trends.obs)
print(asset.trends.mod)


### PLOT TRENDS ###
tiff("C:\\Users\\Geoff\\Dropbox\\D\\projects\\scf_biz_assets\\figures\\figure_C1.tiff",
     width=9,
     height=9,
     units='in',
     res=600)

par(mfrow=c(2,2))

plot(trends.mod$year,trends.mod$pr,
     type="l",
     ylim=c(0.0,0.2),
     xlab="Year",
     ylab="P(Y>0)",
     main="Proportion of Business Owners")
lines(trends.obs$year,trends.obs$pr,type="l",lty="dashed",lwd=1)
points(trends.obs$year, trends.obs$pr, pch=1, col="black")
labels<-c("Model-based","Observed")
type<-c("solid","dashed")
legend("topleft",inset=0.03,labels,lty=type,seg.len=3,cex=1.0)

plot(trends.mod$year,trends.mod$mu,
     type="l",
     ylim=c(11.0,12.5),
     xlab="Year",
     ylab="E(log(Y)|Y>0)",
     main="Mean Asset Value among Business Owners")
lines(trends.obs$year,trends.obs$mu,type="l",lty="dashed",lwd=1)
points(trends.obs$year, trends.obs$mu, pch=2, col="black")

plot(trends.mod$year,trends.mod$sigmasq,
     type="l",
     ylim=c(3,7),
     xlab="Year",
     ylab="Var(log(Y)|Y>0)",
     main="Asset Variance among Business Owners")
lines(trends.obs$year,trends.obs$sigmasq,type="l",lty="dashed",lwd=1)
points(trends.obs$year, trends.obs$sigmasq, pch=4, col="black")

plot(asset.trends.mod$nyear,asset.trends.mod$biz1m,
     type="l",
     ylim=c(0.60,0.90),
     xlab="Year",
     ylab="Quantile Share",
     main="Top 1% Share of Business Assets")
lines(asset.trends.obs$nyear,asset.trends.obs$biz1,type="l",lty="dashed",lwd=1)
points(asset.trends.obs$nyear,asset.trends.obs$biz1, pch=5, col="black")

dev.off()

sink()
