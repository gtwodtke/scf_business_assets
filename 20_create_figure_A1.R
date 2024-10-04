sink("C:\\Users\\Geoff\\Dropbox\\D\\projects\\scf_biz_assets\\programs\\_LOGS\\20_create_figure_A1_log.txt")

##########################################################
##########################################################
##                                                      ##
## PROGRAM NAME: 20_create_figure_A1                    ##
## DESCRIPTION:                                         ##
##                                                      ##  
##  trends in distribution of inc cap assets measure    ##
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
nyear<-data.frame(nyear=c(1989,1992,1995,1998,2001,2004,2007,2010,2013,2016,2019))
adjust<-0.9
smooth<-10

### COMPUTE PROP, MEAN, AND VAR ESTIMATES ###
miestpr.mod<-miestmu.mod<-miestsg.mod<-matrix(data=NA,nrow=length(nyear[,1]),ncol=5)
miestpr.obs<-miestmu.obs<-miestsg.obs<-matrix(data=NA,nrow=length(nyear[,1]),ncol=5)

for (i in 1:5) {

	scfpop<-scfmi[which(scfmi$"nminum"==i),]
	scfbiz<-scfmi[which(scfmi$"nminum"==i & scfmi$"nbizvalposic"==1),]
	m1<-gam(nbizvalposic~s(nyear),family=quasibinomial(link="probit"),gamma=smooth,data=scfpop,weights=nsampwt)
      p<-predict(m1,newdata=nyear,type="response")
      m2<-gam(nlogbizvalic~s(nyear),family=gaussian(link="identity"),gamma=smooth,data=scfbiz,weights=nsampwt)
      mu<-predict(m2,newdata=nyear,type="response")
      scfbiz$ehatsq<-residuals(m2,type="response")^2
      m3<-gam(ehatsq~s(nyear),family=Gamma(link="log"),gamma=smooth,data=scfbiz,weights=nsampwt)
      sigmasq<-exp(predict(m3,newdata=nyear))
      miestpr.mod[,i]<-p
      miestmu.mod[,i]<-mu
      miestsg.mod[,i]<-sigmasq

	m1.sat<-gam(nbizvalposic~factor(nyear),family=quasibinomial(link="probit"),data=scfpop,weights=nsampwt)
      p.sat<-predict(m1.sat,newdata=nyear,type="response")
      m2.sat<-gam(nlogbizvalic~factor(nyear),family=gaussian(link="identity"),data=scfbiz,weights=nsampwt)
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

### COMPUTE QUANTILE TRENDS ###
miestq90 <- miestq75 <- miestq50 <- miestq25 <- miestq10 <- matrix(data=NA,nrow=length(nyear[,1]),ncol=5)

for (i in 1:5) {
	
	scfbiz <- scfmi[which(scfmi$"nminum"==i & scfmi$"nbizvalposic"==1),]
	
	q90fit <- rq(ntotbizvalic~poly(nyear,2),tau=0.90,data=scfbiz,weights=nsampwt)
	q75fit <- rq(ntotbizvalic~poly(nyear,2),tau=0.75,data=scfbiz,weights=nsampwt)
	q50fit <- rq(ntotbizvalic~poly(nyear,2),tau=0.50,data=scfbiz,weights=nsampwt)
	q25fit <- rq(ntotbizvalic~poly(nyear,2),tau=0.25,data=scfbiz,weights=nsampwt)
	q10fit <- rq(ntotbizvalic~poly(nyear,2),tau=0.10,data=scfbiz,weights=nsampwt)
	
	miestq90[,i] <- predict(q90fit,newdata=nyear)
	miestq75[,i] <- predict(q75fit,newdata=nyear)
	miestq50[,i] <- predict(q50fit,newdata=nyear)
	miestq25[,i] <- predict(q25fit,newdata=nyear)
	miestq10[,i] <- predict(q10fit,newdata=nyear)

	}

micomq90 <- micomq75 <- micomq50 <- micomq25 <- micomq10 <- matrix(data=NA,nrow=length(nyear[,1]),ncol=1)

for (i in 1:length(nyear[,1])) { 

	micomq90[i,1] <- mean(miestq90[i,])/mean(miestq90[1,])
	micomq75[i,1] <- mean(miestq75[i,])/mean(miestq75[1,])
	micomq50[i,1] <- mean(miestq50[i,])/mean(miestq50[1,])
	micomq25[i,1] <- mean(miestq25[i,])/mean(miestq25[1,])
	micomq10[i,1] <- mean(miestq10[i,])/mean(miestq10[1,])
}

trends.qnt <- data.frame(cbind(nyear,micomq90,micomq75,micomq50,micomq25,micomq10))
setnames(trends.qnt,old=c("nyear","micomq90","micomq75","micomq50","micomq25","micomq10"),new=c("year","q90","q75","q50","q25","q10"))
print(trends.qnt)

### PLOT TRENDS ###
tiff("C:\\Users\\Geoff\\Dropbox\\D\\projects\\scf_biz_assets\\figures\\figure_A1.tiff",
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
legend("topleft",inset=0.03,labels,lty=type,seg.len=3,cex=0.9)

plot(trends.mod$year,trends.mod$mu,
     type="l",
     ylim=c(11.0,13.0),
     xlab="Year",
     ylab="E(log(Y)|Y>0)",
     main="Mean Asset Value among Business Owners")
lines(trends.obs$year,trends.obs$mu,type="l",lty="dashed",lwd=1)
points(trends.obs$year, trends.obs$mu, pch=2, col="black")
legend("topleft",inset=0.03,labels,lty=type,seg.len=3,cex=0.9)

plot(trends.mod$year,trends.mod$sigmasq,
     type="l",
     ylim=c(3,7),
     xlab="Year",
     ylab="Var(log(Y)|Y>0)",
     main="Asset Variance among Business Owners")
lines(trends.obs$year,trends.obs$sigmasq,type="l",lty="dashed",lwd=1)
points(trends.obs$year, trends.obs$sigmasq, pch=4, col="black")
legend("topleft",inset=0.03,labels,lty=type,seg.len=3,cex=0.9)

plot(trends.qnt$year,trends.qnt$q90,
     type="l",
     lty="solid",
     ylim=c(0.5,2.5),
     xlab="Year",
     ylab="Multiplicative Change Since 1989",
     main="Trends in Asset Quantiles among \nBusiness Owners")
lines(trends.qnt$year,trends.qnt$q75,type="l",lty="dashed")
lines(trends.qnt$year,trends.qnt$q50,type="l",lty="dotted")
lines(trends.qnt$year,trends.qnt$q25,type="l",lty="dotdash")
lines(trends.qnt$year,trends.qnt$q10,type="l",lty="longdash")

labels <- c("90th Percentile","75th Precentile","50th Percentile","25th Percentile","10th Percentile")
type <- c("solid","dashed","dotted","dotdash","longdash")
legend("topleft",inset=0.03,labels,lty=type,seg.len=3,cex=0.9)

dev.off()

sink()
