sink("C:\\Users\\Geoff\\Dropbox\\D\\projects\\scf_biz_assets\\programs\\_LOGS\\10_create_figure_4_log.txt")

##########################################################
##########################################################
##                                                      ##
## PROGRAM NAME: 10_create_figure_4                     ##
## DESCRIPTION:                                         ##
##                                                      ##  
##  trends in total wealth inequality                   ##
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

### TOP 1% SHARE OF OBSERVED WEALTH ###
miestobs<-matrix(data=NA,nrow=length(nyear[,1]),ncol=5)

for (i in 1:5) {
	scf<-scfmi[which(scfmi$"nminum"==i),]
	q99fit<-rq(ntotwealth~factor(nyear),tau=0.99,data=scf,weights=nsampwt)
	q99hat<-predict(q99fit)
	tot<-q99tot<-NULL
	for (j in 1:length(nyear[,1])) {
		tot<-c(tot,sum(scf[which(scf$"nyear"==nyear[j,1]),"nsampwt"]*scf[which(scf$"nyear"==nyear[j,1]),"ntotwealth"]))
		q99tot<-c(q99tot,sum(scf[which(scf$"nyear"==nyear[j,1] & scf$"ntotwealth">=q99hat),"nsampwt"]*scf[which(scf$"nyear"==nyear[j,1] & scf$"ntotwealth">=q99hat),"ntotwealth"]))
		}
	miestobs[,i]<-q99tot/tot
	}

### TOP 1% SHARE OF WEALTH UNDER NO PRIVATE BUSINESS ASSETS ###
miestcf1<-matrix(data=NA,nrow=length(nyear[,1]),ncol=5)

for (i in 1:5) {
	scf<-scfmi[which(scfmi$"nminum"==i),]
	q99fit<-rq(naltwealth~factor(nyear),tau=0.99,data=scf,weights=nsampwt)
	q99hat<-predict(q99fit)
	tot<-q99tot<-NULL
	for (j in 1:length(nyear[,1])) {
		tot<-c(tot,sum(scf[which(scf$"nyear"==nyear[j,1]),"nsampwt"]*scf[which(scf$"nyear"==nyear[j,1]),"naltwealth"]))
		q99tot<-c(q99tot,sum(scf[which(scf$"nyear"==nyear[j,1] & scf$"naltwealth">=q99hat),"nsampwt"]*scf[which(scf$"nyear"==nyear[j,1] & scf$"naltwealth">=q99hat),"naltwealth"]))
		}
	miestcf1[,i]<-q99tot/tot
	}

### TOP 1% SHARE OF WEALTH UNDER EQUALIZED PRIVATE BUSINESS ASSETS ###
miestcf2<-matrix(data=NA,nrow=length(nyear[,1]),ncol=5)

for (j in 1:length(nyear[,1])) {
	scfmi$naltwealth<-scfmi$naltwealth+wtd.mean(scfmi$ntotbizval,weights=scfmi$nsampwt)
	}

for (i in 1:5) {
	scf<-scfmi[which(scfmi$"nminum"==i),]
	q99fit<-rq(naltwealth~factor(nyear),tau=0.99,data=scf,weights=nsampwt)
	q99hat<-predict(q99fit)
	tot<-q99tot<-NULL
	for (j in 1:length(nyear[,1])) {
		tot<-c(tot,sum(scf[which(scf$"nyear"==nyear[j,1]),"nsampwt"]*scf[which(scf$"nyear"==nyear[j,1]),"naltwealth"]))
		q99tot<-c(q99tot,sum(scf[which(scf$"nyear"==nyear[j,1] & scf$"naltwealth">=q99hat),"nsampwt"]*scf[which(scf$"nyear"==nyear[j,1] & scf$"naltwealth">=q99hat),"naltwealth"]))
		}
	miestcf2[,i]<-q99tot/tot
	}

# COMBINE MI ESTIMATES #
miobs<-micf1<-micf2<-matrix(data=NA,nrow=length(nyear[,1]),ncol=2)

for (i in 1:length(nyear[,1])) { 
	miobs[i,1]<-mean(miestobs[i,])
	micf1[i,1]<-mean(miestcf1[i,])
	micf2[i,1]<-mean(miestcf2[i,])

	miobs[i,2]<-mean(miestobs[i,])-mean(miestobs[1,])
	micf1[i,2]<-mean(miestcf1[i,])-mean(miestcf1[1,])
	micf2[i,2]<-mean(miestcf2[i,])-mean(miestcf2[1,])
	}

wealth.trends.obs<-data.frame(cbind(nyear,miobs))
wealth.trends.cf1<-data.frame(cbind(nyear,micf1))
wealth.trends.cf2<-data.frame(cbind(nyear,micf2))

setnames(wealth.trends.obs,old=c("nyear","X1","X2"),new=c("nyear","top1","diff"))
setnames(wealth.trends.cf1,old=c("nyear","X1","X2"),new=c("nyear","top1","diff"))
setnames(wealth.trends.cf2,old=c("nyear","X1","X2"),new=c("nyear","top1","diff"))

print(wealth.trends.obs)
print(wealth.trends.cf1)
print(wealth.trends.cf2)

### PLOT TRENDS ###
tiff("C:\\Users\\Geoff\\Dropbox\\D\\projects\\scf_biz_assets\\figures\\figure_4.tiff",
     width=9,
     height=4.5,
     units='in',
     res=600)

par(mfrow=c(1,2))

plot(wealth.trends.obs$nyear,wealth.trends.obs$top1,
     type="l",
     ylim=c(0.0,0.53),
     xlab="Year",
     ylab="Quantile Share",
     main="Top 1% Share of Total Wealth",
     cex.lab=0.8,
     cex.axis=0.8)
lines(wealth.trends.cf1$nyear,wealth.trends.cf1$top1,type="l",lty="longdash",lwd=1)
lines(wealth.trends.cf2$nyear,wealth.trends.cf2$top1,type="l",lty="dotted",lwd=1)

labels<-c("Total Wealth","Excluding Bus. Assets","Equalizing Bus. Assets")
type<-c("solid","longdash","dotted")
legend("topleft",inset=0.025,labels,lty=type,seg.len=3,cex=0.70)

plot(wealth.trends.obs$nyear,wealth.trends.obs$diff,
     type="l",
     ylim=c(-0.0025,0.10),
     xlab="Year",
     ylab="Change in Quantile Share",
     main="Change in Top 1% Share of Total Wealth",
     cex.lab=0.8,
     cex.axis=0.8)
lines(wealth.trends.cf1$nyear,wealth.trends.cf1$diff,type="l",lty="longdash",lwd=1)
lines(wealth.trends.cf2$nyear,wealth.trends.cf2$diff,type="l",lty="dotted",lwd=1)

labels<-c("Total Wealth","Excluding Bus. Assets","Equalizing Bus. Assets")
type<-c("solid","longdash","dotted")
legend("topleft",inset=0.025,labels,lty=type,seg.len=3,cex=0.70)

dev.off()

sink()
