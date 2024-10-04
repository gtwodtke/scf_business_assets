sink("C:\\Users\\Geoff\\Dropbox\\D\\projects\\scf_biz_assets\\programs\\_LOGS\\19_create_misc_stats_log.txt")

##########################################################
##########################################################
##                                                      ##
## PROGRAM NAME: 19_create_misc_stats                   ##
## DESCRIPTION:                                         ##
##                                                      ##  
##  creates miscellaneous stats mentioned in main text  ##
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
library(Hmisc)
library(reldist)
library(wCorr)

### INPUT DATA ###
scfmi<-read.dta("C:\\Users\\Geoff\\Dropbox\\D\\projects\\scf_biz_assets\\data\\v01_scf_final_merged.dta")

### COMPUTE STATS ###
cat("SAMPLE QUANTILES FOR PRIVATE BUSINESS ASSETS AMONG TOTAL POP\n")
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
print(wtd.quantile(scfmi$ntotbizval,q=0.95,weight=scfmi$nsampwt))
print(wtd.quantile(scfmi$ntotbizval,q=0.99,weight=scfmi$nsampwt))
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
cat(" \n")
cat(" \n")

cat("SAMPLE QUANTILES FOR PRIVATE BUSINESS ASSETS AMONG BUSINESS OWNERS\n")
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
print(wtd.quantile(scfmi$ntotbizval[scfmi$nbizvalpos==1],q=0.10,weight=scfmi$nsampwt[scfmi$nbizvalpos==1]))
print(wtd.quantile(scfmi$ntotbizval[scfmi$nbizvalpos==1],q=0.50,weight=scfmi$nsampwt[scfmi$nbizvalpos==1]))
print(wtd.quantile(scfmi$ntotbizval[scfmi$nbizvalpos==1],q=0.90,weight=scfmi$nsampwt[scfmi$nbizvalpos==1]))
print(wtd.quantile(scfmi$ntotbizval[scfmi$nbizvalpos==1],q=0.95,weight=scfmi$nsampwt[scfmi$nbizvalpos==1]))
print(wtd.quantile(scfmi$ntotbizval[scfmi$nbizvalpos==1],q=0.99,weight=scfmi$nsampwt[scfmi$nbizvalpos==1]))
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
cat(" \n")
cat(" \n")

cat("TOP 1% SHARE OF FINANCIAL ASSETS\n")
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
nyear<-data.frame(nyear=c(1989,1992,1995,1998,2001,2004,2007,2010,2013,2016,2019))
q99fit<-rq(ntotcapval~factor(nyear),tau=0.99,data=scfmi,weights=nsampwt)
q99hat<-predict(q99fit)
totcap<-q99totcap<-NULL
for (j in 1:length(nyear[,1])) {
	totcap<-c(totcap,sum(scfmi[which(scfmi$"nyear"==nyear[j,1]),"nsampwt"]*scfmi[which(scfmi$"nyear"==nyear[j,1]),"ntotcapval"]))
	q99totcap<-c(q99totcap,sum(scfmi[which(scfmi$"nyear"==nyear[j,1] & scfmi$"ntotcapval">=q99hat),"nsampwt"]*scfmi[which(scfmi$"nyear"==nyear[j,1] & scfmi$"ntotcapval">=q99hat),"ntotcapval"]))
	}
cap1pct<-q99totcap/totcap
print(cbind(nyear,cap1pct))
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
cat(" \n")
cat(" \n")

cat("TOP 1% SHARE OF REAL ESTATE ASSETS\n")
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
nyear<-data.frame(nyear=c(1989,1992,1995,1998,2001,2004,2007,2010,2013,2016,2019))
q99fit<-rq(ntotrealval~factor(nyear),tau=0.99,data=scfmi,weights=nsampwt)
q99hat<-predict(q99fit)
totreal<-q99totreal<-NULL
for (j in 1:length(nyear[,1])) {
	totreal<-c(totreal,sum(scfmi[which(scfmi$"nyear"==nyear[j,1]),"nsampwt"]*scfmi[which(scfmi$"nyear"==nyear[j,1]),"ntotrealval"]))
	q99totreal<-c(q99totreal,sum(scfmi[which(scfmi$"nyear"==nyear[j,1] & scfmi$"ntotrealval">=q99hat),"nsampwt"]*scfmi[which(scfmi$"nyear"==nyear[j,1] & scfmi$"ntotrealval">=q99hat),"ntotrealval"]))
	}
real1pct<-q99totreal/totreal
print(cbind(nyear,real1pct))
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
cat(" \n")
cat(" \n")

cat("SAMPLE PROPORTION BUSINESS OWNERS WHO INHERITED FIRM\n")
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
summary(scfmi$ninhfirm[scfmi$nbizown==1])
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
cat(" \n")
cat(" \n")

cat("SAMPLE PROPORTION BUSINESS OWNERS WITH INCORPORATED FIRMS\n")
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
summary(scfmi$ncorp[scfmi$nbizown==1])
summary(scfmi$nsoleprop[scfmi$nbizown==1])
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")

cat("SPEARMAN CORR OF BIZ ASSETS WITH BIZ INCOME\n")
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
scfbiz<-scfmi[which(scfmi$"nbizvalpos"==1),]
spearmanCorr<-weightedCorr(x=scfbiz$nbizinc,y=scfbiz$ntotbizval,weights=scfbiz$nsampwt,method="Spearman")
print(spearmanCorr)
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")

cat("PEARSON CORR OF BIZ ASSETS WITH BIZ INCOME AFTER IHS TRANSFORM\n")
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
scfbiz$nihsbizval<-log(scfbiz$ntotbizval+sqrt((scfbiz$ntotbizval^2)+1))
pearsonCorr<-weightedCorr(x=scfbiz$nihsbizinc,y=scfbiz$nihsbizval,weights=scfbiz$nsampwt,method="Pearson")
print(pearsonCorr)
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")

cat("PEARSON CORR OF BIZ ASSETS WITH BIZ INCOME AFTER LOG TRANSFORM (DROPPING NET NEGATIVE INCS)\n")
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
scfbiz$nlogbizinc<-NA
scfbiz$nlogbizinc[scfbiz$nbizinc>0]<-log(scfbiz$nbizinc[scfbiz$nbizinc>0])
scfbiz<-scfbiz[complete.cases(scfbiz[,c("nlogbizinc", "nlogbizval")]),]
pearsonLnCorr<-weightedCorr(x=scfbiz$nlogbizinc,y=scfbiz$nlogbizval,weights=scfbiz$nsampwt,method="Pearson")
print(pearsonLnCorr)
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")

sink()
