sink("C:\\Users\\Geoff\\Dropbox\\D\\projects\\scf_biz_assets\\programs\\_LOGS\\07_create_figure_1_log.txt")

##########################################################
##########################################################
##                                                      ##
## PROGRAM NAME: 07_create_figure_1                     ##
## DESCRIPTION:                                         ##
##                                                      ##  
##  distribution of private business assets             ##
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

### PLOT DISTNs ###
tiff("C:\\Users\\Geoff\\Dropbox\\D\\projects\\scf_biz_assets\\figures\\figure_1.tiff",
     width=9,
     height=4.5,
     units='in',
     res=600)

par(mfrow=c(1,2))

ticks<-c(0,log(10),log(10^2),log(10^3),log(10^4),log(10^5),log(10^6),log(10^7),log(10^8),log(10^9),log(10^10))
tick_labels<-c("0","10","100","1K","10K","100K","1M","10M","100M","1B","10B")

scfmi$y<-0
scfmi$y[scfmi$nbizvalpos==1]<-scfmi$nlogbizval[scfmi$nbizvalpos==1]

wtd.hist(scfmi$y,weight=scfmi$nsampwt,
     freq=FALSE,
     xlab="2019 Real Dollars (Log Scale)",
     ylab="Density",
     ylim=c(0.0,1.0),
     xlim=c(0,23.1),
     xaxt="n",	
     main="Total Population",
     cex.lab=0.8,
     cex.axis=0.8)

axis(1, at=ticks, labels=tick_labels, cex.axis=0.8, las=2)

wtd.hist(scfmi$nlogbizval[scfmi$nbizvalpos==1],weight=scfmi$nsampwt[scfmi$nbizvalpos==1],
     freq=FALSE,
     xlab="2019 Real Dollars (Log Scale)",
     ylab="Density",
     ylim=c(0.0,0.2),
     xlim=c(0,23.1),
     xaxt="n",	
     main="Business Owners",
     cex.lab=0.8,
     cex.axis=0.8)

axis(1, at=ticks, labels=tick_labels, cex.axis=0.8, las=2)

m<-wtd.mean(scfmi$nlogbizval[scfmi$nbizvalpos==1],scfmi$nsampwt[scfmi$nbizvalpos==1])
var<-wtd.var(scfmi$nlogbizval[scfmi$nbizvalpos==1],scfmi$nsampwt[scfmi$nbizvalpos==1])
den<-dnorm(seq(0,23.1,0.1),mean=m,sd=sqrt(var))
lines(seq(0,23.1,0.1),den,type="l",lwd=2)

dev.off()

sink()
