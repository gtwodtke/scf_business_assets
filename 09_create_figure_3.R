sink("C:\\Users\\Geoff\\Dropbox\\D\\projects\\scf_biz_assets\\programs\\_LOGS\\09_create_figure_3_log.txt")

##########################################################
##########################################################
##                                                      ##
## PROGRAM NAME: 09_create_figure_3                     ##
## DESCRIPTION:                                         ##
##                                                      ##  
##  trends in quantiles of assets distn                 ##
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

### COMPUTE MI ESTIMATES ###
miestq90 <- miestq75 <- miestq50 <- miestq25 <- miestq10 <- matrix(data=NA,nrow=length(nyear[,1]),ncol=5)

for (i in 1:5) {
	
	scfbiz <- scfmi[which(scfmi$"nminum"==i & scfmi$"nbizvalpos"==1),]
	
	q90fit <- rq(ntotbizval~poly(nyear,2),tau=0.90,data=scfbiz,weights=nsampwt)
	q75fit <- rq(ntotbizval~poly(nyear,2),tau=0.75,data=scfbiz,weights=nsampwt)
	q50fit <- rq(ntotbizval~poly(nyear,2),tau=0.50,data=scfbiz,weights=nsampwt)
	q25fit <- rq(ntotbizval~poly(nyear,2),tau=0.25,data=scfbiz,weights=nsampwt)
	q10fit <- rq(ntotbizval~poly(nyear,2),tau=0.10,data=scfbiz,weights=nsampwt)
	
	miestq90[,i] <- predict(q90fit,newdata=nyear)
	miestq75[,i] <- predict(q75fit,newdata=nyear)
	miestq50[,i] <- predict(q50fit,newdata=nyear)
	miestq25[,i] <- predict(q25fit,newdata=nyear)
	miestq10[,i] <- predict(q10fit,newdata=nyear)

	}

### COMBINE MI ESTIMATES ###
micomq90 <- micomq75 <- micomq50 <- micomq25 <- micomq10 <- matrix(data=NA,nrow=length(nyear[,1]),ncol=1)
micomq90o <- micomq75o <- micomq50o <- micomq25o <- micomq10o <- matrix(data=NA,nrow=length(nyear[,1]),ncol=1)

for (i in 1:length(nyear[,1])) { 

	micomq90[i,1] <- mean(miestq90[i,])/mean(miestq90[1,])
	micomq75[i,1] <- mean(miestq75[i,])/mean(miestq75[1,])
	micomq50[i,1] <- mean(miestq50[i,])/mean(miestq50[1,])
	micomq25[i,1] <- mean(miestq25[i,])/mean(miestq25[1,])
	micomq10[i,1] <- mean(miestq10[i,])/mean(miestq10[1,])

	micomq90o[i,1] <- mean(miestq90[i,])
	micomq75o[i,1] <- mean(miestq75[i,])
	micomq50o[i,1] <- mean(miestq50[i,])
	micomq25o[i,1] <- mean(miestq25[i,])
	micomq10o[i,1] <- mean(miestq10[i,])
}

trends <- data.frame(cbind(nyear,micomq90,micomq75,micomq50,micomq25,micomq10))
trends.qnt <- data.frame(cbind(nyear,micomq90o,micomq75o,micomq50o,micomq25o,micomq10o))

setnames(trends,old=c("nyear","micomq90","micomq75","micomq50","micomq25","micomq10"),new=c("year","q90","q75","q50","q25","q10"))
setnames(trends.qnt,old=c("nyear","micomq90o","micomq75o","micomq50o","micomq25o","micomq10o"),new=c("year","q90","q75","q50","q25","q10"))

print(trends)
print(trends.qnt)

### PLOT TRENDS ###
tiff("C:\\Users\\Geoff\\Dropbox\\D\\projects\\scf_biz_assets\\figures\\figure_3.tiff",
     width=5,
     height=5,
     units='in',
     res=600)

plot(trends$year,trends$q90,
     type="l",
     lty="solid",
     ylim=c(0.5,2.5),
     xlab="Year",
     ylab="Multiplicative Change Since 1989",
     main="Trends in Asset Quantiles among \nBusiness Owners")
lines(trends$year,trends$q75,type="l",lty="dashed")
lines(trends$year,trends$q50,type="l",lty="dotted")
lines(trends$year,trends$q25,type="l",lty="dotdash")
lines(trends$year,trends$q10,type="l",lty="longdash")

labels <- c("90th Percentile","75th Percentile","50th Percentile","25th Percentile","10th Percentile")
type <- c("solid","dashed","dotted","dotdash","longdash")
legend("topleft",inset=0.04,labels,lty=type,seg.len=2,cex=0.9)

dev.off()

sink()

