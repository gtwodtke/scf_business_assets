sink("C:\\Users\\Geoff\\Dropbox\\D\\projects\\scf_biz_assets\\programs\\_LOGS\\15_create_table_5_log.txt")

##########################################################
##########################################################
##                                                      ##
## PROGRAM NAME: 15_create_table_5                      ##
## DESCRIPTION:                                         ##
##                                                      ##  
##  parallel analyses by firm type (legal form)         ##
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
scfmi$nyear<-scfmi$nyear-1989
nboot<-200
smooth<-10
coefs<-6

### COMPUTE LOG-NORMAL HURDLE MODEL ESTIMATES ###
miesty<-miestesq<-matrix(data=NA,nrow=coefs,ncol=5)
mivary<-mivaresq<-matrix(data=NA,nrow=coefs,ncol=5)

for (i in 1:5) {

	scfbiz<-scfmi[which(scfmi$"nminum"==i & scfmi$"nbizvalpos"==1),]
	ylinmod<-gam(nlogbizval~poly(nyear,1,raw=T)*(nsoleprop+ncorp),family=gaussian(link="identity"),data=scfbiz,weights=nsampwt)
	ylinmod_coefs<-data.frame(coefficients(ylinmod))
	scfbiz$ehatsq<-residuals(ylinmod,type="response")^2
	esqlindmod<-gam(ehatsq~poly(nyear,1,raw=T)*(nsoleprop+ncorp),family=Gamma(link="log"),data=scfbiz,weights=nsampwt)
	esqlindmod_coefs<-data.frame(coefficients(esqlindmod))
	miesty[,i]<-ylinmod_coefs[,1]
	miestesq[,i]<-esqlindmod_coefs[,1]

	boot.dist.dcoefs<-boot.dist.ycoefs<-boot.dist.esqcoefs<-matrix(data=NA,nrow=coefs,ncol=nboot)
	for (j in 60:(nboot+59)) {
		boot.ylinmod<-gam(nlogbizval~poly(nyear,1,raw=T)*(nsoleprop+ncorp),family=gaussian(link="identity"),data=scfbiz,weights=scfbiz[,j])
		boot.ylinmod_coefs<-data.frame(coefficients(boot.ylinmod))
		scfbiz$boot.ehatsq<-residuals(boot.ylinmod,type="response")^2
		boot.esqlindmod<-gam(boot.ehatsq~poly(nyear,1,raw=T)*(nsoleprop+ncorp),family=Gamma(link="log"),data=scfbiz,weights=scfbiz[,j])
		boot.esqlindmod_coefs<-data.frame(coefficients(boot.esqlindmod))
		boot.dist.ycoefs[,(j-59)]<-boot.ylinmod_coefs[,1]
		boot.dist.esqcoefs[,(j-59)]<-boot.esqlindmod_coefs[,1]
		}
	for (m in 1:coefs) { 
		mivary[m,i]<-var(boot.dist.ycoefs[m,]) 
		mivaresq[m,i]<-var(boot.dist.esqcoefs[m,]) 
		}

	}

micomesty<-micomestesq<-matrix(data=NA,nrow=coefs,ncol=5)

for (i in 1:coefs) { 

	micomesty[i,1]<-round(mean(miesty[i,]),digits=3)
	micomesty[i,2]<-round(sqrt(mean(mivary[i,])+(var(miesty[i,])*(1+(1/5)))),digits=3)
	micomesty[i,3]<-round((pnorm(abs(micomesty[i,1]/micomesty[i,2]),0,1,lower.tail=FALSE)*2),digits=3)
	micomesty[i,4]<-round(micomesty[i,1]-1.96*micomesty[i,2],digits=3)
	micomesty[i,5]<-round(micomesty[i,1]+1.96*micomesty[i,2],digits=3)

	micomestesq[i,1]<-round(mean(miestesq[i,]),digits=3)
	micomestesq[i,2]<-round(sqrt(mean(mivaresq[i,])+(var(miestesq[i,])*(1+(1/5)))),digits=3)
	micomestesq[i,3]<-round((pnorm(abs(micomestesq[i,1]/micomestesq[i,2]),0,1,lower.tail=FALSE)*2),digits=3)
	micomestesq[i,4]<-round(micomestesq[i,1]-1.96*micomestesq[i,2],digits=3)
	micomestesq[i,5]<-round(micomestesq[i,1]+1.96*micomestesq[i,2],digits=3)

	}

rlabel<-c('intercept','nyear','soleprop','corp','nyear:soleprop','nyear:corp')

output.y<-data.frame(micomesty,row.names=rlabel)
output.esq<-data.frame(micomestesq,row.names=rlabel)

colnames(output.y)<-c('Coef','SE','Pval','LL 95% CI','UL 95% CI')
colnames(output.esq)<-c('Coef','SE','Pval','LL 95% CI','UL 95% CI')

scfmibiz<-scfmi[which(scfmi$"nbizvalpos"==1 & scfmi$"nminum"==1),]
scfmibiz$ftype<-1
scfmibiz$ftype[scfmibiz$nsoleprop==1]<-2
scfmibiz$ftype[scfmibiz$ncorp==1]<-3
scfmibiz$ftype<-as.factor(scfmibiz$ftype)
ysmooth_null<-gam(nlogbizval~poly(nyear,1,raw=T)*ftype,family=gaussian(link="identity"),data=scfmibiz,weights=nsampwt)
ysmooth_alt<-gam(nlogbizval~ftype+s(nyear,by=ftype),family=gaussian(link="identity"),gamma=smooth,data=scfmibiz,weights=nsampwt)
scfmibiz$ehatsq<-residuals(ysmooth_alt,type="response")^2
esqsmooth_null<-gam(ehatsq~poly(nyear,1,raw=T)*ftype,family=Gamma(link="log"),data=scfmibiz,weights=nsampwt)
esqsmooth_alt<-gam(ehatsq~ftype+s(nyear,by=ftype),family=Gamma(link="log"),gamma=smooth,data=scfmibiz,weights=nsampwt)

### PRINT RESULTS ###
cat("$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$\n")
cat("$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$\n")
cat("TABLE 5 - Trends by Firm Type\n")
cat("$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$\n")
cat("$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$\n")
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
cat("===========================================\n")
cat("Conditional Mean Model\n")
cat("===========================================\n")
cat("**********Linear Trends**********\n")
print(output.y)
cat(" \n")
cat("**********Spline Trends**********\n")
print(c(ysmooth_null$deviance,(nrow(scfmibiz)-ysmooth_null$df.residual)))
print(c(ysmooth_alt$deviance,(nrow(scfmibiz)-ysmooth_alt$df.residual)))
cat("===========================================\n")
cat(" \n")
cat("===========================================\n")
cat("Conditional Variance Model\n")
cat("===========================================\n")
cat("**********Linear Trends**********\n")
print(output.esq)
cat(" \n")
cat("**********Spline Trends**********\n")
print(c(esqsmooth_null$deviance,(nrow(scfmibiz)-esqsmooth_null$df.residual)))
print(c(esqsmooth_alt$deviance,(nrow(scfmibiz)-esqsmooth_alt$df.residual)))
cat("===========================================\n")
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
cat(" \n")
cat(" \n")
cat(" \n")
cat(" \n")
cat(" \n")

sink()
