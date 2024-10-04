sink("C:\\Users\\Geoff\\Dropbox\\D\\projects\\scf_biz_assets\\programs\\_LOGS\\17_create_table_7_log.txt")

##########################################################
##########################################################
##                                                      ##
## PROGRAM NAME: 17_create_table_7                      ##
## DESCRIPTION:                                         ##
##                                                      ##  
##  trends among owners who did not inherit             ##
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
coefs<-2

### COMPUTE LOG-NORMAL HURDLE MODEL ESTIMATES ###
miestd<-miesty<-miestesq<-matrix(data=NA,nrow=coefs,ncol=5)
mivard<-mivary<-mivaresq<-matrix(data=NA,nrow=coefs,ncol=5)

for (i in 1:5) {

	scfpop<-scfmi[(which(scfmi$"nminum"==i & scfmi$"ninhfirm"==0)),]
	scfbiz<-scfmi[which(scfmi$"nminum"==i & scfmi$"ninhfirm"==0 & scfmi$"nbizvalpos"==1),]
	dlinmod<-gam(nbizvalpos~poly(nyear,1,raw=T),family=quasibinomial(link="probit"),data=scfpop,weights=nsampwt)
	dlinmod_coefs<-data.frame(coefficients(dlinmod))
	ylinmod<-gam(nlogbizval~poly(nyear,1,raw=T),family=gaussian(link="identity"),data=scfbiz,weights=nsampwt)
	ylinmod_coefs<-data.frame(coefficients(ylinmod))
	scfbiz$ehatsq<-residuals(ylinmod,type="response")^2
	esqlindmod<-gam(ehatsq~poly(nyear,1,raw=T),family=Gamma(link="log"),data=scfbiz,weights=nsampwt)
	esqlindmod_coefs<-data.frame(coefficients(esqlindmod))
	miestd[,i]<-dlinmod_coefs[,1]
	miesty[,i]<-ylinmod_coefs[,1]
	miestesq[,i]<-esqlindmod_coefs[,1]

	boot.dist.dcoefs<-boot.dist.ycoefs<-boot.dist.esqcoefs<-matrix(data=NA,nrow=coefs,ncol=nboot)
	for (j in 60:(nboot+59)) {
		boot.dlinmod<-gam(nbizvalpos~poly(nyear,1,raw=T),family=quasibinomial(link="probit"),data=scfpop,weights=scfpop[,j])
		boot.dlinmod_coefs<-data.frame(coefficients(boot.dlinmod))
		boot.ylinmod<-gam(nlogbizval~poly(nyear,1,raw=T),family=gaussian(link="identity"),data=scfbiz,weights=scfbiz[,j])
		boot.ylinmod_coefs<-data.frame(coefficients(boot.ylinmod))
		scfbiz$boot.ehatsq<-residuals(boot.ylinmod,type="response")^2
		boot.esqlindmod<-gam(boot.ehatsq~poly(nyear,1,raw=T),family=Gamma(link="log"),data=scfbiz,weights=scfbiz[,j])
		boot.esqlindmod_coefs<-data.frame(coefficients(boot.esqlindmod))
		boot.dist.dcoefs[,(j-59)]<-boot.dlinmod_coefs[,1]
		boot.dist.ycoefs[,(j-59)]<-boot.ylinmod_coefs[,1]
		boot.dist.esqcoefs[,(j-59)]<-boot.esqlindmod_coefs[,1]
		}
	for (m in 1:coefs) { 
		mivard[m,i]<-var(boot.dist.dcoefs[m,]) 
		mivary[m,i]<-var(boot.dist.ycoefs[m,]) 
		mivaresq[m,i]<-var(boot.dist.esqcoefs[m,]) 
		}

	}

micomestd<-micomesty<-micomestesq<-matrix(data=NA,nrow=coefs,ncol=5)

for (i in 1:coefs) { 

	micomestd[i,1]<-round(mean(miestd[i,]),digits=3)
	micomestd[i,2]<-round(sqrt(mean(mivard[i,])+(var(miestd[i,])*(1+(1/5)))),digits=3)
	micomestd[i,3]<-round((pnorm(abs(micomestd[i,1]/micomestd[i,2]),0,1,lower.tail=FALSE)*2),digits=3)
	micomestd[i,4]<-round(micomestd[i,1]-1.96*micomestd[i,2],digits=3)
	micomestd[i,5]<-round(micomestd[i,1]+1.96*micomestd[i,2],digits=3)

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

rlabel<-c('intercept','nyear')

output.d<-data.frame(micomestd,row.names=rlabel)
output.y<-data.frame(micomesty,row.names=rlabel)
output.esq<-data.frame(micomestesq,row.names=rlabel)

colnames(output.d)<-c('Coef','SE','Pval','LL 95% CI','UL 95% CI')
colnames(output.y)<-c('Coef','SE','Pval','LL 95% CI','UL 95% CI')
colnames(output.esq)<-c('Coef','SE','Pval','LL 95% CI','UL 95% CI')

scfmi<-scfmi[which(scfmi$"nminum"==1 & scfmi$"ninhfirm"==0),]
scfmibiz<-scfmi[which(scfmi$"nbizvalpos"==1 & scfmi$"ninhfirm"==0 & scfmi$"nminum"==1),]
dsmooth_null<-gam(nbizvalpos~poly(nyear,1,raw=T),family=binomial(link="probit"),data=scfmi,weights=nsampwt)
dsmooth_alt<-gam(nbizvalpos~s(nyear),family=binomial(link="probit"),gamma=smooth,data=scfmi,weights=nsampwt)
ysmooth_null<-gam(nlogbizval~poly(nyear,1,raw=T),family=gaussian(link="identity"),data=scfmibiz,weights=nsampwt)
ysmooth_alt<-gam(nlogbizval~s(nyear),family=gaussian(link="identity"),gamma=smooth,data=scfmibiz,weights=nsampwt)
scfmibiz$ehatsq<-residuals(ysmooth_alt,type="response")^2
esqsmooth_null<-gam(ehatsq~poly(nyear,1,raw=T),family=Gamma(link="log"),data=scfmibiz,weights=nsampwt)
esqsmooth_alt<-gam(ehatsq~s(nyear),family=Gamma(link="log"),gamma=smooth,data=scfmibiz,weights=nsampwt)

### PRINT RESULTS ###
cat("$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$\n")
cat("$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$\n")
cat("TABLE 7 - Trends excl. Inherited Firms\n")
cat("$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$\n")
cat("$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$\n")
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
cat("===========================================\n")
cat("Selection Model\n")
cat("===========================================\n")
cat("**********Linear Trends**********\n")
print(output.d)
cat(" \n")
cat("**********Spline Trends**********\n")
print(c(dsmooth_null$deviance,(nrow(scfmi)-dsmooth_null$df.residual)))
print(c(dsmooth_alt$deviance,(nrow(scfmi)-dsmooth_alt$df.residual)))
cat("===========================================\n")
cat(" \n")
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
