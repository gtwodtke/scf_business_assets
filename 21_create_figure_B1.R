sink("C:\\Users\\Geoff\\Dropbox\\D\\projects\\scf_biz_assets\\programs\\_LOGS\\21_create_figure_B1_log.txt")

##########################################################
##########################################################
##                                                      ##
## PROGRAM NAME: 21_create_figure_B1                    ##
## DESCRIPTION:                                         ##
##                                                      ##  
##  trends in alternative measures of ineq              ##
##                                                      ##
##########################################################
##########################################################

### LOAD LIBRARIES ###
rm(list=ls())

packages<-c("foreign", "dplyr", "tidyr", "ggplot2", "mgcv", 
	"data.table", "quantreg", "weights", "Hmisc", "dineq")

#install.packages(packages)

for (package.i in packages) {
	suppressPackageStartupMessages(library(package.i, character.only=TRUE))
	}

### INPUT DATA ###
scfmi<-read.dta("C:\\Users\\Geoff\\Dropbox\\D\\projects\\scf_biz_assets\\data\\v01_scf_final_merged.dta")
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
      sigmasq<-exp(predict(m3,newdata=nyear))*adjust
      miestpr.mod[,i]<-p
      miestmu.mod[,i]<-mu
      miestsg.mod[,i]<-sigmasq

	}

micompr.mod<-micommu.mod<-micomsg.mod<-matrix(data=NA,nrow=length(nyear[,1]),ncol=1)

for (i in 1:length(nyear[,1])) { 

	micompr.mod[i,1]<-mean(miestpr.mod[i,])
	micommu.mod[i,1]<-mean(miestmu.mod[i,])
	micomsg.mod[i,1]<-mean(miestsg.mod[i,])

	}

trends.mod<-data.frame(cbind(nyear,micompr.mod,micommu.mod,micomsg.mod))
setnames(trends.mod,old=c("nyear","micompr.mod","micommu.mod","micomsg.mod"),new=c("year","pr","mu","sigmasq"))

### COMPUTE OBSERVED TRENDS IN TOP 5% SHARE AND GINI INDEX ###
miestbiz5<-miestgini<-matrix(data=NA,nrow=length(nyear[,1]),ncol=5)

for (i in 1:5) {

	scf<-scfmi[which(scfmi$"nminum"==i),]
	scfbizown<-scfmi[which(scfmi$"nminum"==i & scfmi$"nbizown"==1),]
	q95fitbiz<-rq(ntotbizval~factor(nyear),tau=0.95,data=scf,weights=nsampwt)
	q95hatbiz<-predict(q95fitbiz)
	
	totbiz<-q95totbiz<-gini<-NULL
	for (j in 1:length(nyear[,1])) {
		totbiz<-c(totbiz,sum(scf[which(scf$"nyear"==nyear[j,1]),"nsampwt"]*scf[which(scf$"nyear"==nyear[j,1]),"ntotbizval"]))
		q95totbiz<-c(q95totbiz,sum(scf[which(scf$"nyear"==nyear[j,1] & scf$"ntotbizval">=q95hatbiz),"nsampwt"]*scf[which(scf$"nyear"==nyear[j,1] & scf$"ntotbizval">=q95hatbiz),"ntotbizval"]))
            gini<-c(gini,gini.wtd(scf[which(scf$"nyear"==nyear[j,1]),"ntotbizval"], weights=scf[which(scf$"nyear"==nyear[j,1]),"nsampwt"]))
		}
	
	miestbiz5[,i]<-q95totbiz/totbiz
	miestgini[,i]<-gini

	}

### COMPUTE MODEL-BASED TRENDS IN TOP 5% SHARE AND GINI ###
miestbiz5m<-miestginim<-matrix(data=NA,nrow=length(nyear[,1]),ncol=5)

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

  top5pct<-gini<-NULL
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
    q95<-quantile(sim$y,probs=0.95)
    toty<-sum(sim$y)
    totyq95<-sum(sim$y[sim$y>=q95])
    share5pct<-(totyq95/toty)
    top5pct<-c(top5pct,share5pct)
    gini <- c(gini,gini.wtd(sim$y))
    }

  miestbiz5m[,i]<-top5pct
  miestginim[,i]<-gini

  }

# COMBINE MI ESTIMATES #
micombiz5<-micomgini<-matrix(data=NA,nrow=length(nyear[,1]),ncol=1)
micombiz5m<-micomginim<-matrix(data=NA,nrow=length(nyear[,1]),ncol=1)

for (i in 1:length(nyear[,1])) { 

	micombiz5[i,1]<-mean(miestbiz5[i,])
	micombiz5m[i,1]<-mean(miestbiz5m[i,])

	micomgini[i,1]<-mean(miestgini[i,])
	micomginim[i,1]<-mean(miestginim[i,])

	}

trends.5pct<-data.frame(cbind(nyear,micombiz5,micombiz5m))
trends.gini<-data.frame(cbind(nyear,micomgini,micomginim))

setnames(trends.5pct,old=c("nyear","micombiz5","micombiz5m"),new=c("nyear","5pct.obs","5pct.mod"))
setnames(trends.gini,old=c("nyear","micomgini","micomginim"),new=c("nyear","gini.obs","gini.mod"))

print(trends.5pct)
print(trends.gini)

### PLOT TRENDS ###
tiff("C:\\Users\\Geoff\\Dropbox\\D\\projects\\scf_biz_assets\\figures\\figure_B1.tiff",
     width=9,
     height=5,
     units='in',
     res=600)

par(mfrow=c(1,2))

plot(trends.5pct$nyear,trends.5pct$"5pct.mod",
     type="l",
     ylim=c(0.95,1),
     xlab="Year",
     ylab="Quantile Share",
     main="Top 5% Share of Business Assets",
     cex.lab=0.8,
     cex.axis=0.8)
lines(trends.5pct$nyear,trends.5pct$"5pct.obs",type="l",lty="dashed",lwd=1)
points(trends.5pct$nyear,trends.5pct$"5pct.obs", pch=1, col="black")
labels<-c("Model-based","Observed")
type<-c("solid","dashed")
legend("topleft",inset=0.03,labels,lty=type,seg.len=3,cex=0.8)

plot(trends.gini$nyear,trends.gini$"gini.mod",
     type="l",
     ylim=c(0.95,1),
     xlab="Year",
     ylab="Gini",
     main="Gini Index for Business Assets",     
     cex.lab=0.8,
     cex.axis=0.8)
lines(trends.gini$nyear,trends.gini$"gini.obs",type="l",lty="dashed",lwd=1)
points(trends.gini$nyear,trends.gini$"gini.obs", pch=2, col="black")

dev.off()

sink()
