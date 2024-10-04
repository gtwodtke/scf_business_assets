sink("C:\\Users\\Geoff\\Dropbox\\D\\projects\\scf_biz_assets\\programs\\_LOGS\\25_create_figure_D1_log.txt")

##########################################################
##########################################################
##                                                      ##
## PROGRAM NAME: 25_create_figure_D1                    ##
## DESCRIPTION:                                         ##
##                                                      ##  
##  trends in correlations of business assets with      ##
##  with other asset classes                            ##
##                                                      ##
##########################################################
##########################################################

### LOAD LIBRARIES ###
rm(list=ls())

packages<-c("foreign", "dplyr", "tidyr", "ggplot2", "mgcv", 
	"data.table", "quantreg", "weights", "Hmisc", "wCorr")

#install.packages(packages)

for (package.i in packages) {
	suppressPackageStartupMessages(library(package.i, character.only=TRUE))
	}

### INPUT DATA ###
scfmi<-read.dta("C:\\Users\\Geoff\\Dropbox\\D\\projects\\scf_biz_assets\\data\\v01_scf_final_merged.dta")
nyear<-data.frame(nyear=c(1989,1992,1995,1998,2001,2004,2007,2010,2013,2016,2019))

### CREATE IHS TRANSFORM ###
scfmi$nihsbizval<-log(scfmi$ntotbizval+sqrt((scfmi$ntotbizval^2)+1))
scfmi$nihsrealval<-log(scfmi$ntotrealval+sqrt((scfmi$ntotrealval^2)+1))
scfmi$nihscapval<-log(scfmi$ntotcapval+sqrt((scfmi$ntotcapval^2)+1))

### COMPUTE CORRELATIONS ###
miest.sp.re<-miest.sp.fi<-miest.pr.re<-miest.pr.fi<-matrix(data=NA,nrow=length(nyear[,1]),ncol=5)

for (i in 1:5) {

     scf<-scfmi[which(scfmi$"nminum"==i),]

     sp.re<-sp.fi<-pr.re<-pr.fi<-NULL
     
     for (j in 1:length(nyear[,1])) {
     
          sp.re<-c(sp.re, 
               weightedCorr(
                    x=scf[which(scf$"nyear"==nyear[j,1]),"ntotbizval"],
                    y=scf[which(scf$"nyear"==nyear[j,1]),"ntotrealval"],
                    weights=scf[which(scf$"nyear"==nyear[j,1]),"nsampwt"], 
                    method="Spearman"))

          sp.fi<-c(sp.fi, 
               weightedCorr(
                    x=scf[which(scf$"nyear"==nyear[j,1]),"ntotbizval"],
                    y=scf[which(scf$"nyear"==nyear[j,1]),"ntotcapval"],
                    weights=scf[which(scf$"nyear"==nyear[j,1]),"nsampwt"], 
                    method="Spearman"))

          pr.re<-c(pr.re, 
               weightedCorr(
                    x=scf[which(scf$"nyear"==nyear[j,1]),"nihsbizval"],
                    y=scf[which(scf$"nyear"==nyear[j,1]),"nihsrealval"],
                    weights=scf[which(scf$"nyear"==nyear[j,1]),"nsampwt"], 
                    method="Pearson"))

          pr.fi<-c(pr.fi, 
               weightedCorr(
                    x=scf[which(scf$"nyear"==nyear[j,1]),"nihsbizval"],
                    y=scf[which(scf$"nyear"==nyear[j,1]),"nihscapval"],
                    weights=scf[which(scf$"nyear"==nyear[j,1]),"nsampwt"], 
                    method="Pearson"))                    
          }
	
	miest.sp.re[,i]<-sp.re
	miest.sp.fi[,i]<-sp.fi
	miest.pr.re[,i]<-pr.re
	miest.pr.fi[,i]<-pr.fi

	}

# COMBINE MI ESTIMATES #
micomspre<-micomspfi<-micomprre<-micomprfi<-matrix(data=NA,nrow=length(nyear[,1]),ncol=1)

for (i in 1:length(nyear[,1])) { 

	micomspre[i,1]<-mean(miest.sp.re[i,])
	micomspfi[i,1]<-mean(miest.sp.fi[i,])
	micomprre[i,1]<-mean(miest.pr.re[i,])
	micomprfi[i,1]<-mean(miest.pr.fi[i,])

	}

trends.corr<-data.frame(cbind(nyear,micomspre,micomspfi,micomprre,micomprfi))
setnames(
     trends.corr,
     old=c("nyear","micomspre","micomspfi","micomprre","micomprfi"),
     new=c("year","spearman.re","spearman.fi","pearson.re","pearson.fi"))

print(trends.corr)

### PLOT TRENDS ###
tiff("C:\\Users\\Geoff\\Dropbox\\D\\projects\\scf_biz_assets\\figures\\figure_D1.tiff",
     width=9,
     height=5,
     units='in',
     res=600)

par(mfrow=c(1,2))

plot(trends.corr$year,trends.corr$"spearman.re",
     type="l",
     ylim=c(0.0,0.4),
     xlab="Year",
     ylab="Correlation Coefficient",
     main="Spearman Correlations between \n Business Assets and Other Asset Classes",
     cex.lab=0.8,
     cex.axis=0.8,
     cex.main=0.85)
points(trends.corr$year,trends.corr$"spearman.re", pch=1, col="black")
points(trends.corr$year,trends.corr$"spearman.fi", pch=2, col="black")
lines(trends.corr$year,trends.corr$"spearman.fi",type="l",lty="dashed",lwd=1)
labels<-c("Real Estate Assets","Financial Assets")
type<-c("solid","dashed")
legend("topleft",inset=0.03,labels,lty=type,seg.len=3,cex=0.8)

plot(trends.corr$year,trends.corr$"pearson.re",
     type="l",
     ylim=c(0.0,0.4),
     xlab="Year",
     ylab="Correlation Coefficient",
     main="Pearson Correlations between Business Assets and \n Other Asset Classes after arsinh() Transformation",
     cex.lab=0.8,
     cex.axis=0.8,     
     cex.main=0.85)
points(trends.corr$year,trends.corr$"pearson.re", pch=1, col="black")
points(trends.corr$year,trends.corr$"pearson.fi", pch=2, col="black")
lines(trends.corr$year,trends.corr$"pearson.fi",type="l",lty="dashed",lwd=1)
labels<-c("Real Estate Assets","Financial Assets")
type<-c("solid","dashed")
legend("topleft",inset=0.03,labels,lty=type,seg.len=3,cex=0.8)

dev.off()

sink()
