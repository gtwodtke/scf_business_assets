#delimit ;
capture log close ;
capture clear all ;
set more off ;

global data_directory "C:\Users\Geoff\Dropbox\D\projects\scf_biz_assets\data\" ;
global log_directory "C:\Users\Geoff\Dropbox\D\projects\scf_biz_assets\programs\_LOGS\" ;
global temp_directory "C:\Users\Geoff\Dropbox\D\projects\scf_biz_assets\data\_TEMP\" ;

log using "${log_directory}06_create_v01_scf_final_merged.log", replace ;

/*************************************************************
PROGRAM NAME: 06_create_v01_scf_final_merged.do
PURPOSE: merge berkeley scf with fed scf and bootstrap weights
**************************************************************/

/***************************
MERGE BERKELEY WITH FED DATA
****************************/
use "${data_directory}berkeley_scf\v03_berkeley_scf.dta", clear ; 
merge 1:1 nyear ny1 using "${data_directory}fed_scf\surveys\v01_fed_scf.dta" ;
tab _merge ;
keep if _merge==3 ;
drop _merge ;
save "${temp_directory}temp_scf.dta", replace ;

/************************
MERGE SCF WITH P/E RATIOS
*************************/
import excel using "${data_directory}pe_ratio\cape_1983_2019.xlsx", firstrow clear ;
keep year cape ;
rename year nyear ;
save "${temp_directory}temp_peratio.dta", replace ;

use "${temp_directory}temp_scf.dta", clear ;
merge m:1 nyear using "${temp_directory}temp_peratio.dta" ;
keep if _merge==3 ;
drop _merge ;

/*****************
CREATE/RECODE VARS
******************/
/***VALUE OF BUSINESS ASSETS COMPUTED VIA INCOME CAPITALIZATION***/
/*DOLLARS*/
gen ntotbizvalic=ntotbizval ;
replace ntotbizvalic=nbizinc*(cape*0.90) if nbizown==1 & ncorp==1 & nbizinc>0 ;
replace ntotbizvalic=nbizinc*(cape*0.70) if nbizown==1 & ncorp==0 & nsoleprop==0 & nbizinc>0 ;
label var ntotbizvalic "Total Value of Business - Inc Capitalized" ;
sum ntotbizvalic, detail ;
drop cape ;
/*LOG-NORMAL HURDLE TRANSFORM*/
gen nbizvalposic=0 ;
replace nbizvalposic=1 if ntotbizvalic>0 ;
gen nlogbizvalic=ln(ntotbizvalic) if ntotbizvalic>0 ;
label var nbizvalposic "Dummy for Nonzero Business Assets - Inc Capitalized" ;
label var nlogbizvalic "Log of Total Biz Value - Inc Capitalized" ;
sum nbizvalposic nlogbizvalic ;
/***AMMOUNT OF BUSINESS LOAN PERSONALLY COLLATERALIZED OR GUARANTEED***/
/*DOLLARS*/
replace nbizcoll=nbizcoll*(376.5/188.6) if nyear==1989 ;
replace nbizcoll=nbizcoll*(376.5/210.2) if nyear==1992 ;
replace nbizcoll=nbizcoll*(376.5/225.3) if nyear==1995 ;
replace nbizcoll=nbizcoll*(376.5/239.5) if nyear==1998 ;
replace nbizcoll=nbizcoll*(376.5/260.1) if nyear==2001 ;
replace nbizcoll=nbizcoll*(376.5/277.5) if nyear==2004 ;
replace nbizcoll=nbizcoll*(376.5/304.6) if nyear==2007 ;
replace nbizcoll=nbizcoll*(376.5/320.4) if nyear==2010 ;
replace nbizcoll=nbizcoll*(376.5/342.5) if nyear==2013 ;
replace nbizcoll=nbizcoll*(376.5/353.4) if nyear==2016 ;

/***MISSING RECODES***/
replace nbizcred=. if nbizown==0 ;
replace ntotfirms=. if nbizown==0 ;
replace nempnum=. if nbizown==0 ;

/************************
MERGE SCF WITH BS WEIGHTS
*************************/
merge m:1 nurespid using "${data_directory}fed_scf\bsweights\v01_fed_bsweights.dta" ;
tab _merge ;
drop _merge ;

/********
SAVE DATA
*********/
sort nyear ;
saveold "${data_directory}v01_scf_final_merged.dta", replace version(12) ;
erase "${temp_directory}temp_scf.dta" ;
erase "${temp_directory}temp_peratio.dta" ;
*codebook n* ;

capture clear all ;

log close ;
