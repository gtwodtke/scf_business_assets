#delimit ;
capture log close ;
capture clear all ;
set more off ;

global data_directory "C:\Users\Geoff\Dropbox\D\projects\scf_biz_assets\data\fed_scf\bsweights\" ;
global log_directory "C:\Users\Geoff\Dropbox\D\projects\scf_biz_assets\programs\_LOGS\" ;

log using "${log_directory}05_create_v01_fed_bsweights.log", replace ;

/*******************************************************************************
PROGRAM NAME: 05_create_v03_berkeley_scf.do
PURPOSE: input bootstrap replicate weights from fed into single stacked file

Instructions for obtaining Bootstrap Replicate Weights for Waves 1989 to 2010: 

Proceed to "https://www.federalreserve.gov/econres/scf-previous-surveys.htm" 

Select the year of survey required

For each wave between 1989 and 2010 wave, scroll to the bottom of the page 
and you will see "Replicate weights--X42001: Stata version", 
click and download this 

Instructions for obtaining the 2013-2019 SCF bootstrap replicate weight:
Proceed to "https://www.federalreserve.gov/econres/scfindex.htm"

Scroll down to the heading labeled "Survey Data and Replicate Weight Files"
 
Under the third column titled "Replicate Weight File(X42001)" and row "STATA", 
click and download "Stata version"
*******************************************************************************/

/*********
INPUT DATA
**********/
/***1989***/
use "${data_directory}scf89rw1s\p89_rw1.dta", clear ; 
gen nyear=1989 ;
gen nx1=X1 ;
gen nxx1=XX1 ;
drop MM* ;
forval i=1/999 { ; 
	rename WT1B`i', lower ; 
	quietly replace wt1b`i'=0 if wt1b`i'==. ; 
	quietly sum wt1b`i' ;
	quietly replace wt1b`i'=wt1b`i'/r(mean) ;
	} ;
keep n* wt* ;
save "${data_directory}temp1989.dta", replace ;

/***1992***/
use "${data_directory}scf92rw1s\p92_rw1.dta", clear ; 
gen nyear=1992 ;
gen ny1=Y1 ;
gen nyy1=YY1 ;
drop MM* ;
forval i=1/999 { ; 
	rename WT1B`i', lower ; 
	quietly replace wt1b`i'=0 if wt1b`i'==. ; 
	quietly sum wt1b`i' ;
	quietly replace wt1b`i'=wt1b`i'/r(mean) ;
	} ;
keep n* wt* ;
save "${data_directory}temp1992.dta", replace ;

/***1995***/
use "${data_directory}scf95rw1s\p95_rw1.dta", clear ; 
gen nyear=1995 ;
gen ny1=Y1 ;
gen nyy1=YY1 ;
drop MM* ;
forval i=1/999 { ; 
	rename WT1B`i', lower ; 
	quietly replace wt1b`i'=0 if wt1b`i'==. ; 
	quietly sum wt1b`i' ;
	quietly replace wt1b`i'=wt1b`i'/r(mean) ;
	} ;
keep n* wt* ;
save "${data_directory}temp1995.dta", replace ;

/***1998***/
use "${data_directory}scf98rw1s\p98_rw1.dta", clear ; 
gen nyear=1998 ;
gen ny1=Y1 ;
gen nyy1=YY1 ;
drop MM* ;
forval i=1/999 { ; 
	rename WT1B`i', lower ; 
	quietly replace wt1b`i'=0 if wt1b`i'==. ; 
	quietly sum wt1b`i' ;
	quietly replace wt1b`i'=wt1b`i'/r(mean) ;
	} ;
keep n* wt* ;
save "${data_directory}temp1998.dta", replace ;

/***2001***/
use "${data_directory}scf2001rw1s\scf2001rw1s.dta", clear ; 
gen nyear=2001 ;
gen ny1=y1 ;
gen nyy1=yy1 ;
forval i=1/999 { ; 
	quietly replace wt1b`i'=0 if wt1b`i'==. ; 
	quietly sum wt1b`i' ;
	quietly replace wt1b`i'=wt1b`i'/r(mean) ;
	} ;
drop mm* ;
keep n* wt* ;
save "${data_directory}temp2001.dta", replace ;

/***2004***/
use "${data_directory}scf2004rw1s\p04_rw1.dta", clear ; 
gen nyear=2004 ;
gen ny1=Y1 ;
gen nyy1=YY1 ;
drop MM* ;
forval i=1/999 { ; 
	rename WT1B`i', lower ; 
	quietly replace wt1b`i'=0 if wt1b`i'==. ; 
	quietly sum wt1b`i' ;
	quietly replace wt1b`i'=wt1b`i'/r(mean) ;
	} ;
keep n* wt* ;
save "${data_directory}temp2004.dta", replace ;

/***2007***/
use "${data_directory}scf2007rw1s\p07_rw1.dta", clear ; 
gen nyear=2007 ;
gen ny1=Y1 ;
gen nyy1=YY1 ;
drop MM* ;
forval i=1/999 { ; 
	rename WT1B`i', lower ; 
	quietly replace wt1b`i'=0 if wt1b`i'==. ; 
	quietly sum wt1b`i' ;
	quietly replace wt1b`i'=wt1b`i'/r(mean) ;
	} ;
keep n* wt* ;
save "${data_directory}temp2007.dta", replace ;

/***2010***/
use "${data_directory}scf2010rw1s\p10_rw1.dta", clear ; 
gen nyear=2010 ;
gen ny1=Y1 ;
gen nyy1=YY1 ;
drop MM* ;
forval i=1/999 { ; 
	rename WT1B`i', lower ; 
	quietly replace wt1b`i'=0 if wt1b`i'==. ; 
	quietly sum wt1b`i' ;
	quietly replace wt1b`i'=wt1b`i'/r(mean) ;
	} ;
keep n* wt* ;
save "${data_directory}temp2010.dta", replace ;

/***2013***/
use "${data_directory}scf2013rw1s\p13_rw1.dta", clear ; 
gen nyear=2013 ;
gen ny1=Y1 ;
gen nyy1=YY1 ;
drop MM* ;
forval i=1/999 { ; 
	rename WT1B`i', lower ; 
	quietly replace wt1b`i'=0 if wt1b`i'==. ; 
	quietly sum wt1b`i' ;
	quietly replace wt1b`i'=wt1b`i'/r(mean) ;
	} ;
keep n* wt* ;
save "${data_directory}temp2013.dta", replace ;

/***2016***/
use "${data_directory}scf2016rw1s\p16_rw1.dta", clear ; 
gen nyear=2016 ;
gen ny1=Y1 ;
gen nyy1=YY1 ;
drop MM* ;
forval i=1/999 { ; 
	rename WT1B`i', lower ; 
	quietly replace wt1b`i'=0 if wt1b`i'==. ; 
	quietly sum wt1b`i' ;
	quietly replace wt1b`i'=wt1b`i'/r(mean) ;
	} ;
keep n* wt* ;
save "${data_directory}temp2016.dta", replace ;

/***2019***/
use "${data_directory}scf2019rw1s\p19_rw1.dta", clear ; 
gen nyear=2019 ;
gen ny1=y1 ;
gen nyy1=yy1 ;
drop mm* ;
forval i=1/999 { ; 
	quietly replace wt1b`i'=0 if wt1b`i'==. ; 
	quietly sum wt1b`i' ;
	quietly replace wt1b`i'=wt1b`i'/r(mean) ;
	} ;
keep n* wt* ;
save "${data_directory}temp2019.dta", replace ;

/**********
APPEND DATA
***********/
use "${data_directory}temp1989.dta", clear ;
forval i=1992(3)2019 { ;
	append using "${data_directory}temp`i'.dta" ;
	} ;
tab nyear, missing ;

/*************************
CREATE/RECODE ID VARIABLES
**************************/
gen nrespid=nxx1 if nyear==1989 ;
replace nrespid=nyy1 if nyear!=1989 ;
label var nrespid "Respondent ID (unique within years)" ;
codebook nrespid ;
/***UNIQUE RESPONDENT ID VARIABLE***/
gen double nurespid=(10000*nyear)+nrespid ;
label var nurespid "Unique Respondent ID (unique across years)" ;
codebook nurespid ;

/********
SAVE DATA
*********/
keep wt1b1-wt1b500 n* ;
save "${data_directory}v01_fed_bsweights.dta", replace ;

/**************
ERASE TEMP DATA
***************/
forval i=1989(3)2019 { ;
	capture erase "${data_directory}temp`i'.dta" ;
	} ;

log close ;
