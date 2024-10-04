#delimit ;
capture log close ;
capture clear all ;
set more off ;

global data_directory "C:\Users\Geoff\Dropbox\D\projects\scf_biz_assets\data\fed_scf\surveys\" ;
global log_directory "C:\Users\Geoff\Dropbox\D\projects\scf_biz_assets\programs\_LOGS\" ;
global temp_directory "C:\Users\Geoff\Dropbox\D\projects\scf_biz_assets\data\_TEMP\" ;

set maxvar 10000 ;

log using "${log_directory}04_create_v01_fed_scf.log", replace ;

/**********************************************************************
PROGRAM NAME: 04_create_v01_fed_scf.do
PURPOSE: input data from fed SCF files; recode and create new variables
***********************************************************************/

/********************
INPUT AND RECODE DATA
*********************/
/***1989***/
use "${data_directory}scf89s\p89i6.dta", clear ;
/*IDs*/
keep 
	XX1 X1 
	X3105 J3105
	X3108 X3208 X3308 J3108 J3208 J3308
	X3111 X3211 X3311 J3111 J3211 J3311
	X3120 X3220 X3320 J3120 J3220 J3320
	X3121 X3221 X3321 J3121 J3221 J3321
	X918 X1018 J918 J1018
    X1106 X1117 X1128 J1106 J1117 J1128
    X2710 X2727 X2810 X2827 X2910 X2927 J2710 J2727 J2810 J2827 J2910 J2927
	X3119 J3119	
	J3129 J3229 J3329 ;
gen nyear=1989 ;
gen nx1=X1 ;
gen nxx1=XX1 ;
gen ny1=X1 ;
gen nyy1=XX1 ;
drop X1 XX1 ;
/*firm size*/
egen nfirmsize=rowmax(X3111 X3211 X3311) ;
replace nfirmsize=. if nfirmsize<1 ;
label var nfirmsize "Firm Scale (num. of emp.)" ;
tab nfirmsize, missing ;
drop X3111 X3211 X3311 J3111 J3211 J3311 ;
/*collateralized business loan*/
gen nbizloan=0 ;
replace nbizloan=1 if X3120==1 | X3220==1 | X3320==1 ;
label var nbizloan "Has Personally Collateralized or Cosigned Biz Loan (1=yes)" ;
tab nbizloan, missing ;
drop X3120 X3220 X3320 J3120 J3220 J3320 ;
/*ammount collateralized*/
gen nbizcoll = X3121 + X3221 + X3321 ;
label var nbizcoll "Ammount Collateralized or Guaranteed" ;
sum nbizcoll, detail ;
codebook nbizcoll ;
drop X3121 X3221 X3321 J3121 J3221 J3321 ;
/*any credit for investment in business*/
gen nbizcred=nbizloan ;
replace nbizcred=1 if 
	inrange(X918,71,79) | inrange(X1018,71,79) | 
    inrange(X1106,71,79) | inrange(X1117,71,79) | inrange(X1128,71,79) | 
    inrange(X2710,71,79) | inrange(X2727,71,79) | inrange(X2810,71,79) | 
	inrange(X2827,71,79) | inrange(X2910,71,79) | inrange(X2927,71,79) ;
label var nbizcred "Has Used Any Personal Credit to Invest in Biz (1=yes)" ;
tab nbizcred, missing ;
drop 	
	X918 X1018 
    X1106 X1117 X1128 
    X2710 X2727 X2810 X2827 X2910 X2927 
	J918 J1018 
    J1106 J1117 J1128 
    J2710 J2727 J2810 J2827 J2910 J2927 ;
/*tot num of businesses owned + managed*/
gen ntotfirms=X3105 if X3105!=0 ;
tab ntotfirms, missing ;
label var ntotfirms "Tot. Num. of Firms Owned & Managed" ;
tab ntotfirms, missing ;
drop X3105 J3105 ;
/*firm type*/
gen nsoleprop=0 ;
replace nsoleprop=1 if X3119==2 ;
gen ncorp=0 ;
replace ncorp=1 if inlist(X3119,3,4) ;
label var nsoleprop "Owns a Sole Proprietorship (1=yes)" ;
label var ncorp "Owns an S, C or Other Corporation (1=yes)" ;
tab X3119 nsoleprop, missing ;
tab X3119 ncorp, missing ;
drop X3119 J3119 ;
/*inherited firm*/
gen ninhfirm=0 ;
replace ninhfirm=1 if inlist(X3108,3,4) ;
label var ninhfirm "Inherited or given main firm (1=yes)" ;
tab X3108 ninhfirm, missing ;
drop X3108 X3208 X3308 J3108 J3208 J3308 ;
/*business assets - missing flag*/
gen ibizval=0 ;
replace ibizval=1 if J3129>=50 ;
tab ibizval, missing ;
drop J3129 J3229 J3329 ;
/*save temp data*/
save "${temp_directory}scf89.dta", replace ;

/***1992***/
use "${data_directory}scf92s\p92i4.dta", clear ;
/*IDs*/
keep 
	YY1 Y1 
	X3105 J3105
	X3108 X3208 X3308 J3108 J3208 J3308
	X3111 X3211 X3311 J3111 J3211 J3311
	X3120 X3220 X3320 J3120 J3220 J3320
	X3121 X3221 X3321 J3121 J3221 J3321
	X918 X1018 J918 J1018
    X1106 X1117 X1128 J1106 J1117 J1128
    X2710 X2727 X2810 X2827 X2910 X2927 J2710 J2727 J2810 J2827 J2910 J2927 
	X3119 J3119 
	J3129 J3229 J3329 ;
gen nyear=1992 ;
rename Y1 ny1  ;
rename YY1 nyy1 ;
/*firm size*/
egen nfirmsize=rowmax(X3111 X3211 X3311) ;
replace nfirmsize=. if nfirmsize<1 ;
label var nfirmsize "Firm Scale (num. of emp.)" ;
tab nfirmsize, missing ;
drop X3111 X3211 X3311 J3111 J3211 J3311 ;
/*collateralized business loan*/
gen nbizloan=0 ;
replace nbizloan=1 if X3120==1 | X3220==1 | X3320==1 ;
label var nbizloan "Has Personally Collateralized or Cosigned Biz Loan (1=yes)" ;
tab nbizloan, missing ;
drop X3120 X3220 X3320 J3120 J3220 J3320 ;
/*ammount collateralized*/
gen nbizcoll = X3121 + X3221 + X3321 ;
label var nbizcoll "Ammount Collateralized or Guaranteed" ;
sum nbizcoll, detail ;
codebook nbizcoll ;
drop X3121 X3221 X3321 J3121 J3221 J3321 ;
/*any credit for investment in business*/
gen nbizcred=nbizloan ;
replace nbizcred=1 if 
	inrange(X918,71,79) | inrange(X1018,71,79) | 
    inrange(X1106,71,79) | inrange(X1117,71,79) | inrange(X1128,71,79) | 
    inrange(X2710,71,79) | inrange(X2727,71,79) | inrange(X2810,71,79) | 
	inrange(X2827,71,79) | inrange(X2910,71,79) | inrange(X2927,71,79) ;
label var nbizcred "Has Used Any Personal Credit to Invest in Biz (1=yes)" ;
tab nbizcred, missing ;
drop 	
	X918 X1018 
    X1106 X1117 X1128 
    X2710 X2727 X2810 X2827 X2910 X2927 
	J918 J1018 
    J1106 J1117 J1128 
    J2710 J2727 J2810 J2827 J2910 J2927 ;
/*tot num of businesses owned + managed*/
gen ntotfirms=X3105 if X3105!=0 ;
replace ntotfirms=10 if inrange(ntotfirms,10,99) ; 
tab ntotfirms, missing ;
label var ntotfirms "Tot. Num. of Firms Owned & Managed" ;
tab ntotfirms, missing ;
drop X3105 J3105 ;
/*firm type*/
gen nsoleprop=0 ;
replace nsoleprop=1 if X3119==2 ;
gen ncorp=0 ;
replace ncorp=1 if inlist(X3119,3,4) ;
label var nsoleprop "Owns a Sole Proprietorship (1=yes)" ;
label var ncorp "Owns an S, C or Other Corporation (1=yes)" ;
tab X3119 nsoleprop, missing ;
tab X3119 ncorp, missing ;
drop X3119 J3119 ;
/*inherited firm*/
gen ninhfirm=0 ;
replace ninhfirm=1 if inlist(X3108,3,4) ;
label var ninhfirm "Inherited or given main firm (1=yes)" ;
tab X3108 ninhfirm, missing ;
drop X3108 X3208 X3308 J3108 J3208 J3308 ;
/*business assets - missing flag*/
gen ibizval=0 ;
replace ibizval=1 if J3129>=150 ;
tab ibizval, missing ;
drop J3129 J3229 J3329 ;
/*save temp data*/
save "${temp_directory}scf92.dta", replace ;

/***1995***/
use "${data_directory}scf95s\p95i6.dta", clear ;
/*IDs*/
keep 
	YY1 Y1 
	X3105 J3105
	X3108 X3208 X3308 J3108 J3208 J3308
	X3111 X3211 X3311 J3111 J3211 J3311
	X3120 X3220 X3320 J3120 J3220 J3320 
	X3121 X3221 X3321 J3121 J3221 J3321
	X918 X1018 J918 J1018 
    X1106 X1117 X1128 J1106 J1117 J1128
    X2710 X2727 X2810 X2827 X2910 X2927 J2710 J2727 J2810 J2827 J2910 J2927 
	X3119 J3119 
	J3129 J3229 J3329 ;
gen nyear=1995 ;
rename Y1 ny1  ;
rename YY1 nyy1 ;
/*firm size*/
egen nfirmsize=rowmax(X3111 X3211 X3311) ;
replace nfirmsize=. if nfirmsize<1 ;
label var nfirmsize "Firm Scale (num. of emp.)" ;
tab nfirmsize, missing ;
drop X3111 X3211 X3311 J3111 J3211 J3311 ;
/*collateralized business loan*/
gen nbizloan=0 ;
replace nbizloan=1 if X3120==1 | X3220==1 | X3320==1 ;
label var nbizloan "Has Personally Collateralized or Cosigned Biz Loan (1=yes)" ;
tab nbizloan, missing ;
drop X3120 X3220 X3320 J3120 J3220 J3320 ;
/*ammount collateralized*/
gen nbizcoll = X3121 + X3221 + X3321 ;
label var nbizcoll "Ammount Collateralized or Guaranteed" ;
sum nbizcoll, detail ;
codebook nbizcoll ;
drop X3121 X3221 X3321 J3121 J3221 J3321 ;
/*any credit for investment in business*/
gen nbizcred=nbizloan ;
replace nbizcred=1 if 
	X918==7 | X1018==7 | 
    X1106==7 | X1117==7 | X1128==7 | 
    X2710==7 | X2727==7 | X2810==7 | 
	X2827==7 | X2910==7 | X2927==7 ;
label var nbizcred "Has Used Any Personal Credit to Invest in Biz (1=yes)" ;
tab nbizcred, missing ;
drop 	
	X918 X1018 
    X1106 X1117 X1128 
    X2710 X2727 X2810 X2827 X2910 X2927 
	J918 J1018 
    J1106 J1117 J1128 
    J2710 J2727 J2810 J2827 J2910 J2927 ;
/*tot num of businesses owned + managed*/
gen ntotfirms=X3105 if X3105!=0 ;
replace ntotfirms=10 if inrange(ntotfirms,10,99) ; 
tab ntotfirms, missing ;
label var ntotfirms "Tot. Num. of Firms Owned & Managed" ;
tab ntotfirms, missing ;
drop X3105 J3105 ;
/*firm type*/
gen nsoleprop=0 ;
replace nsoleprop=1 if X3119==2 ;
gen ncorp=0 ;
replace ncorp=1 if inlist(X3119,3,4) ;
label var nsoleprop "Owns a Sole Proprietorship (1=yes)" ;
label var ncorp "Owns an S, C or Other Corporation (1=yes)" ;
tab X3119 nsoleprop, missing ;
tab X3119 ncorp, missing ;
drop X3119 J3119 ;
/*inherited firm*/
gen ninhfirm=0 ;
replace ninhfirm=1 if inlist(X3108,3,4) ;
label var ninhfirm "Inherited or given main firm (1=yes)" ;
tab X3108 ninhfirm, missing ;
drop X3108 X3208 X3308 J3108 J3208 J3308 ;
/*business assets - missing flag*/
gen ibizval=0 ;
replace ibizval=1 if J3129>=994 ;
tab ibizval, missing ;
drop J3129 J3229 J3329 ;
/*save tempt data*/
save "${temp_directory}scf95.dta", replace ;

/***1998***/
use "${data_directory}scf98s\p98i6.dta", clear ;
/*IDs*/
keep 
	YY1 Y1 
	X3105 J3105
	X3108 X3208 X3308 J3108 J3208 J3308
	X3111 X3211 X3311 J3111 J3211 J3311
	X3120 X3220 X3320 J3120 J3220 J3320 
	X3121 X3221 X3321 J3121 J3221 J3321
	X918 X1018 J918 J1018 
    X1106 X1117 X1128 J1106 J1117 J1128
    X2710 X2727 X2810 X2827 X2910 X2927 J2710 J2727 J2810 J2827 J2910 J2927 
	X3119 J3119 
	J3129 J3229 J3329 ;
gen nyear=1998 ;
rename Y1 ny1  ;
rename YY1 nyy1 ;
/*firm size*/
egen nfirmsize=rowmax(X3111 X3211 X3311) ;
replace nfirmsize=. if nfirmsize<1 ;
label var nfirmsize "Firm Scale (num. of emp.)" ;
tab nfirmsize, missing ;
drop X3111 X3211 X3311 J3111 J3211 J3311 ;
/*collateralized business loan*/
gen nbizloan=0 ;
replace nbizloan=1 if X3120==1 | X3220==1 | X3320==1 ;
label var nbizloan "Has Personally Collateralized or Cosigned Biz Loan (1=yes)" ;
tab nbizloan, missing ;
drop X3120 X3220 X3320 J3120 J3220 J3320 ;
/*ammount collateralized*/
gen nbizcoll = X3121 + X3221 + X3321 ;
label var nbizcoll "Ammount Collateralized or Guaranteed" ;
sum nbizcoll, detail ;
codebook nbizcoll ;
drop X3121 X3221 X3321 J3121 J3221 J3321 ;
/*any credit for investment in business*/
gen nbizcred=nbizloan ;
replace nbizcred=1 if 
	X918==7 | X1018==7 | 
    X1106==7 | X1117==7 | X1128==7 | 
    X2710==7 | X2727==7 | X2810==7 | 
	X2827==7 | X2910==7 | X2927==7 ;
label var nbizcred "Has Used Any Personal Credit to Invest in Biz (1=yes)" ;
tab nbizcred, missing ;
drop 	
	X918 X1018 
    X1106 X1117 X1128 
    X2710 X2727 X2810 X2827 X2910 X2927 
	J918 J1018 
    J1106 J1117 J1128 
    J2710 J2727 J2810 J2827 J2910 J2927 ;
/*tot num of businesses owned + managed*/
gen ntotfirms=X3105 if X3105!=0 ;
replace ntotfirms=10 if inrange(ntotfirms,10,99) ; 
tab ntotfirms, missing ;
label var ntotfirms "Tot. Num. of Firms Owned & Managed" ;
tab ntotfirms, missing ;
drop X3105 J3105 ;
/*firm type*/
gen nsoleprop=0 ;
replace nsoleprop=1 if X3119==2 ;
gen ncorp=0 ;
replace ncorp=1 if inlist(X3119,3,4) ;
label var nsoleprop "Owns a Sole Proprietorship (1=yes)" ;
label var ncorp "Owns an S, C or Other Corporation (1=yes)" ;
tab X3119 nsoleprop, missing ;
tab X3119 ncorp, missing ;
drop X3119 J3119 ;
/*inherited firm*/
gen ninhfirm=0 ;
replace ninhfirm=1 if inlist(X3108,3,4) ;
label var ninhfirm "Inherited or given main firm (1=yes)" ;
tab X3108 ninhfirm, missing ;
drop X3108 X3208 X3308 J3108 J3208 J3308 ;
/*business assets - missing flag*/
gen ibizval=0 ;
replace ibizval=1 if J3129>=994 ;
tab ibizval, missing ;
drop J3129 J3229 J3329 ;
/*save temp data*/
save "${temp_directory}scf98.dta", replace ;

/***2001***/
use "${data_directory}scf2001s\p01i6.dta", clear ;
/*IDs*/
keep 
	YY1 Y1 
	X3105 J3105
	X3108 X3208 X3308 J3108 J3208 J3308 
	X3111 X3211 X3311 J3111 J3211 J3311
	X3120 X3220 X3320 J3120 J3220 J3320 
	X3121 X3221 X3321 J3121 J3221 J3321
	X918 X1018 J918 J1018 
    X1106 X1117 X1128 J1106 J1117 J1128
    X2710 X2727 X2810 X2827 X2910 X2927 J2710 J2727 J2810 J2827 J2910 J2927 
	X3119 J3119 
	J3129 J3229 J3329 ;
gen nyear=2001 ;
rename Y1 ny1  ;
rename YY1 nyy1 ;
/*firm size*/
egen nfirmsize=rowmax(X3111 X3211 X3311) ;
replace nfirmsize=. if nfirmsize<1 ;
label var nfirmsize "Firm Scale (num. of emp.)" ;
tab nfirmsize, missing ;
drop X3111 X3211 X3311 J3111 J3211 J3311 ;
/*collateralized business loan*/
gen nbizloan=0 ;
replace nbizloan=1 if X3120==1 | X3220==1 | X3320==1 ;
label var nbizloan "Has Personally Collateralized or Cosigned Biz Loan (1=yes)" ;
tab nbizloan, missing ;
drop X3120 X3220 X3320 J3120 J3220 J3320 ;
/*ammount collateralized*/
gen nbizcoll = X3121 + X3221 + X3321 ;
label var nbizcoll "Ammount Collateralized or Guaranteed" ;
sum nbizcoll, detail ;
codebook nbizcoll ;
drop X3121 X3221 X3321 J3121 J3221 J3321 ;
/*any credit for investment in business*/
gen nbizcred=nbizloan ;
replace nbizcred=1 if 
	X918==7 | X1018==7 | 
    X1106==7 | X1117==7 | X1128==7 | 
    X2710==7 | X2727==7 | X2810==7 | 
	X2827==7 | X2910==7 | X2927==7 ;
label var nbizcred "Has Used Any Personal Credit to Invest in Biz (1=yes)" ;
tab nbizcred, missing ;
drop 	
	X918 X1018 
    X1106 X1117 X1128 
    X2710 X2727 X2810 X2827 X2910 X2927 
	J918 J1018 
    J1106 J1117 J1128 
    J2710 J2727 J2810 J2827 J2910 J2927 ;
/*tot num of businesses owned + managed*/
gen ntotfirms=X3105 if X3105!=0 ;
replace ntotfirms=10 if inrange(ntotfirms,10,99) ; 
tab ntotfirms, missing ;
label var ntotfirms "Tot. Num. of Firms Owned & Managed" ;
tab ntotfirms, missing ;
drop X3105 J3105 ;
/*firm type*/
gen nsoleprop=0 ;
replace nsoleprop=1 if X3119==2 ;
gen ncorp=0 ;
replace ncorp=1 if inlist(X3119,3,4) ;
label var nsoleprop "Owns a Sole Proprietorship (1=yes)" ;
label var ncorp "Owns an S, C or Other Corporation (1=yes)" ;
tab X3119 nsoleprop, missing ;
tab X3119 ncorp, missing ;
drop X3119 J3119 ;
/*inherited firm*/
gen ninhfirm=0 ;
replace ninhfirm=1 if inlist(X3108,3,4) ;
label var ninhfirm "Inherited or given main firm (1=yes)" ;
tab X3108 ninhfirm, missing ;
drop X3108 X3208 X3308 J3108 J3208 J3308 ;
/*business assets - missing flag*/
gen ibizval=0 ;
replace ibizval=1 if J3129>=1000 ;
tab ibizval, missing ;
drop J3129 J3229 J3329 ;
/*save temp data*/
save "${temp_directory}scf01.dta", replace ;

/***2004***/
use "${data_directory}scf2004s\p04i6.dta", clear ;
/*IDs*/
keep 
	YY1 Y1 
	X3105 J3105
	X3108 X3208 X3308 J3108 J3208 J3308
	X3111 X3211 X3311 J3111 J3211 J3311
	X3120 X3220 X3320 J3120 J3220 J3320 
	X3121 X3221 X3321 J3121 J3221 J3321
	X918 X1018 J918 J1018 
    X1106 X1117 X1128 J1106 J1117 J1128
    X2710 X2727 X2810 X2827 X2910 X2927 J2710 J2727 J2810 J2827 J2910 J2927 
	X3119 J3119 
	J3129 J3229 J3329 ;
gen nyear=2004 ;
rename Y1 ny1  ;
rename YY1 nyy1 ;
/*firm size*/
egen nfirmsize=rowmax(X3111 X3211 X3311) ;
replace nfirmsize=. if nfirmsize<1 ;
label var nfirmsize "Firm Scale (num. of emp.)" ;
tab nfirmsize, missing ;
drop X3111 X3211 X3311 J3111 J3211 J3311 ;
/*collateralized business loan*/
gen nbizloan=0 ;
replace nbizloan=1 if X3120==1 | X3220==1 | X3320==1 ;
label var nbizloan "Has Personally Collateralized or Cosigned Biz Loan (1=yes)" ;
tab nbizloan, missing ;
drop X3120 X3220 X3320 J3120 J3220 J3320 ;
/*ammount collateralized*/
gen nbizcoll = X3121 + X3221 + X3321 ;
label var nbizcoll "Ammount Collateralized or Guaranteed" ;
sum nbizcoll, detail ;
codebook nbizcoll ;
drop X3121 X3221 X3321 J3121 J3221 J3321 ;
/*any credit for investment in business*/
gen nbizcred=nbizloan ;
replace nbizcred=1 if 
	X918==7 | X1018==7 | 
    X1106==7 | X1117==7 | X1128==7 | 
    X2710==7 | X2727==7 | X2810==7 | 
	X2827==7 | X2910==7 | X2927==7 ;
label var nbizcred "Has Used Any Personal Credit to Invest in Biz (1=yes)" ;
tab nbizcred, missing ;
drop 	
	X918 X1018 
    X1106 X1117 X1128 
    X2710 X2727 X2810 X2827 X2910 X2927 
	J918 J1018 
    J1106 J1117 J1128 
    J2710 J2727 J2810 J2827 J2910 J2927 ;
/*tot num of businesses owned + managed*/
gen ntotfirms=X3105 if X3105!=0 ;
replace ntotfirms=10 if inrange(ntotfirms,10,99) ; 
tab ntotfirms, missing ;
label var ntotfirms "Tot. Num. of Firms Owned & Managed" ;
tab ntotfirms, missing ;
drop X3105 J3105 ;
/*firm type*/
gen nsoleprop=0 ;
replace nsoleprop=1 if X3119==2 ;
gen ncorp=0 ;
replace ncorp=1 if inlist(X3119,3,4) ;
label var nsoleprop "Owns a Sole Proprietorship (1=yes)" ;
label var ncorp "Owns an S, C or Other Corporation (1=yes)" ;
tab X3119 nsoleprop, missing ;
tab X3119 ncorp, missing ;
drop X3119 J3119 ;
/*inherited firm*/
gen ninhfirm=0 ;
replace ninhfirm=1 if inlist(X3108,3,4) ;
label var ninhfirm "Inherited or given main firm (1=yes)" ;
tab X3108 ninhfirm, missing ;
drop X3108 X3208 X3308 J3108 J3208 J3308 ;
/*business assets - missing flag*/
gen ibizval=0 ;
replace ibizval=1 if J3129>=1000 ;
tab ibizval, missing ;
drop J3129 J3229 J3329 ;
/*save temp data*/
save "${temp_directory}scf04.dta", replace ;

/***2007***/
use "${data_directory}scf2007s\p07i6.dta", clear ;
/*IDs*/
keep 
	YY1 Y1 
	X3105 J3105
	X3108 X3208 X3308 J3108 J3208 J3308
	X3111 X3211 X3311 J3111 J3211 J3311
	X3120 X3220 X3320 J3120 J3220 J3320 
	X3121 X3221 X3321 J3121 J3221 J3321
	X918 X1018 J918 J1018 
    X1106 X1117 X1128 J1106 J1117 J1128
    X2710 X2727 X2810 X2827 X2910 X2927 J2710 J2727 J2810 J2827 J2910 J2927 
	X3119 J3119 
	J3129 J3229 J3329 ;
gen nyear=2007 ;
rename Y1 ny1  ;
rename YY1 nyy1 ;
/*firm size*/
egen nfirmsize=rowmax(X3111 X3211 X3311) ;
replace nfirmsize=. if nfirmsize<1 ;
label var nfirmsize "Firm Scale (num. of emp.)" ;
tab nfirmsize, missing ;
drop X3111 X3211 X3311 J3111 J3211 J3311 ;
/*collateralized business loan*/
gen nbizloan=0 ;
replace nbizloan=1 if X3120==1 | X3220==1 | X3320==1 ;
label var nbizloan "Has Personally Collateralized or Cosigned Biz Loan (1=yes)" ;
tab nbizloan, missing ;
drop X3120 X3220 X3320 J3120 J3220 J3320 ;
/*ammount collateralized*/
gen nbizcoll = X3121 + X3221 + X3321 ;
label var nbizcoll "Ammount Collateralized or Guaranteed" ;
sum nbizcoll, detail ;
codebook nbizcoll ;
drop X3121 X3221 X3321 J3121 J3221 J3321 ;
/*any credit for investment in business*/
gen nbizcred=nbizloan ;
replace nbizcred=1 if 
	X918==7 | X1018==7 | 
    X1106==7 | X1117==7 | X1128==7 | 
    X2710==7 | X2727==7 | X2810==7 | 
	X2827==7 | X2910==7 | X2927==7 ;
label var nbizcred "Has Used Any Personal Credit to Invest in Biz (1=yes)" ;
tab nbizcred, missing ;
drop 	
	X918 X1018 
    X1106 X1117 X1128 
    X2710 X2727 X2810 X2827 X2910 X2927 
	J918 J1018 
    J1106 J1117 J1128 
    J2710 J2727 J2810 J2827 J2910 J2927 ;
/*tot num of businesses owned + managed*/
gen ntotfirms=X3105 if X3105!=0 ;
replace ntotfirms=10 if inrange(ntotfirms,10,99) ; 
tab ntotfirms, missing ;
label var ntotfirms "Tot. Num. of Firms Owned & Managed" ;
tab ntotfirms, missing ;
drop X3105 J3105 ;
/*firm type*/
gen nsoleprop=0 ;
replace nsoleprop=1 if X3119==2 ;
gen ncorp=0 ;
replace ncorp=1 if inlist(X3119,3,4) ;
label var nsoleprop "Owns a Sole Proprietorship (1=yes)" ;
label var ncorp "Owns an S, C or Other Corporation (1=yes)" ;
tab X3119 nsoleprop, missing ;
tab X3119 ncorp, missing ;
drop X3119 J3119 ;
/*inherited firm*/
gen ninhfirm=0 ;
replace ninhfirm=1 if inlist(X3108,3,4) ;
label var ninhfirm "Inherited or given main firm (1=yes)" ;
tab X3108 ninhfirm, missing ;
drop X3108 X3208 X3308 J3108 J3208 J3308 ;
/*business assets - missing flag*/
gen ibizval=0 ;
replace ibizval=1 if J3129>=1200 ;
tab ibizval, missing ;
drop J3129 J3229 J3329 ;
/*save temp data*/
save "${temp_directory}scf07.dta", replace ;

/***2010***/
use "${data_directory}scf2010s\p10i6.dta", clear ;
/*IDs*/
keep 
	YY1 Y1 
	X3105 J3105
	X3108 X3208 J3108 J3208	 
	X3111 X3211 J3111 J3211
	X3120 X3220 J3120 J3220  
	X3121 X3221 J3121 J3221
	X918 X1018 J918 J1018 
    X1106 X1117 X1128 J1106 J1117 J1128
    X2710 X2727 X2810 X2827 X2910 X2927 J2710 J2727 J2810 J2827 J2910 J2927 
	X3119 J3119 
	J3129 J3229 ;
gen nyear=2010 ;
rename Y1 ny1  ;
rename YY1 nyy1 ;
/*firm size*/
egen nfirmsize=rowmax(X3111 X3211) ;
replace nfirmsize=. if nfirmsize<1 ;
label var nfirmsize "Firm Scale (num. of emp.)" ;
tab nfirmsize, missing ;
drop X3111 X3211 J3111 J3211 ;
/*collateralized business loan*/
gen nbizloan=0 ;
replace nbizloan=1 if X3120==1 | X3220==1 ;
label var nbizloan "Has Personally Collateralized or Cosigned Biz Loan (1=yes)" ;
tab nbizloan, missing ;
drop X3120 X3220 J3120 J3220 ;
/*ammount collateralized*/
gen nbizcoll = X3121 + X3221 ;
label var nbizcoll "Ammount Collateralized or Guaranteed" ;
sum nbizcoll, detail ;
codebook nbizcoll ;
drop X3121 X3221 J3121 J3221 ;
/*any credit for investment in business*/
gen nbizcred=nbizloan ;
replace nbizcred=1 if 
	X918==7 | X1018==7 | 
    X1106==7 | X1117==7 | X1128==7 | 
    X2710==7 | X2727==7 | X2810==7 | 
	X2827==7 | X2910==7 | X2927==7 ;
label var nbizcred "Has Used Any Personal Credit to Invest in Biz (1=yes)" ;
tab nbizcred, missing ;
drop 	
	X918 X1018 
    X1106 X1117 X1128 
    X2710 X2727 X2810 X2827 X2910 X2927 
	J918 J1018 
    J1106 J1117 J1128 
    J2710 J2727 J2810 J2827 J2910 J2927 ;
/*tot num of businesses owned + managed*/
gen ntotfirms=X3105 if X3105!=0 ;
replace ntotfirms=10 if inrange(ntotfirms,10,99) ; 
tab ntotfirms, missing ;
label var ntotfirms "Tot. Num. of Firms Owned & Managed" ;
tab ntotfirms, missing ;
drop X3105 J3105 ;
/*firm type*/
gen nsoleprop=0 ;
replace nsoleprop=1 if X3119==2 ;
gen ncorp=0 ;
replace ncorp=1 if inlist(X3119,3,4) ;
label var nsoleprop "Owns a Sole Proprietorship (1=yes)" ;
label var ncorp "Owns an S, C or Other Corporation (1=yes)" ;
tab X3119 nsoleprop, missing ;
tab X3119 ncorp, missing ;
drop X3119 J3119 ;
/*inherited firm*/
gen ninhfirm=0 ;
replace ninhfirm=1 if inlist(X3108,3,4,10) ;
label var ninhfirm "Inherited or given main firm (1=yes)" ;
tab X3108 ninhfirm, missing ;
drop X3108 X3208 J3108 J3208 ;
/*business assets - missing flag*/
gen ibizval=0 ;
replace ibizval=1 if J3129>=1200 ;
tab ibizval, missing ;
drop J3129 J3229 ;
/*save temp data*/
save "${temp_directory}scf10.dta", replace ;

/***2013***/
use "${data_directory}scf2013s\p13i6.dta", clear ;
/*IDs*/
keep 
	YY1 Y1 
	X3105 J3105
	X3108 X3208 J3108 J3208
	X3111 X3211 J3111 J3211
	X3120 X3220 J3120 J3220  
	X3121 X3221 J3121 J3221
	X918 X1018 J918 J1018 
    X1106 X1117 X1128 J1106 J1117 J1128
    X2710 X2727 X2810 X2827 X2910 X2927 J2710 J2727 J2810 J2827 J2910 J2927 
	X3119 J3119 
	J3129 J3229 ;
gen nyear=2013 ;
rename Y1 ny1  ;
rename YY1 nyy1 ;
/*firm size*/
egen nfirmsize=rowmax(X3111 X3211) ;
replace nfirmsize=. if nfirmsize<1 ;
label var nfirmsize "Firm Scale (num. of emp.)" ;
tab nfirmsize, missing ;
drop X3111 X3211 J3111 J3211 ;
/*collateralized business loan*/
gen nbizloan=0 ;
replace nbizloan=1 if X3120==1 | X3220==1 ;
label var nbizloan "Has Personally Collateralized or Cosigned Biz Loan (1=yes)" ;
tab nbizloan, missing ;
drop X3120 X3220 J3120 J3220 ;
/*ammount collateralized*/
gen nbizcoll = X3121 + X3221 ;
label var nbizcoll "Ammount Collateralized or Guaranteed" ;
sum nbizcoll, detail ;
codebook nbizcoll ;
drop X3121 X3221 J3121 J3221 ;
/*any credit for investment in business*/
gen nbizcred=nbizloan ;
replace nbizcred=1 if 
	X918==7 | X1018==7 | 
    X1106==7 | X1117==7 | X1128==7 | 
    X2710==7 | X2727==7 | X2810==7 | 
	X2827==7 | X2910==7 | X2927==7 ;
label var nbizcred "Has Used Any Personal Credit to Invest in Biz (1=yes)" ;
tab nbizcred, missing ;
drop 	
	X918 X1018 
    X1106 X1117 X1128 
    X2710 X2727 X2810 X2827 X2910 X2927 
	J918 J1018 
    J1106 J1117 J1128 
    J2710 J2727 J2810 J2827 J2910 J2927 ;
/*tot num of businesses owned + managed*/
gen ntotfirms=X3105 if X3105!=0 ;
replace ntotfirms=10 if inrange(ntotfirms,10,99) ; 
tab ntotfirms, missing ;
label var ntotfirms "Tot. Num. of Firms Owned & Managed" ;
tab ntotfirms, missing ;
drop X3105 J3105 ;
/*firm type*/
gen nsoleprop=0 ;
replace nsoleprop=1 if X3119==2 ;
gen ncorp=0 ;
replace ncorp=1 if inlist(X3119,3,4) ;
label var nsoleprop "Owns a Sole Proprietorship (1=yes)" ;
label var ncorp "Owns an S, C or Other Corporation (1=yes)" ;
tab X3119 nsoleprop, missing ;
tab X3119 ncorp, missing ;
drop X3119 J3119 ;
/*inherited firm*/
gen ninhfirm=0 ;
replace ninhfirm=1 if inlist(X3108,3,4,10) ;
label var ninhfirm "Inherited or given main firm (1=yes)" ;
tab X3108 ninhfirm, missing ;
drop X3108 X3208 J3108 J3208 ;
/*business assets - missing flag*/
gen ibizval=0 ;
replace ibizval=1 if J3129>=1200 ;
tab ibizval, missing ;
drop J3129 J3229 ;
/*save temp data*/
save "${temp_directory}scf13.dta", replace ;

/***2016***/
use "${data_directory}scf2016s\p16i6.dta", clear ;
/*IDs*/
keep 
	YY1 Y1 
	X3105 J3105
	X3108 X3208 J3108 J3208
	X3111 X3211 J3111 J3211
	X3120 X3220 J3120 J3220  
	X3121 X3221 J3121 J3221
	X918 X1018 J918 J1018 
    X1106 X1117 X1128 J1106 J1117 J1128
    X2710 X2727 X2810 X2827 X2910 X2927 J2710 J2727 J2810 J2827 J2910 J2927 
	X3119 J3119 
	J3129 J3229 ;
gen nyear=2016 ;
rename Y1 ny1  ;
rename YY1 nyy1 ;
/*firm size*/
egen nfirmsize=rowmax(X3111 X3211) ;
replace nfirmsize=. if nfirmsize<1 ;
label var nfirmsize "Firm Scale (num. of emp.)" ;
tab nfirmsize, missing ;
drop X3111 X3211 J3111 J3211 ;
/*collateralized business loan*/
gen nbizloan=0 ;
replace nbizloan=1 if X3120==1 | X3220==1 ;
label var nbizloan "Has Personally Collateralized or Cosigned Biz Loan (1=yes)" ;
tab nbizloan, missing ;
drop X3120 X3220 J3120 J3220 ;
/*ammount collateralized*/
gen nbizcoll = X3121 + X3221 ;
label var nbizcoll "Ammount Collateralized or Guaranteed" ;
sum nbizcoll, detail ;
codebook nbizcoll ;
drop X3121 X3221 J3121 J3221 ;
/*any credit for investment in business*/
gen nbizcred=nbizloan ;
replace nbizcred=1 if 
	X918==7 | X1018==7 | 
    X1106==7 | X1117==7 | X1128==7 | 
    X2710==7 | X2727==7 | X2810==7 | 
	X2827==7 | X2910==7 | X2927==7 ;
label var nbizcred "Has Used Any Personal Credit to Invest in Biz (1=yes)" ;
tab nbizcred, missing ;
drop 	
	X918 X1018 
    X1106 X1117 X1128 
    X2710 X2727 X2810 X2827 X2910 X2927 
	J918 J1018 
    J1106 J1117 J1128 
    J2710 J2727 J2810 J2827 J2910 J2927 ;
/*tot num of businesses owned + managed*/
gen ntotfirms=X3105 if X3105!=0 ;
replace ntotfirms=10 if inrange(ntotfirms,10,99) ; 
tab ntotfirms, missing ;
label var ntotfirms "Tot. Num. of Firms Owned & Managed" ;
tab ntotfirms, missing ;
drop X3105 J3105 ;
/*firm type*/
gen nsoleprop=0 ;
replace nsoleprop=1 if X3119==2 ;
gen ncorp=0 ;
replace ncorp=1 if inlist(X3119,3,4) ;
label var nsoleprop "Owns a Sole Proprietorship (1=yes)" ;
label var ncorp "Owns an S, C or Other Corporation (1=yes)" ;
tab X3119 nsoleprop, missing ;
tab X3119 ncorp, missing ;
drop X3119 J3119 ;
/*inherited firm*/
gen ninhfirm=0 ;
replace ninhfirm=1 if inlist(X3108,3,4,10) ;
label var ninhfirm "Inherited or given main firm (1=yes)" ;
tab X3108 ninhfirm, missing ;
drop X3108 X3208 J3108 J3208 ;
/*business assets - missing flag*/
gen ibizval=0 ;
replace ibizval=1 if J3129>=1200 ;
tab ibizval, missing ;
drop J3129 J3229 ;
/*save temp data*/
save "${temp_directory}scf16.dta", replace ;

/***2019***/
use "${data_directory}scf2019s\p19i6.dta", clear ;
/*IDs*/
keep 
	yy1 y1 
	x3105 j3105
	x3108 x3208 j3108 j3208
	x3111 x3211 j3111 j3211
	x3120 x3220 j3120 j3220  
	x3121 x3221 j3121 j3221
	x918 x1018 j918 j1018 
    x1106 x1117 x1128 j1106 j1117 j1128
    x2710 x2727 x2810 x2827 x2910 x2927 j2710 j2727 j2810 j2827 j2910 j2927 
	x3119 j3119 
	j3129 j3229 ;
gen nyear=2019 ;
rename y1 ny1  ;
rename yy1 nyy1 ;
/*firm size*/
egen nfirmsize=rowmax(x3111 x3211) ;
replace nfirmsize=. if nfirmsize<1 ;
label var nfirmsize "Firm Scale (num. of emp.)" ;
tab nfirmsize, missing ;
drop x3111 x3211 j3111 j3211 ;
/*collateralized business loan*/
gen nbizloan=0 ;
replace nbizloan=1 if x3120==1 | x3220==1 ;
label var nbizloan "Has Personally Collateralized or Cosigned Biz Loan (1=yes)" ;
tab nbizloan, missing ;
drop x3120 x3220 j3120 j3220 ;
/*ammount collateralized*/
gen nbizcoll = x3121 + x3221 ;
label var nbizcoll "Ammount Collateralized or Guaranteed" ;
sum nbizcoll, detail ;
codebook nbizcoll ;
drop x3121 x3221 j3121 j3221 ;
/*any credit for investment in business*/
gen nbizcred=nbizloan ;
replace nbizcred=1 if 
	x918==7 | x1018==7 | 
    x1106==7 | x1117==7 | x1128==7 | 
    x2710==7 | x2727==7 | x2810==7 | 
	x2827==7 | x2910==7 | x2927==7 ;
label var nbizcred "Has Used Any Personal Credit to Invest in Biz (1=yes)" ;
tab nbizcred, missing ;
drop 	
	x918 x1018 
    x1106 x1117 x1128 
    x2710 x2727 x2810 x2827 x2910 x2927 
	j918 j1018 
    j1106 j1117 j1128 
    j2710 j2727 j2810 j2827 j2910 j2927 ;
/*tot num of businesses owned + managed*/
gen ntotfirms=x3105 if x3105!=0 ;
replace ntotfirms=10 if inrange(ntotfirms,10,99) ; 
tab ntotfirms, missing ;
label var ntotfirms "Tot. Num. of Firms Owned & Managed" ;
tab ntotfirms, missing ;
drop x3105 j3105 ;
/*firm type*/
gen nsoleprop=0 ;
replace nsoleprop=1 if x3119==2 ;
gen ncorp=0 ;
replace ncorp=1 if inlist(x3119,3,4) ;
label var nsoleprop "Owns a Sole Proprietorship (1=yes)" ;
label var ncorp "Owns an S, C or Other Corporation (1=yes)" ;
tab x3119 nsoleprop, missing ;
tab x3119 ncorp, missing ;
drop x3119 j3119 ;
/*inherited firm*/
gen ninhfirm=0 ;
replace ninhfirm=1 if inlist(x3108,3,4,10) ;
label var ninhfirm "Inherited or given main firm (1=yes)" ;
tab x3108 ninhfirm, missing ;
drop x3108 x3208 j3108 j3208 ;
/*business assets - missing flag*/
gen ibizval=0 ;
replace ibizval=1 if j3129>=1200 ;
tab ibizval, missing ;
drop j3129 j3229 ;
/*save temp data*/
save "${temp_directory}scf19.dta", replace ;

/**********
APPEND DATA 
***********/
use "${temp_directory}scf89.dta", clear ;
append using "${temp_directory}scf92.dta" ;
append using "${temp_directory}scf95.dta" ;
append using "${temp_directory}scf98.dta" ;
append using "${temp_directory}scf01.dta" ;
append using "${temp_directory}scf04.dta" ;
append using "${temp_directory}scf07.dta" ;
append using "${temp_directory}scf10.dta" ;
append using "${temp_directory}scf13.dta" ;
append using "${temp_directory}scf16.dta" ;
append using "${temp_directory}scf19.dta" ;

/**********
RECODE VARS
***********/
gen nempnum=nfirmsize ;
replace nempnum=0 if nfirmsize==. ;
label var nempnum "Number of Employees" ;
recode nfirmsize (-99/-1=.) (0/9=1) (10/99=2) (100/9999999=3) ;
tab nfirmsize, missing ;

/*********
LABEL VARS
**********/
label define nbizloan_lbl 0 "no_loan" 1 "loan" ;
label values nbizloan nbizloan_lbl ;
label define nbizcred_lbl 0 "no_credit" 1 "credit" ;
label values nbizcred nbizcred_lbl ;
label define nfirmsize_lbl 1 "0to9" 2 "10to99" 3 "100plus" ;
label values nfirmsize nfirmsize_lbl ;

/********
SAVE DATA
*********/
save "${data_directory}v01_fed_scf.dta", replace ;
codebook ;

/**************
ERASE TEMP DATA 
***************/
erase "${temp_directory}scf89.dta" ;
erase "${temp_directory}scf92.dta" ;
erase "${temp_directory}scf95.dta" ;
erase "${temp_directory}scf98.dta" ;
erase "${temp_directory}scf01.dta" ;
erase "${temp_directory}scf04.dta" ;
erase "${temp_directory}scf07.dta" ;
erase "${temp_directory}scf10.dta" ;
erase "${temp_directory}scf13.dta" ;
erase "${temp_directory}scf16.dta" ;
erase "${temp_directory}scf19.dta" ;

log close ;
