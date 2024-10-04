#delimit ;
capture log close ;
capture clear all ;
set more off ;

global data_directory "C:\Users\Geoff\Dropbox\D\projects\scf_biz_assets\data\berkeley_scf\" ;
global log_directory "C:\Users\Geoff\Dropbox\D\projects\scf_biz_assets\programs\_LOGS\" ;
global temp_directory "C:\Users\Geoff\Dropbox\D\projects\scf_biz_assets\data\_TEMP\" ;

log using "${log_directory}03_create_v03_berkeley_scf.log", replace ;

/**************************************************************
PROGRAM NAME: 03_create_v03_berkeley_scf.do
PURPOSE: create new variables and recode variables for analysis
***************************************************************/

/*********
INPUT DATA
**********/
use "${data_directory}v02_berkeley_scf.dta", clear ; 

/*************************
CREATE/RECODE ID VARIABLES
**************************/
/***RENAME ID VARIABLES***/
rename CASEID ncaseid ;
rename YEAR nyear ;
rename X1 nx1 ;
rename XX1 nxx1  ;
rename Y1 ny1  ;
rename YY1 nyy1 ;
replace ny1=nx1 if nyear==1989 ;
replace nyy1=nxx1 if nyear==1989 ;
/***RESPONDENT ID VARIABLE***/ 
gen nrespid=nxx1 if nyear==1989 ;
replace nrespid=nyy1 if nyear!=1989 ;
label var nrespid "Respondent ID (unique within years)" ;
codebook nrespid ;
/***UNIQUE RESPONDENT ID VARIABLE***/
gen double nurespid=(10000*nyear)+nrespid ;
label var nurespid "Unique Respondent ID (unique across years)" ;
codebook nurespid ;
/***IMPUTATION/REPLICATE ID VARIABLE***/
sort nurespid ;
by nurespid: gen nminum=_n ;
label var nminum "Imputation/replicate Number ID (unique within respondent-years)" ;
codebook nminum ;

/**********************************
CREATE/RECODE DEMOGRAPHIC VARIABLES
***********************************/
/***AGE***/
gen nage=AGE ;
label var nage "Age in years" ;
sum nage, detail ;
/***GENDER***/
gen nfemale=0 ;
replace nfemale=1 if HHSEX==2 ;
label var nfemale "Gender Dummy (1=Female)" ;
tab HHSEX nfemale, missing ;
/***RACE***/
gen nwhite=0 ;
replace nwhite=1 if RACECL==1 ;
label var nwhite "Race Dummy (1=white nonhispanic)" ;
tab RACECL nwhite, missing ;
/***EDUCATION***/
gen neducyr=EDUC ;
replace neducyr=0 if EDUC==-1 ;
replace neducyr=6 if EDUC==. ;
recode neducyr (0/11=0) (12/14=1) ;
label define neducyr_lbl 0 "lt_udegree" 1 "udegree" ;
label values neducyr neducyr_lbl ;
label var neducyr "Completed Schooling" ;
tab EDUC neducyr, missing ;
/***LABOR FORCE STATUS***/
gen nworking=0 ;
replace nworking=1 if inrange(OCCAT1,1,2) ;
label var nworking "LABOR FORCE STATUS DUMMY (1=IN LABOR FORCE)" ;
tab OCCAT1 nworking, missing ;
/***BUSINESS OWNERSHIP***/
gen nbizown=0 ;
replace nbizown=1 if HBUS==1 & BUS>0 ;
label var nbizown "Business Owner Dummy (1=owners)" ;
tab HBUS nbizown, missing ;
/***OCCUPATION***/
gen nocc=0 ;
replace nocc=1 if OCCAT2==1 ;
label var nocc "Prof/Mgr Dummy (1=prof/mgr)" ;
tab OCCAT2 nocc, missing ;
/***INDUSTRY***/
gen nindustry=0 ;
replace nindustry=1 if INDCAT==1 ;
label var nindustry "Goods Producing Dummy (1=goods producing)" ;
label define nindustry_lbl 0 "service" 1 "goods" ;
label values nindustry nindustry_lbl ;
tab INDCAT nindustry, missing ;

/*****************************
CREATE/RECODE INCOME VARIABLES
******************************/
/***TOTAL INCOME***/
/*DOLLARS*/
gen ntotinc=INCOME ;
replace ntotinc=0 if INCOME<0 ;
label var ntotinc "Total Household Income" ;
sum ntotinc, detail ;
/*INVERSE HYPERBOLIC SINE TRANSFORM*/
gen nihstotinc=ln(ntotinc+sqrt((ntotinc^2)+1)) ;
label var nihstotinc "Inv Hyperbolic Sine of Total Household Income" ;
sum nihstotinc, detail ;
/***LABOR INCOME***/
/*DOLLARS*/
gen nlabinc=WAGEINC ;
replace nlabinc=0 if WAGEINC<0 ;
label var nlabinc "Labor Income of Household" ;
sum nlabinc, detail ;
/*INVERSE HYPERBOLIC SINE TRANSFORM*/
gen nihslabinc=ln(nlabinc+sqrt((nlabinc^2)+1)) ;
label var nihslabinc "Inv Hyperbolic Sine of Labor Income" ;
sum nihslabinc, detail ;
/***BUSINESS INCOME***/
/*DOLLARS*/
gen nbizinc=BUSSEFARMINC ;
quietly qreg nbizinc i.nyear [pw=WGT], q(0.005) ;
quietly predict q, xb ;
replace nbizinc=q if nbizinc<q ;
drop q ;
label var nbizinc "Business Income of Household" ;
sum nbizinc, detail ;
/*INVERSE HYPERBOLIC SINE TRANSFORM*/
gen nihsbizinc=ln(nbizinc+sqrt((nbizinc^2)+1)) ;
label var nihsbizinc "Inv Hyperbolic Sine of Business Income" ;
sum nihsbizinc, detail ;
/***CAPITAL INCOME***/
/*DOLLARS*/
gen ncapinc=INTDIVINC ;
quietly qreg ncapinc i.nyear [pw=WGT], q(0.005) ;
quietly predict q, xb ;
replace ncapinc=q if ncapinc<q ;
drop q ;
label var ncapinc "Capital Income of Household" ;
sum ncapinc, detail ;
/*INVERSE HYPERBOLIC SINE TRANSFORM*/
gen nihscapinc=ln(ncapinc+sqrt((ncapinc^2)+1)) ;
label var nihscapinc "Inv Hyperbolic Sine of Capital Income" ;
sum nihscapinc, detail ;
/***CAPITAL INCOME + CAPITAL GAINS***/
/*DOLLARS*/
gen ncapinckg=INTDIVINC+KGINC+KGHOUSE+KGORE+KGSTMF ;
quietly qreg ncapinckg i.nyear [pw=WGT], q(0.005) ;
quietly predict q, xb ;
replace ncapinckg=q if ncapinckg<q ;
drop q ;
label var ncapinckg "Capital Income (Incl. Capital Gains) of Household" ;
sum ncapinckg, detail ;
/***ACCOUNTING INCOME***/
/*DOLLARS*/
gen ntotearn=nlabinc+nbizinc+ncapinc ;
label var ntotearn "Market Earnings of Household" ;
sum ntotearn, detail ;

/*****************************
CREATE/RECODE ASSETS VARIABLES
******************************/
/***VALUE OF BUSINESS ASSETS***/
/*DOLLARS*/
gen ntotbizval=BUS ;
replace ntotbizval=0 if ntotbizval<0 ;
label var ntotbizval "Total Value of Business" ;
sum ntotbizval, detail ;
/*LOG-NORMAL HURDLE TRANSFORM*/
gen nbizvalpos=0 ;
replace nbizvalpos=1 if ntotbizval>0 ;
gen nlogbizval=ln(ntotbizval) if ntotbizval>0 ;
label var nbizvalpos "Nonzero Business Assets" ;
label var nlogbizval "Log of Total Biz Value | Nonzero Assets" ;
sum nbizvalpos nlogbizval ;
/***VALUE OF NONFARM BUSINESS ASSETS***/
/*DOLLARS*/
gen ntotbizvalnf=BUS-FARMBUS ;
replace ntotbizvalnf=0 if ntotbizvalnf<1 ;
label var ntotbizvalnf "Total Value of Nonfarm Business" ;
sum ntotbizvalnf, detail ;
/*LOG-NORMAL HURDLE TRANSFORM*/
gen nbizvalposnf=0 ;
replace nbizvalposnf=1 if ntotbizvalnf>0 ;
gen nlogbizvalnf=ln(ntotbizvalnf) if ntotbizvalnf>0 ;
label var nbizvalposnf "Dummy for Nonzero Nonfarm Business Assets" ;
label var nlogbizvalnf "Log of Total Nonfarm Biz Value" ;
sum nbizvalposnf nlogbizvalnf ;
/***VALUE OF ACTIVELY MANAGED BUSINESS ASSETS***/
/*DOLLARS*/
gen nactbizval=ACTBUS ;
replace nactbizval=0 if nactbizval<1 ;
label var nactbizval "Total Value of Actively Managed Business Assets" ;
sum nactbizval, detail ;
/*LOG-NORMAL HURDLE TRANSFORM*/
gen nactbizvalpos=0 ;
replace nactbizvalpos=1 if nactbizval>0 ;
gen nlogactbizval=ln(nactbizval) if nactbizval>0 ;
label var nactbizvalpos "Dummy for Nonzero Actively Managed Business Assets" ;
label var nlogactbizval "Log of Total Actively Managed Business Assets" ;
sum nactbizvalpos nlogactbizval ;
/***VALUE OF FINANCIAL ASSETS***/
/*DOLLARS*/
gen ntotcapval=FIN ;
replace ntotcapval=0 if ntotcapval<0 ;
label var ntotcapval "Total Value of Financial Assets" ;
sum ntotcapval, detail ;
/***VALUE OF REAL ESTATE ASSETS***/
/*DOLLARS*/
gen ntotrealval=(HOUSES+ORESRE)-(MRTHEL+RESDBT)+NNRESRE ;
quietly qreg ntotrealval i.nyear [pw=WGT], q(0.001) ;
quietly predict q, xb ;
replace ntotrealval=q if ntotrealval<q ;
drop q ;
label var ntotrealval "Net Equity in Real Estate Assets" ;
sum ntotrealval, detail ;
/***NET WORTH***/
/*DOLLARS*/
gen ntotwealth=ASSET-DEBT ;
quietly qreg ntotwealth i.nyear [pw=WGT], q(0.001) ;
quietly predict q, xb ;
replace ntotwealth=q if ntotwealth<q ;
drop q ;
label var ntotwealth "Net Worth" ;
sum ntotwealth, detail ;
/*TOP 1% INDICATOR FOR NET WORTH*/
gen ntop1wealth=0 ;
quietly qreg ntotwealth i.nyear [pw=WGT], q(0.99) ;
quietly predict q, xb ;
replace ntop1wealth=1 if ntotwealth>q ;
drop q ;
label var ntop1wealth "In Top 1% of Net Worth Distn" ;
tab nyear ntop1wealth [iw=WGT], row ;
/***NET WORTH EXCLUDING BUSINESS ASSETS***/
/*DOLLARS*/
gen naltwealth=ntotwealth-ntotbizval ;
label var naltwealth "Net Worth Excl Biz Assets" ;
quietly qreg naltwealth i.nyear [pw=WGT], q(0.001) ;
quietly predict q, xb ;
replace naltwealth=q if ntotrealval<q ;
drop q ;
sum naltwealth, detail ;

/*****************************
CREATE/RECODE DESIGN VARIABLES
******************************/
/***SAMPLING WEIGHTS***/
gen nsampwt=WGT ;
forval j=1989(3)2019 { ;
		quietly sum nsampwt if nyear==`j' ;
		quietly replace nsampwt=nsampwt/r(mean) if nyear==`j' ;
		} ;
sort nyear ;
by nyear: sum nsampwt ;

/********
SAVE DATA
*********/
keep n* ;
save "${data_directory}v03_berkeley_scf.dta", replace ;
*codebook ;

log close ;
