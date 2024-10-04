#delimit ;
capture log close ;
capture clear all ;
set more off ;

global data_directory "C:\Users\Geoff\Dropbox\D\projects\scf_biz_assets\data\berkeley_scf\" ;
global log_directory "C:\Users\Geoff\Dropbox\D\projects\scf_biz_assets\programs\_LOGS\" ;

log using "${log_directory}02_create_v02_berkeley_scf.log", replace ;

/******************************************
PROGRAM NAME: 02_create_v02_berkeley_scf.do
PURPOSE: Read in berkeley harmonized data
*******************************************/

/*********************
DEFINE VARIABLE LABELS
**********************/
label data "SCF Combined Extract Data" ;
label define AGECL     1 "<35" 2 "35-44" 3 "45-54" 4 "55-64" 5 "65-74" 
                       6 ">=75" ;
label define HHSEX     1 "male" 2 "female" ;
label define EDCL      1 "no high school diploma/GED" 
                       2 "high school diploma or GED" 3 "some college" 
                       4 "college degree" ;
label define EDUC      -1 "LESS THAN 1ST GRADE" 
                       1 "1ST, 2ND, 3RD, OR 4TH GRADE" 2 "5TH OR 6TH GRADE" 
                       3 "7TH OR 8TH GRADE" 4 "9TH GRADE" 5 "10TH GRADE" 
                       6 "11TH GRADE" 7 "12TH GRADE, NO DIPLOMA" 
                       8 "HIGH SCHOOL GRADUATE - HIGH SCHOOL DIPLOMA OR EQUIVALENT" 
                       9 "SOME COLLEGE BUT NO DEGREE" 
                       10 "ASSOCIATE DEGREE IN COLLEGE - OCCUPATION/VOCATION PROGRAM" 
                       11 "ASSOCIATE DEGREE IN COLLEGE - ACADEMIC PROGRAM" 
                       12 "BACHELOR'S DEGREE (FOR EXAMPLE: BA, AB, BS)" 
                       13 "MASTER'S DEGREE" 
                       14 "DOCTORATE OR PROFESSIONAL SCHOOL DEGREE" ;
label define FAMSTRUCT 1 "not married/LWP + children" 
                       2 "not married/LWP + no children + reference person under 55" 
                       3 "not married/LWP + no children + reference person 55 or older" 
                       4 "married/LWP+ children" 
                       5 "married/LWP + no children" ;
label define HOUSECL   1 "owns ranch/farm/mobile home/house/condo/coop/etc." 
                       2 "otherwise" ;
label define MARRIED   1 "married/living with partner" 
                       2 "neither married nor living with partner" ;
label define NOCHK     0 "has checking account" 1 "no checking account" ;
label define OCCAT1    1 "work for someone else" 
                       2 "self-employed/partnership" 
                       3 "retired/disabled + (student/homemaker/misc. not working and age 65 or older)" 
                       4 "other groups not working (mainly those under 65 and out of the labor force)" ;
label define OCCAT2    1 "managerial/professional" 
                       2 "technical/sales/services" 
                       3 "other (incl. production/craft/repair workers, operators, laborers, farmers, foresters, fishers)" 
                       4 "not working" ;
label define RACE      1 "white non-Hispanic" 2 "black / African American" 
                       3 "Hispanic" 5 "Other" ;
label define RACECL    1 "white non-Hispanic" 2 "nonwhite or Hispanic" ;
label define RACECL4   1 "White, non-mixed race" 
                       2 "Black/African-American, non-mixed race" 
                       3 "Hispanic/Latino, non-mixed race" 4 "Other" ;
label define LIFECL    
                       1 "reference person under 55 + not married/LWP + no children" 
                       2 "reference person under 55 + married/LWP + no children" 
                       3 "reference person under 55 + married/LWP + children" 
                       4 "reference person under 55 + not married/LWP + children" 
                       5 "reference person 55 or older and working" 
                       6 "reference person 55 or older and not working" ;
label define LF        0 "not working" 1 "working in some way" ;
label define INDCAT    1 "mining + construction + manufacturing" 
                       2 "transportation + communications + utilities and sanitary services + wholesale trade + finance, insurance and real estate" 
                       4 "agriculture + retail trade + services + public administration" ;
label define EXPENSHILO 1 "unusually high" 2 "unusually low" 3 "normal" ;
label define TURNDOWN  0 "no" 1 "yes" ;
label define FEARDENIAL 0 "no" 1 "yes" ;
label define TURNFEAR  0 "no" 1 "yes" ;
label define BNKRUPLAST5 0 "no" 1 "yes" ;
label define FORECLLAST5 0 "No" 1 "Yes" ;
label define NOCCBAL   0 "no" 1 "yes" ;
label define ASSETCAT  1 "0-20" 2 "20-39.9" 3 "40-59.9" 4 "60-79.9" 
                       5 "80-89.9" 6 "90-100" ;
label define HLIQ      0 "no" 1 "yes" ;
label define ANYPEN    0 "no" 1 "yes" ;
label define HBUS      0 "no" 1 "yes" ;
label define BUSVEH    0 "no" 1 "yes" ;
label define HSTOCKS   0 "no" 1 "yes" ;
label define DBPLANCJ  0 "no" 1 "yes" ;
label define DBPLANT   0 "no" 1 "yes" ;
label define DCPLANCJ  0 "no" 1 "yes" ;
label define BPLANCJ   0 "no" 1 "yes" ;
label define HBROK     0 "no" 1 "yes" ;
label define HTRAD     0 "no" 1 "yes" ;
label define OWN       0 "no" 1 "yes" ;
label define LEASE     0 "no" 1 "yes" ;
label define HDEBT     0 "No" 1 "Yes" ;
label define LATE      0 "no" 1 "yes" ;
label define LATE60    0 "no" 1 "yes" ;
label define HPAYDAY   0 "no" 1 "yes" ;
label define HMORT2    0 "no" 1 "yes" ;
label define HPRIM_MORT 0 "no" 1 "yes" ;
label define HSEC_MORT 0 "no" 1 "yes" ;
label define PURCH1    0 "no" 1 "yes" ;
label define PURCH2    0 "no" 1 "yes" ;
label define REFIN_EVER 0 "no" 1 "yes" ;
label define HELOC_YN  0 "no" 1 "yes" ;
label define PIR40     0 "no" 1 "yes" ;
label define BSHOPGRDL 0 "no" 1 "yes" ;
label define BSHOPMODR 0 "no" 1 "yes" ;
label define BSHOPNONE 0 "no" 1 "yes" ;
label define ISHOPGRDL 0 "no" 1 "yes" ;
label define ISHOPMODR 0 "no" 1 "yes" ;
label define ISHOPNONE 0 "no" 1 "yes" ;
label define BCALL     0 "no" 1 "yes" ;
label define BDONT     0 "no" 1 "yes" ;
label define BFINPLAN  0 "no" 1 "yes" ;
label define BFINPRO   0 "no" 1 "yes" ;
label define BFRIENDWORK 0 "no" 1 "yes" ;
label define BINTERNET 0 "no" 1 "yes" ;
label define BMAGZNEWS 0 "no" 1 "yes" ;
label define BMAILADTV 0 "no" 1 "yes" ;
label define BSELF     0 "no" 1 "yes" ;
label define BOTHER    0 "no" 1 "yes" ;
label define ICALL     0 "no" 1 "yes" ;
label define IDONT     0 "no" 1 "yes" ;
label define IFINPLAN  0 "no" 1 "yes" ;
label define IFINPRO   0 "no" 1 "yes" ;
label define IFRIENDWORK 0 "no" 1 "yes" ;
label define IINTERNET 0 "no" 1 "yes" ;
label define IMAGZNEWS 0 "no" 1 "yes" ;
label define IMAILADTV 0 "no" 1 "yes" ;
label define ISELF     0 "no" 1 "yes" ;
label define IOTHER    0 "no" 1 "yes" ;
label define INTERNET  0 "no" 1 "yes" ;
label define YESFINRISK 0 "No" 1 "Yes" ;
label define NOFINRISK 0 "No" 1 "Yes" ;
label define CKCONNECTN 0 "no" 1 "yes" ;
label define CKCONVPAYRL 0 "no" 1 "yes" ;
label define CKLOCATION 0 "no" 1 "yes" ;
label define CKLONGTIME 0 "no" 1 "yes" ;
label define CKLOWFEEBAL 0 "no" 1 "yes" ;
label define CKMANYSVCS 0 "no" 1 "yes" ;
label define CKPERSONAL 0 "no" 1 "yes" ;
label define CKRECOMFRND 0 "no" 1 "yes" ;
label define CKSAFETY  0 "no" 1 "yes" ;
label define CKOTHCHOOSE 0 "no" 1 "yes" ;
label define MINBAL    0 "no" 1 "yes" ;
label define CANTMANG  0 "no" 1 "yes" ;
label define CREDIT    0 "no" 1 "yes" ;
label define DONTLIKE  0 "no" 1 "yes" ;
label define DONTWANT  0 "no" 1 "yes" ;
label define DONTWRIT  0 "no" 1 "yes" ;
label define NOMONEY   0 "no" 1 "yes" ;
label define SVCCHG    0 "no" 1 "yes" ;
label define OTHER     0 "no" 1 "yes" ;
label define WHYNOCKG  -7 "other" 
                       -1 "can't manage/balance a checking account" 0 "Inap" 
                       1 "don't write enough checks to make it worthwhile" 
                       2 "minimum balance is too high" 
                       3 "do not like dealing with banks" 
                       4 "service charges are too high" 
                       5 "no bank has convenient hours or location" 
                       12 "checkbook has been/could be lost/stolen" 
                       13 "haven't gotten around to it" 
                       14 "R has alternative source of checking services (money market, savings account,etc). Does not include individuals who write checks for R" 
                       15 "R not allowed to have account (e.g., asset test for welfare)" 
                       16 "Someone else writes checks for R or manages R's finances" 
                       20 "R does not need/want a checking account (NEC)" 
                       21 "credit problems, bankruptcy, R does not meet depositorys qualifications for having an account" 
                       22 "Concern about overdraft fees" 
                       95 "don't have(enough) money" 
                       96 "can't manage/balance a checking account" ;
label define SAVRES1   0 "no" 1 "yes" ;
label define SAVRES2   0 "no" 1 "yes" ;
label define SAVRES3   0 "no" 1 "yes" ;
label define SAVRES4   0 "no" 1 "yes" ;
label define SAVRES5   0 "no" 1 "yes" ;
label define SAVRES6   0 "no" 1 "yes" ;
label define SAVRES7   0 "no" 1 "yes" ;
label define SAVRES8   0 "no" 1 "yes" ;
label define SAVRES9   0 "no" 1 "yes" ;
label define SAVED     0 "Household did not save" 1 "Household saved" ;
label define WSAVED    1 "spending exceeded income" 
                       2 "spending equaled income" 
                       3 "spending less than income" ;
label define SPENDMOR  1 "agree strongly" 2 "agree somewhat" 
                       3 "neither agree nor disagree" 4 "disagree somewhat" 
                       5 "disagree strongly" ;
label define SPENDLESS 1 "agree strongly" 2 "agree somewhat" 
                       3 "neither agree nor disagree" 4 "disagree somewhat" 
                       5 "disagree strongly" ;
label define EHCHKG    0 "N/A (has checking account)" 1 "yes" 5 "no" ;
label define INCCAT    1 "0-20" 2 "20-39.9" 3 "40-59.9" 4 "60-79.9" 
                       5 "80-89.9" 6 "90-100" ;
label define INCPCTLECAT 1 "0-9.9" 2 "10-19.9" 3 "20-29.9" 4 "30-39.9" 
                       5 "40-49.9" 6 "50-59.9" 7 "60-69.9" 8 "70-79.9" 
                       9 "80-89.9" 10 "90-94.9" 11 "95-98.9" 12 "99-100" ;
label define INCQRTCAT 1 "0-24.9" 2 "25-49.9" 3 "50-74.9" 4 "75-100" ;
label define NINCCAT   1 "0-20" 2 "20-39.9" 3 "40-59.9" 4 "60-79.9" 
                       5 "80-89.9" 6 "90-100" ;
label define NINCPCTLECAT 1 "0-9.9" 2 "10-19.9" 3 "20-29.9" 4 "30-39.9" 
                       5 "40-49.9" 6 "50-59.9" 7 "60-69.9" 8 "70-79.9" 
                       9 "80-89.9" 10 "90-94.9" 11 "95-98.9" 12 "99-100" ;
label define NINCQRTCAT 1 "0-24.9" 2 "25-49.9" 3 "50-74.9" 4 "75-100" ;
label define NWCAT     1 "0-24.9" 2 "25-49.9" 3 "50-74.9" 4 "75-89.9" 
                       5 "90-100" ;
label define NWPCTLECAT 1 "0-9.9" 2 "10-19.9" 3 "20-29.9" 4 "30-39.9" 
                       5 "40-49.9" 6 "50-59.9" 7 "60-69.9" 8 "70-79.9" 
                       9 "80-89.9" 10 "90-94.9" 11 "95-98.9" 12 "99-100" ;
label define EMERGBORR 0 "No" 1 "Yes" ;
label define HBORRFF   0 "No" 1 "Yes" ;
label define HBORRCC   0 "No" 1 "Yes" ;
label define HBORRALT  0 "No" 1 "Yes" ;
label define HBORRFIN  0 "No" 1 "Yes" ;
label define EMERGSAV  0 "No" 1 "Yes" ;
label define HSAVFIN   0 "No" 1 "Yes" ;
label define HSAVNFIN  0 "No" 1 "Yes" ;
label define EMERGPSTP 0 "No" 1 "Yes" ;
label define HPSTPPAY  0 "No" 1 "Yes" ;
label define HPSTPLN   0 "No" 1 "Yes" ;
label define HPSTPOTH  0 "No" 1 "Yes" ;
label define EMERGCUT  0 "No" 1 "Yes" ;
label define HCUTFOOD  0 "No" 1 "Yes" ;
label define HCUTENT   0 "No" 1 "Yes" ;
label define HCUTOTH   0 "No" 1 "Yes" ;

/********************************
READ IN RAW DATA USING DICTIONARY
*********************************/
infile using "${data_directory}v01_berkeley_scf_dictionary.dct" ;
describe ;
*codebook ;

/************************
SAVE DATA IN STATA FORMAT
*************************/
save "${data_directory}v02_berkeley_scf.dta", replace ;

log close ;
