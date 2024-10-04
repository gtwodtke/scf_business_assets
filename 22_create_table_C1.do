#delimit ;
capture log close ;
capture clear all ;
set more off ;

global data_directory "C:\Users\Geoff\Dropbox\D\projects\scf_biz_assets\data\" ;
global log_directory "C:\Users\Geoff\Dropbox\D\projects\scf_biz_assets\programs\_LOGS\" ;
global temp_directory "C:\Users\Geoff\Dropbox\D\projects\scf_biz_assets\data\_TEMP\" ;

log using "${log_directory}22_create_table_C1.log", replace ;

/*************************************
PROGRAM NAME: 22_create_table_C1.do
PURPOSE: tabulate missing data by year
**************************************/

/***INPUT DATA***/
use "${data_directory}v01_scf_final_merged.dta", clear ; 

/***TABULATE MISSINGNESS IN TOTAL SAMPLE***/
tab nyear ibizval, row ;

/***TABULATE MISSINGNESS AMONG BUSINESS OWNERS***/
tab nyear ibizval if nbizown==1, row ;

log close ;
