capture clear all

global prgmpath "C:\Users\Geoff\Dropbox\D\projects\scf_biz_assets\programs\" 

/*DATA PROCESSING*/
do "${prgmpath}02_create_v02_berkeley_scf.do" nostop

do "${prgmpath}03_create_v03_berkeley_scf.do" nostop

do "${prgmpath}04_create_v01_fed_scf.do" nostop

do "${prgmpath}05_create_v01_fed_bsweights.do" nostop

do "${prgmpath}06_create_v01_scf_final_merged.do" nostop

/*ANALYSES*/
shell "C:\Program Files\R\R-4.4.1\bin\x64\R.exe" CMD BATCH --vanilla --slave --no-restore --no-timing --no-echo "${prgmpath}07_create_figure_1.R"
shell DEL "${prgmpath}07_create_figure_1.Rout"

shell "C:\Program Files\R\R-4.4.1\bin\x64\R.exe" CMD BATCH --vanilla --slave --no-restore --no-timing --no-echo "${prgmpath}08_create_figure_2.R"
shell DEL "${prgmpath}08_create_figure_2.Rout"

shell "C:\Program Files\R\R-4.4.1\bin\x64\R.exe" CMD BATCH --vanilla --slave --no-restore --no-timing --no-echo "${prgmpath}09_create_figure_3.R"
shell DEL "${prgmpath}09_create_figure_3.Rout"

shell "C:\Program Files\R\R-4.4.1\bin\x64\R.exe" CMD BATCH --vanilla --slave --no-restore --no-timing --no-echo "${prgmpath}10_create_figure_4.R"
shell DEL "${prgmpath}10_create_figure_4.Rout"

shell "C:\Program Files\R\R-4.4.1\bin\x64\R.exe" CMD BATCH --vanilla --slave --no-restore --no-timing --no-echo "${prgmpath}11_create_table_1.R"
shell DEL "${prgmpath}11_create_table_1.Rout"

shell "C:\Program Files\R\R-4.4.1\bin\x64\R.exe" CMD BATCH --vanilla --slave --no-restore --no-timing --no-echo "${prgmpath}12_create_table_2.R"
shell DEL "${prgmpath}12_create_table_2.Rout"

shell "C:\Program Files\R\R-4.4.1\bin\x64\R.exe" CMD BATCH --vanilla --slave --no-restore --no-timing --no-echo "${prgmpath}13_create_table_3.R"
shell DEL "${prgmpath}13_create_table_3.Rout"

shell "C:\Program Files\R\R-4.4.1\bin\x64\R.exe" CMD BATCH --vanilla --slave --no-restore --no-timing --no-echo "${prgmpath}14_create_table_4.R"
shell DEL "${prgmpath}14_create_table_4.Rout"

shell "C:\Program Files\R\R-4.4.1\bin\x64\R.exe" CMD BATCH --vanilla --slave --no-restore --no-timing --no-echo "${prgmpath}15_create_table_5.R"
shell DEL "${prgmpath}15_create_table_5.Rout"

shell "C:\Program Files\R\R-4.4.1\bin\x64\R.exe" CMD BATCH --vanilla --slave --no-restore --no-timing --no-echo "${prgmpath}16_create_table_6.R"
shell DEL "${prgmpath}16_create_table_6.Rout"

shell "C:\Program Files\R\R-4.4.1\bin\x64\R.exe" CMD BATCH --vanilla --slave --no-restore --no-timing --no-echo "${prgmpath}17_create_table_7.R"
shell DEL "${prgmpath}17_create_table_7.Rout"

shell "C:\Program Files\R\R-4.4.1\bin\x64\R.exe" CMD BATCH --vanilla --slave --no-restore --no-timing --no-echo "${prgmpath}18_create_table_8.R"
shell DEL "${prgmpath}18_create_table_8.Rout"

/*MISC*/
shell "C:\Program Files\R\R-4.4.1\bin\x64\R.exe" CMD BATCH --vanilla --slave --no-restore --no-timing --no-echo "${prgmpath}19_create_misc_stats.R"
shell DEL "${prgmpath}19_create_misc_stats.Rout"

/*APPENDIX*/
shell "C:\Program Files\R\R-4.4.1\bin\x64\R.exe" CMD BATCH --vanilla --slave --no-restore --no-timing --no-echo "${prgmpath}20_create_figure_A1.R"
shell DEL "${prgmpath}20_create_figure_A1.Rout"

shell "C:\Program Files\R\R-4.4.1\bin\x64\R.exe" CMD BATCH --vanilla --slave --no-restore --no-timing --no-echo "${prgmpath}21_create_figure_B1.R"
shell DEL "${prgmpath}21_create_figure_B1.Rout"

do "${prgmpath}22_create_table_C1.do" nostop

shell "C:\Program Files\R\R-4.4.1\bin\x64\R.exe" CMD BATCH --vanilla --slave --no-restore --no-timing --no-echo "${prgmpath}23_create_table_C2.R"
shell DEL "${prgmpath}23_create_table_C2.Rout"

shell "C:\Program Files\R\R-4.4.1\bin\x64\R.exe" CMD BATCH --vanilla --slave --no-restore --no-timing --no-echo "${prgmpath}24_create_figure_C1.R"
shell DEL "${prgmpath}24_create_figure_C1.Rout"

shell "C:\Program Files\R\R-4.4.1\bin\x64\R.exe" CMD BATCH --vanilla --slave --no-restore --no-timing --no-echo "${prgmpath}25_create_figure_D1.R"
shell DEL "${prgmpath}25_create_figure_D1.Rout"

shell "C:\Program Files\R\R-4.4.1\bin\x64\R.exe" CMD BATCH --vanilla --slave --no-restore --no-timing --no-echo "${prgmpath}26_create_figure_E1.R"
shell DEL "${prgmpath}26_create_figure_E1.Rout"

shell "C:\Program Files\R\R-4.4.1\bin\x64\R.exe" CMD BATCH --vanilla --slave --no-restore --no-timing --no-echo "${prgmpath}27_create_figure_E2.R"
shell DEL "${prgmpath}27_create_figure_E2.Rout"

shell "C:\Program Files\R\R-4.4.1\bin\x64\R.exe" CMD BATCH --vanilla --slave --no-restore --no-timing --no-echo "${prgmpath}28_create_figure_E3.R"
shell DEL "${prgmpath}28_create_figure_E3.Rout"

