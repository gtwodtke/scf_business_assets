Download data from:  "http://sda.berkeley.edu/sdaweb/analysis/;jsessionid=8D96CCC256DE5AE8AC6901CE280E69FE?dataset=scfcomb" 

Save 'Data File' as a 'text file with no extra blanks' ;
Ensure the 'Codebook' option is checked;
Under 'Data Definitions for:' Select 'STATA' ; 
Under 'Select VARIABLES to include', check all the boxes for every category ; 
Press the '˜Create Files' tab ;
Save the 'Data File' as 'v01_berkeley_scf' into your data directory.

Download 'STATA file' - this is an auto-generated text file which includes variable labels, the labels' definitions, and the data dictionary.
Open the STATA file; Copy the file up until and including "infile using X'; 
Save this copied text as a [.do] file to your programs directory --- 02_create_v02_berkeley_scf.do

Save the remainder of the file - including the line "dictionary using Y {" up until the final close bracket "}" as a [.dct] file; This is the data dictionary.
Ensure that the dictionary file has a solid clear after the final close bracket '}' or you may receive the "unexpect end of file" error. Hit the "return key" after the final '}' to avoid this error.
Save this file into your data directory ~~~ {data_directory}v01_berkeley_scf_dictionary ~~~~

You will now need to change the pathways in the "02_create_v02_berkeley_scf.do" file to your corresponding directory. Open the file up. 
Replace the "X" found in the "infile using X" command with the dictionary pathway you just created ~~~ {data_directory}v01_berkeley_scf_dictionary ~~~~
This is all you have to replace in this [.do] file - save it. 

You will now need to change the pathways in the "{data_directory}v01_berkeley_scf_dictionary" file to your corresponding directory. Open the file in Notepad. 
Replace the "Y" found in the "dictionary using Y {" command with the raw data file you saved earlier ~~~ ${data_directory}v01_berkeley_scf ~~~
This is all you have to replace in this [.dct] file - save it.

Run the "02_create_v02_berkeley_scf.do" program and ensure there are no errors. 

Save the "Codebook" file generated under your documentation directory for future reference. You will no longer require access to the SDA Berkeley extraction tool. 







