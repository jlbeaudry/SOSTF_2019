# The Swinburne Open Science Survey (2019)

## Explanation of key files  

Steps to work with data: 

1. Use '1_read_data.R' to convert the Qualtrics output 'OS_Data_ID_Legacy.csv' 
(available from JB) into a data file that is ready to be cleaned. You should only need to do this once--I don't need to play with this code.

2. Use '2_clean_data.R' to convert the '.csv' file from Step 1 into a clean data
file. This script will evolve as I continue to manipulate the data. You should run
the updated script any time you work with the data. As I play with this, I will 
update the code in the 'open_science_survey_2019.Rmd' file to ensure that everything runs on the new 'clean_data.csv' dataset. 

2. Use '2_clean_data.R' to convert the '.csv' file from Step 1 into a clean data
file. This file will then be used in step 3 to recode the variables. You should only need to do this once--I don't need to play with this code.

3. Use '3_recode_data.R' to recode the variables from the '.csv' file from Step 2. This script will evolve as I continue to manipulate the data. YOU SHOULD RUN THIS UPDATED SCRIPT ANYTIME YOU WORK WITH THE DATA. 

3. The RMarkdown file 'open_science_survey_2019.Rmd' is the working document with all figures. 

## Info of additional files

1. Files needed to use zotero within RMarkdown: 'apa-old-doi-previx.csi' and 
'Beaudry_Library.bib'. These need to be in the R folder, but should not be touched.

