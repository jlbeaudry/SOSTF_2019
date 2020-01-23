# Our private repo for the 2019 version of the Swinburne Open Science Survey

## Explanation of key files  

Steps to work with data: 

1. Use '1_read_data.R' to convert the Qualtrics output 'OS_Data_ID_Legacy.csv' into 
a data file that is ready to be cleaned. You should only need to do this once--I 
don't need to play with this code.

2. Use '2_clean_data.R' to convert the '.csv' file from Step 1 into a clean data
file. This script will evolve as I continue to manipulate the data. You should run
the updated script any time you work with the data. As I play with this, I will 
update the code in the 'open_science_survey_2019.Rmd' file to ensure that everything
runs on the new 'clean_data.csv' dataset. 

3. The RMarkdown file 'open_science_survey_2019.Rmd' is the working document with all
figures. 

## Info of additional files

These additional files might be useful, but aren't key to getting things done.

1. 'OS_Data_Metadata_tbc.xlsx' ultimately this file will be completed with the details
of each of the variables with labels, etc. This is a work in progress.

2. 'OS_questions.csv' contains all of the questions from Qualtrics.  

3. Files needed to use zotero within RMarkdown: 'apa-old-doi-previx.csi' and 
'Beaudry_Library.bib'. These need to be in the R folder, but should not be touched.

