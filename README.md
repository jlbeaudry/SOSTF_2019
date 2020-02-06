# The Swinburne Open Science Survey (2019)

## Explanation of key files  

Steps to work with data: 

1. The original data ('OS_Data_ID_Legacy.csv') is in the 'survey/data' foler, which 
is not loaded on GitHub, but is available from JB. You will need to run the 
'preprocessing.R' script to clean the data & code it. This script will then write
a .csv file to the main 'data' folder for use with the .Rmd script. 

WHILE WE ARE IN DEVELOPMENT MODE, YOU SHOULD RUN THIS PREPROCESSING SCRIPT EVERYTIME
YOU WORK WITH THE DATA. 

2. The RMarkdown file 'open_science_survey_2019.Rmd' is in the 'analysis' folder. 

## Info of additional files

1. Files needed to use zotero within RMarkdown: 'apa-old-doi-previx.csi' and 
'Beaudry_Library.bib'. These need to be in the R folder, but should not be touched.

FYI: I am trying to use the file structure recommended by https://github.com/djnavarro/newproject

