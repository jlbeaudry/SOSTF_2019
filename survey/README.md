This folder contains the raw data from the survey as well as a draft of the metadata information 
and survey questions. 

We discovered that the pdf of the survey and the survey participants completed had slightly 
different coding for some of the multiple-choice variables (argh). As such, we need to obtain 
the accurate values from the ID_Legacy data (refer to the header to see how responses were 
coded). For example, for OAfees, the response option "I paid the fees with my personal money" 
was actually coded in the data as "6" rather than "7" (as indicated in the pdf of the survey). 

To rectify this issue and prevent any confusion, we will: 
1. include the correct coding information in the metadata files.
2. edit the pdf of the survey to include the correct coding information.

The raw data file should NOT be edited. The 'preprocessing.R' scrip in the 'preprocessing' folder 
will read in the data, process it, and then export the data file to the main 'data' folder.