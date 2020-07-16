library(here)
library(plyr) 
library(tidyverse)
library(tools)

####### FUNCTIONS ############
#FROM MATHEW LING'S MISINFORMATION PACKAGE ON GITHUB

# modify Mathew Ling's 'read_qualtrics' function to remove 'janitor_names'
read_qualtrics <- function(file, legacy = TRUE) {
  a <- readr::read_csv(file)
  if (legacy == FALSE) {
    a <- a[3:nrow(a), ]
  } else {
    a <- a[2:nrow(a), ]
  }
  a %>% readr::type_convert(trim_ws = TRUE)
}

# Mathew Ling's 'meta_rename' function 

meta_rename <-  function(df, metadata, old, new) {
  
  keys   <- metadata[[deparse(substitute(old))]]
  values <- metadata[[deparse(substitute(new))]]
  rename_at(df, vars(keys), ~ values)
}

####### READ IN DATA FILE ############

# laod data with the 'read_qualtrics' function

# df is the main dataset, but it doesn't have the FOR code variables in a usable format
df <- here::here("survey", "data", "OS_Data_ID_Legacy.csv") %>% 
  read_qualtrics() %>% 
  select (-FORcode_1, -FORcode_2) #remove the columns that don't make sense for FOR code

# need df_for from the 'not legacy' dataset for FOR codes
df_for <- here::here("survey", "data", "OS_Data_ID_Not_Legacy.csv") %>% 
  read_qualtrics (legacy = FALSE) %>% 
  select (ParticipantNumber, FORcode_1, FORcode_2) #keep only variables we need

# join the two sets by ParticipantNumber

df <- df %>% left_join(df_for, by = "ParticipantNumber")

# load in metadata for concern variables
metadata <- here::here("survey", "data", "os_metadata_good.csv") %>% 
  read_csv(col_names = TRUE, skip_empty_rows = TRUE) %>% 
  filter(!is.na(OldVariable))

############# CLEANING THE DATA #####################

# delete my test data (participant id = 1062) 

df <- df %>% 
  filter(ParticipantNumber != "1062")

# delete respondent who indicated that they were 'professional staff-manager' as academic level
df <- df %>% 
  filter(ParticipantNumber != "1103")

# select all columns except for the junk columns, which we don't need 
  # (e.g., V1, location, etc.).

df <- df %>% 
  select (-c(V1:V10, consent, Intro, LocationLatitude, LocationLongitude, 
             LocationAccuracy, PreregDef)) 


################# RECODING VARIABLES ##################

# rename wonky variable names & change FOR code columns to reflect number of digits. 

df <- df %>% 
  dplyr::rename(RepEstimate = RepEstimate_1, FORcode_4 = FORcode_2, FORcode_2 = FORcode_1) 


# separate FOR code columns into numbers and labels & change case

df <- df %>% 
  separate (FORcode_2, into = c("FORcode_2_num", "FORcode_2_label"), sep = "-", convert = TRUE) %>% 
  separate (FORcode_4, into = c("FORcode_4_num", "FORcode_4_label"), sep = "-", convert = TRUE)

# one participant (1165) selected 'other' for the FOR code. 'Other' recoded in 
  # 'num' columns, but not in 'label' columns. We need 'num' column to be numerical, 
  # so recode that "Other" as 99 in the '_num' columns and as 'other' in the '_label' columns.

df <- df %>% 
  mutate(FORcode_2_num = replace(FORcode_2_num, ParticipantNumber == "1165", "99")) %>% 
  mutate(FORcode_2_label = replace(FORcode_2_label, ParticipantNumber == "1165", "Other")) %>% 
  mutate(FORcode_4_num = replace(FORcode_4_num, ParticipantNumber == "1165", "99")) %>% 
  mutate(FORcode_4_label = replace(FORcode_4_label, ParticipantNumber == "1165", "Other")) 

# relabel "NA" to "Not Specified"
df <- df %>% 
  mutate(FORcode_2_label = replace(FORcode_2_label, is.na(FORcode_2_label), "Not Specified"))

#change capitalisation of FOR labels
df$FORcode_2_label <- toTitleCase(tolower(df$FORcode_2_label)) 
# using 'to lower' to make the all caps lower case, and then using title case to 
  # capitalise the words
# I'm using this rather than 'str_to_title' because it doesn't capitalise "and"
              
# recode FOR codes into grouped disciplines

  # first, convert from character to number
df$FORcode_2_num <- as.numeric(df$FORcode_2_num)
df$FORcode_4_num <- as.numeric(df$FORcode_4_num)

  # then recode according to discipline (can't use `case_when` because the RHS
  # has more values than the LHS)

df <- df %>% 
  mutate (discipline = ifelse (FORcode_2_num %in% c('14','15','18'), "Business & Law",
                       ifelse (FORcode_2_num %in% c('13','16','19','20'), "ASSH",
                       ifelse (FORcode_2_num %in% '2', "Physical Sciences", 
                       ifelse (FORcode_2_num %in% c('1','3','5','6'), "Math, Chem, Enviro, & Bio Sciences", 
                       ifelse (FORcode_2_num %in% c('8','10'), "Tech & Comp Sciences", 
                       ifelse (FORcode_2_num %in% '9', "Engineering", 
                       ifelse (FORcode_2_num %in% '11', "Medical & Health Sciences", 
                       ifelse (FORcode_2_num %in% '17', "Psyc & Cog Sciences",
                       ifelse (FORcode_2_num %in% c('12', '99'), "Other", "Not Specified"))))))))))


# View the mapping from FOR to discipline & see the number of folks in each FOR 
  # code group (and then ungroup again)

df %>% 
  group_by(FORcode_2_num,FORcode_2_label,discipline) %>% 
  dplyr::summarise(n = n()) %>% 
  ungroup(FORcode_2_num)


# MAKE A DECISION AS TO HOW TO PROCEED WITH RELABELLING THE WONKY VALUES...(e.g., 
  # recode AcLevel_x to have the numbers that match the pdf of the survey.
  # If we re-run the survey, we should not have to do this again, so keep the code
  # separate from the rest of it so it's obvious. Why won't we need it? Because 
  # the data should match what we actually programmed.

## CONVERT VARIABLES INTO FACTORS ###

# the original variables will need to be factors too to use them in the figures.
# because our scripts run the scripts with the labels, I'm going to create new
# variables for these with '_num', so I don't have to redo everything in our .Rmd file.
  # We won't often need to use these variables, so I think this will work.

df$crisis_num <- factor(df$crisis) 

df$OverallExp_num <- factor(df$OverallExperience) 

df$PreRegImp_num_o <- factor(df$PreRegImp) #name as 'original' so we can reverse code it next

df$PreRegExp_num <- factor(df$PreregExp1) 

df$CodeImp_num_o <- factor(df$CodeImp) #name as 'original' so we can reverse code it next

df$CodeExp_num <- factor(df$CodeExp) 

df$DataExp_num <- factor(df$OpenDataExp)

df$DataImp_num_o <- factor(df$OpenDataImp) #name as 'original' so we can reverse code it next

df$PrePubImp_num_o <- factor(df$PrePubImp) #name as 'original' so we can reverse code it next

df$PrePubExp_num <- factor(df$PrePubExp) 

df$OAprop_num_o <- factor(df$OAprop) #name as 'original' so we can reverse code it next

# reverse code `importance` variables so lower values = less importance

df$PreRegImp_num <- df$PreRegImp_num_o %>% 
  mapvalues(
    c("1", "2", "3", "4"), 
    c("4", "3", "2", "1"))

df$CodeImp_num <- df$CodeImp_num_o %>% 
  mapvalues(
    c("1", "2", "3", "4"), 
    c("4", "3", "2", "1"))

df$DataImp_num <- df$DataImp_num_o %>% 
  mapvalues(
    c("1", "2", "3", "4"), 
    c("4", "3", "2", "1"))

df$PrePubImp_num <- df$PrePubImp_num_o %>% 
  mapvalues(
    c("1", "2", "3", "4"), 
    c("4", "3", "2", "1"))

# reverse code 'proportion' variables so lower values = less proportion

df$OAprop_num <- df$OAprop_num_o %>% 
  mapvalues(
    c("1", "2", "3", "4", "0", "5"), 
    c("4", "3", "2", "1", "0", "5"))

# recode Concerns variables (for Preregistration, Code, Data, Pre-publication 
  # archiving) and Use variables (for Code, Data, Pre-publication archiving)

df <- meta_rename(df, metadata, old = OldVariable, new = NewVariable)

# relabel number with text labels for levels

df$crisis <- df$crisis_num %>% 
  mapvalues(
    c("1", "2", "3", "4"), 
    c("Significant Crisis", "Slight Crisis", "No Crisis", "Don't Know"))

df$OverallExp <- df$OverallExp_num %>% 
  mapvalues(
    c("1", "2", "3", "4"), 
    c("Unaware", "Aware, But Not Used", "Some", "Extensive"))

df$PreRegImp <- df$PreRegImp_num %>% 
  mapvalues(
    c ("0", "4", "3", "2", "1"),
    c ("Researchers in my discipline do not conduct research studies", 
       "Extremely important", "Somewhat important", "Somewhat unimportant", 
       "Not at all"))

df$PreRegExp <- df$PreRegExp_num %>% 
  mapvalues(
    c("1", "2", "3", "4"), 
    c("Unaware", "Aware, But Not Used", "Some Experience", "Reg Use"))

df$CodeImp <- df$CodeImp_num %>% 
  mapvalues(
    c ("0", "4", "3", "2", "1"),
    c ("Researchers in my discipline do not use materials and/or code", 
       "Extremely important", "Somewhat important", "Somewhat unimportant", 
       "Not at all"))

df$CodeExp <- df$CodeExp_num %>%
  mapvalues(
    c("1", "2", "3", "4"),
    c("Unaware", "Aware, But Not Used", "Some Use", "Regular Use"))

df$DataImp <- df$DataImp_num %>% 
  mapvalues(
    c ("0", "4", "3", "2", "1"),
    c ("Research publications in my field are not based on data", 
       "Extremely important", "Somewhat important", "Somewhat unimportant", 
       "Not at all"))

df$DataExp <- df$DataExp_num %>%
  mapvalues(
    c("1", "2", "3", "4"),
    c("Unaware", "Aware, But Not Used", "Some Use", "Regular Use"))

df$PrePubImp <- df$PrePubImp_num %>% 
  mapvalues(
    c ("4", "3", "2", "1"),
    c ("Extremely important", "Somewhat important", "Somewhat unimportant", 
       "Not at all"))

df$PrePubExp <- df$PrePubExp_num %>% 
  mapvalues(
    c("1", "2", "3", "4"), 
    c("Unaware", "Aware, But Not Used", "Some Experience", "Extensive Experience"))

df$OAprop <- df$OAprop_num %>% 
  mapvalues(
    c("4", "3", "2", "1", "0", "5"), 
    c("All", "Most", "Half", "Some", "None", "I don't know"))

## RECODE ACADEMIC LEVELS ##

# notes re: how I dealt with folks who selected more than one option from Academic Levels.
  # Keeping for transparency and reproducibility, but no need to run everytime. 

  #AcLevel <- select (df, AcLevel_1:AcLevel_13) 
  #df$Multi_AcLevel <- rowSums (AcLevel == "1", na.rm = TRUE) #create new column with 
    # values across the Academic Level columns
  # filter(df, Multi_AcLevel >= 2) # shows which respondents (n = 11) gave more than 
    # one response to question of Academic Level. Will resolve this in these 11 cases 
    # by applying a data cleansing rule that follows this logic:
    # Academic Position > Student Position > Research Assistantship
    # For these 11 cases, we will just take the first response, because that 
    # coding corresponds to the rule above. 
  # if we need to see those with multiple responses again, use this code
  # df2 <- filter(df, Multi_AcLevel >= 2) %>% 
    #select (ParticipantNumber, AcLevel_1:AcLevel_13) 

# deal with the 'other' responses that align with the actual response options.

df <- df %>% 
  mutate(AcLevel_5 = replace(AcLevel_5, ParticipantNumber == "1065", 1)) %>%  #1065 said 'post doc'
  mutate(AcLevel_1 = replace(AcLevel_1, ParticipantNumber == "1028", 1)) #1028 said 'emeritus professor'

# Recoding Academic Levels. 
  # the coding of the academic levels is not exactly as in the pdf of the survey
  # due to a coding error in Qualtrics. The second row of the 'AcLevel' columns in the 
  # 'OS_Data_ID_Legacy.csv' filehas the correct  labels. R
  # Recoding the numerical responses into text responses here, 
  # based on their first response within the survey to resolve the multiple responses.
  
  
df <- mutate(df, AcLevel_Label = case_when (AcLevel_1 == '1' ~ "Professor", 
                                             AcLevel_2 == '1' ~ "Associate Professor", 
                                             AcLevel_3 == '1' ~ "Senior Lecturer",
                                             AcLevel_4 == '1' ~ "Lecturer", 
                                             AcLevel_5 == '1' ~ "Postdoc",
                                             AcLevel_6 == '1' ~ "PhD Student", 
                                             AcLevel_7 == '1' ~ "Masters Student",
                                             AcLevel_9 == '1' ~ "Research Assistant",
                                             AcLevel_10 == '1' ~ "Other", 
                                             AcLevel_12 == '1' ~ "Senior Research Fellow",
                                             AcLevel_13 == '1' ~ "Research Fellow",
                                             TRUE ~ "NA"))
  
  # transform into factor
  df$AcLevel_Label <- factor(df$AcLevel_Label) 
  
################### WRITE DATA TO CSV #############

# when done recoding, write the data to a new file
# row.names gets rid of the first column from the dataframe.

write.csv(df, here::here("data", "data_sostf.csv"), row.names = FALSE)


# Clean Environment
rm(df)
rm(df_for)
rm(read_qualtrics)
rm(meta_rename)



  