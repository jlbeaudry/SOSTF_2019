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

####### READ IN DATA FILES ############

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
metadata <- here::here("survey", "data", "os_metadata_raw_data.csv") %>% 
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
             LocationAccuracy, PreregDef, CodeDef, OpenDataDef, PrePubDef, OAdef,
             Thanks)) 


##### RECODE VARIABLE NAMES #####

# recode variable labels according to metadata

df <- meta_rename(df, metadata, old = OldVariable, new = NewVariable)

################# RECODING VARIABLES ##################

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
                       ifelse (FORcode_2_num %in% c('13','16','19','20'), "Arts, Soc Sciences, & Humanities",
                       ifelse (FORcode_2_num %in% '2', "Physical Sciences", 
                       ifelse (FORcode_2_num %in% c('1','3','5','6'), "Math, Chem, Enviro, & Bio Sciences", 
                       ifelse (FORcode_2_num %in% c('8','10'), "Tech & Comp Sciences", 
                       ifelse (FORcode_2_num %in% '9', "Engineering", 
                       ifelse (FORcode_2_num %in% '11', "Medical & Health Sciences", 
                       ifelse (FORcode_2_num %in% '17', "Psyc & Cog Sciences",
                       ifelse (FORcode_2_num %in% c('12', '99'), "Other", "Not Specified"))))))))))

# need to add leading zeros for the FOR 2-digit code
df$FORcode_2_num_0 <- str_pad(df$FORcode_2_num, 2, pad = "0")

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

df$OverallExp_num <- factor(df$OverallExp) 

df$PreRegImp_num_o <- factor(df$PreRegImp) #name as 'original' so we can reverse code it next

df$PreRegExp_num <- factor(df$PreRegExp) 

df$PreRegHigh <- factor(df$PreRegHigh) 

df$CodeImp_num_o <- factor(df$CodeImp) #name as 'original' so we can reverse code it next

df$CodeExp_num <- factor(df$CodeExp) 

df$DataExp_num <- factor(df$DataExp)

df$DataImp_num_o <- factor(df$DataImp) #name as 'original' so we can reverse code it next

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

df$PreRegHigh <- factor(df$PreRegHigh) %>% 
  mapvalues(
    c("1", "2", "3", "4", "5", "6"),
    c("I have preregistered a study, but have not yet analysed the data", 
    "I have preregistered a study and am currently writing up the manuscript", 
    "I have preregistered a study and submitted it for publication",
    "I have published one preregistered study",
    "I have published 2 or more preregistered studies", 
    "Other")
  )


df$PreRegConcerns <- factor(df$PreRegConcerns) %>% 
  mapvalues (
    c("0", "1"),
    c("No", "Yes")
  )


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

df$CodeConcerns <- factor(df$CodeConcerns) %>% 
  mapvalues (
    c("0", "1"),
    c("No", "Yes")
  )

df$DataConcerns <- factor(df$DataConcerns) %>% 
  mapvalues (
    c("0", "1"),
    c("No", "Yes")
  )

df$PrePubConcerns <- factor(df$PrePubConcerns) %>% 
  mapvalues (
    c("1", "2"),
    c("No", "Yes")
  )

df$OSBarriers <- factor(df$OSBarriers) %>% 
  mapvalues (
    c("0", "1"),
    c("No", "Yes")
  )

df$Funded <- factor(df$Funded) %>% 
  mapvalues (
    c("0", "1"),
    c("No", "Yes")
  )

df$FundKnow <- factor(df$FundKnow) %>% 
  mapvalues (
    c("0", "1"),
    c("No", "Yes")
  )

# recode the self_rep questions (I'm sure there's a faster way, but this works)

df$self_rep_own <- df$self_rep_own %>% 
  mapvalues(
    c("0", "1", "2"),
    c("No", "Yes", "I can't remember")
  )

df$self_rep_other <- df$self_rep_other %>% 
  mapvalues(
    c("0", "1", "2"),
    c("No", "Yes", "I can't remember")
  )

df$self_rep_pubOtherSuccess <- df$self_rep_pubOtherSuccess %>% 
  mapvalues(
    c("0", "1", "2"),
    c("No", "Yes", "I can't remember")
  )

df$self_rep_pubOtherFail <- df$self_rep_noPubFail %>% 
  mapvalues(
    c("0", "1", "2"),
    c("No", "Yes", "I can't remember")
  )

df$self_rep_noPubSuccess <- df$self_rep_noPubSuccess %>% 
  mapvalues(
    c("0", "1", "2"),
    c("No", "Yes", "I can't remember")
  )

df$self_rep_noPubFail <- df$self_rep_noPubFail %>% 
  mapvalues(
    c("0", "1", "2"),
    c("No", "Yes", "I can't remember")
  )

# recode the OSTools questions (I'm sure there's a faster way, but this works)

df$OSTools_osf <- df$OSTools_osf %>% 
  mapvalues(
    c("0", "1", "2"),
    c("I'm unaware of this", "I use this", "I'm aware of it, but don't use")
  )

df$OSTools_github <- df$OSTools_github %>% 
  mapvalues(
    c("0", "1", "2"),
    c("I'm unaware of this", "I use this", "I'm aware of it, but don't use")
  )

df$OSTools_ardc <- df$OSTools_ardc %>% 
  mapvalues(
    c("0", "1", "2"),
    c("I'm unaware of this", "I use this", "I'm aware of it, but don't use")
  )

df$OSTools_csiro <- df$OSTools_csiro %>% 
  mapvalues(
    c("0", "1", "2"),
    c("I'm unaware of this", "I use this", "I'm aware of it, but don't use")
  )

df$OSTools_cloudstor <- df$OSTools_cloudstor %>% 
  mapvalues(
    c("0", "1", "2"),
    c("I'm unaware of this", "I use this", "I'm aware of it, but don't use")
  )

df$OSTools_embl <- df$OSTools_embl %>% 
  mapvalues(
    c("0", "1", "2"),
    c("I'm unaware of this", "I use this", "I'm aware of it, but don't use")
  )

df$OSTools_figshare <- df$OSTools_figshare %>% 
  mapvalues(
    c("0", "1", "2"),
    c("I'm unaware of this", "I use this", "I'm aware of it, but don't use")
  )

df$OSTools_code <- df$OSTools_code %>% 
  mapvalues(
    c("0", "1", "2"),
    c("I'm unaware of this", "I use this", "I'm aware of it, but don't use")
  )

df$OSTools_datacite <- df$OSTools_datacite %>% 
  mapvalues(
    c("0", "1", "2"),
    c("I'm unaware of this", "I use this", "I'm aware of it, but don't use")
  )

df$OSTools_toolbox <- df$OSTools_toolbox %>% 
  mapvalues(
    c("0", "1", "2"),
    c("I'm unaware of this", "I use this", "I'm aware of it, but don't use")
  )

df$OSTools_orcid <- df$OSTools_orcid %>% 
  mapvalues(
    c("0", "1", "2"),
    c("I'm unaware of this", "I use this", "I'm aware of it, but don't use")
  )

df$OSTools_FAIR <- df$OSTools_FAIR %>% 
  mapvalues(
    c("0", "1", "2"),
    c("I'm unaware of this", "I use this", "I'm aware of it, but don't use")
  )

df$OSTools_instRepo <- df$OSTools_instRepo %>% 
  mapvalues(
    c("0", "1", "2"),
    c("I'm unaware of this", "I use this", "I'm aware of it, but don't use")
  )

df$LastStudy <- factor(df$LastStudy) %>% 
  mapvalues(
    c ("1", "2", "3", "4", "5", "6", "7"),
    c ("All info needed is in publication", 
    "All info needed is openly available",
    "Need info from research team", 
    "Not possible because of context", 
    "Not possible because of other reasons",
    "I do not conduct research studies",
    "Other"))
    
    
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
  mutate(AcLevel_pd = replace(AcLevel_pd, ParticipantNumber == "1065", 1)) %>%  #1065 said 'post doc'
  mutate(AcLevel_prof = replace(AcLevel_prof, ParticipantNumber == "1028", 1)) #1028 said 'emeritus professor'

# Recoding Academic Levels. 
  # the coding of the academic levels is not exactly as in the pdf of the survey
  # due to a coding error in Qualtrics. The second row of the 'AcLevel' columns in the 
  # 'OS_Data_ID_Legacy.csv' filehas the correct  labels. R
  # Recoding the numerical responses into text responses here, 
  # based on their first response within the survey to resolve the multiple responses.
  
  
df <- mutate(df, AcademicLevel = case_when (AcLevel_prof == '1' ~ "Professor", 
                                             AcLevel_ap == '1' ~ "Associate Professor", 
                                             AcLevel_sl == '1' ~ "Senior Lecturer",
                                             AcLevel_l == '1' ~ "Lecturer", 
                                             AcLevel_pd == '1' ~ "Postdoc",
                                             AcLevel_phd == '1' ~ "PhD Student", 
                                             AcLevel_mast == '1' ~ "Masters Student",
                                             AcLevel_ra == '1' ~ "Research Assistant",
                                             AcLevel_other == '1' ~ "Other", 
                                             AcLevel_srf == '1' ~ "Senior Research Fellow",
                                             AcLevel_rf == '1' ~ "Research Fellow",
                                             TRUE ~ "NA"))
  
  # transform into factor
  df$AcademicLevel <- factor(df$AcademicLevel) 
  
###### DELETE THE VARIABLES THAT HAVE BEEN RECODED OR ARE SUPERFLUOUS ######
  
df <- df %>%
  select (-c (OverallExp_num, 
              PreRegImp_num, PreRegImp_num_o,
              PreRegExp_num, 
              CodeImp_num, CodeImp_num_o,
              CodeExp_num,
              DataImp_num, DataImp_num_o, 
              DataExp_num, 
              PrePubImp_num, PrePubImp_num_o, 
              PrePubExp_num,
              OAprop_num, OAprop_num_o,
              crisis_num)) %>% 
    select(-contains ("AcLevel"))

##### DELETE THE QUAL DATA FOR OCT 2020 OSF POSTING #####
  
  df <- df %>%       
    select(-contains("text"))

  
  
###### REARRANGE THE VARIABLES IN THE TIBBLE TO ALIGN WITH SURVEY ORDER ######

df <- df %>% 
    relocate (OverallExp, .after = ParticipantNumber) %>% 
    relocate (PreRegImp, .after = OverallExp) %>% 
    relocate(PreRegExp, .after = PreRegImp) 
    
  
  
################### WRITE DATA TO CSV #############

# when done recoding, write the data to a new file
# row.names gets rid of the first column from the dataframe.

write.csv(df, here::here("data", "data_processed.csv"), row.names = FALSE)





  