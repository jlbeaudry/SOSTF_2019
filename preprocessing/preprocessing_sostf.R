library(tidyverse)
library(here)
library(plyr) 

####### READ IN DATA FILE ############
  
# modify Ling's 'read_qualtrics' function to remove 'janitor_names'
read_qualtrics <- function(file, legacy = TRUE) {
  a <- readr::read_csv(file)
  if (legacy == FALSE) {
    a <- a[3:nrow(a), ]
  } else {
    a <- a[2:nrow(a), ]
  }
  a %>% readr::type_convert(trim_ws = TRUE)
}

# pipe my data through the 'read_qualtrics' function

df <- here::here("survey", "data", "OS_Data_ID_Legacy.csv") %>% 
  read_qualtrics()

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

# rename wonky variable name

df <- df %>% 
  dplyr::rename(RepEstimate = RepEstimate_1)


################# RECODING VARIABLES ##################

# convert variables into factors

# the original variables will need to be factors too to use them in the figures.
# because our scripts run the scripts with the labels, I'm going to create new
# variables for these with '_num', so I don't have to redo everything in our .Rmd file.
  # We won't often need to use these variables, so I think this will work.

df$crisis_num <- factor(df$crisis) 

df$OverallExp_num <- factor(df$OverallExperience) 

df$PreregExp1_num <- factor(df$PreregExp1) 

df$CodeExp_num <- factor(df$CodeExp) 

# create new variables with text labels for levels

df$crisis <- df$crisis %>% 
  mapvalues(
    c("1", "2", "3", "4"), 
    c("Significant Crisis", "Slight Crisis", "No Crisis", "Don't Know")
  )

df$OverallExp <- df$OverallExperience %>% 
  mapvalues(
    c("1", "2", "3", "4"), 
    c("Unaware", "Aware, But Not Used", "Some", "Extensive")
  )

df$PreregExp1 <- df$PreregExp1 %>% 
  mapvalues(
    c("1", "2", "3", "4"), 
    c("Unaware", "Aware, But Not Used", "Some Experience", "Reg Use")
  )

df$CodeExp <- df$CodeExp %>%
  mapvalues(
    c("1", "2", "3", "4"),
    c("Unaware", "Aware, But Not Used", "Some Use", "Regular Use")
  )

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
  
  
  df <- mutate (df, AcLevel_Label = ifelse (AcLevel_1 %in% '1', "Professor",
                                    ifelse (AcLevel_2 %in% '1', "Associate Professor", 
                                    ifelse (AcLevel_3 %in% '1', "Senior Lecturer",
                                    ifelse (AcLevel_4 %in% '1', "Lecturer", 
                                    ifelse (AcLevel_5 %in% '1', "Postdoc",
                                    ifelse (AcLevel_6 %in% '1', "PhD Student", 
                                    ifelse (AcLevel_7 %in% '1', "Masters Student",
                                    ifelse (AcLevel_9 %in% '1', "Research Assistant", 
                                    ifelse (AcLevel_10 %in% '1', "Other", 
                                    ifelse (AcLevel_12 %in% '1', "Senior Research Fellow",
                                    ifelse (AcLevel_13 %in% '1', "Research Fellow", "NA"))))))))))))

  
  # transform into factor
  df$AcLevel_Label <- factor(df$AcLevel_Label) 
  

################### WRITE DATA TO CSV #############

# when done recoding, write the data to a new file
# row.names gets rid of the first column from the dataframe.

write.csv(df, here::here("data", "data_sostf.csv"), row.names = FALSE)





