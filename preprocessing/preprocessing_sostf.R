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
    c("Unaware", "Aware, But Not Used", "Some Experience", "Extensive Experience")
  )

df$PreregExp1 <- df$PreregExp1 %>% 
  mapvalues(
    c("1", "2", "3", "4"), 
    c("Unaware", "Aware, But Not Used", "Some Experience", "Regularly Preregister")
  )

df$CodeExp <- df$CodeExp %>%
  mapvalues(
    c("1", "2", "3", "4"),
    c("Unaware", "Aware, But Not Used", "Some Use", "Regular Use")
  )


# check how many options were selected in the AcLevel columns.

  AcLevel <- select (df, AcLevel_1:AcLevel_13) 
  df$Multi_AcLevel <- rowSums (AcLevel == "1", na.rm = TRUE) #create new column with 
    # values across the Academic Level columns
  # select(df, df$Multi_AcLevel = 2) 
  # next step is to resolve the conflicts, but I've not figured that out yet. 




# the coding of the academic levels are not exactly as in the pdf of the survey
  # in the current version of the survey due to a coding error in Qualtrics. Check
  # the second row of the 'AcLevel' columns to see the actual labels. Recoding those
  # here into a single variable.

df$AcLevel_1 <- recode (df$AcLevel_1, '1' = "Prof")
df$AcLevel_2 <- recode (df$AcLevel_2, '1' = "Ass_Prof")
df$AcLevel_3 <- recode (df$AcLevel_3, '1' ="Sen_Lec")
df$AcLevel_4 <- recode (df$AcLevel_4, '1' = "Lec")
df$AcLevel_5 <- recode (df$AcLevel_5, '1' = "Postdoc")
df$AcLevel_6 <- recode (df$AcLevel_6, '1' = "PhD_student")
df$AcLevel_7 <- recode (df$AcLevel_7, '1' = "Masters_student")
df$AcLevel_9 <- recode (df$AcLevel_9, '1' = "RA")
df$AcLevel_10<- recode (df$AcLevel_10, '1' = "Other")
df$AcLevel_12<- recode (df$AcLevel_12, '1' = "Senior_Res_Fellow")
df$AcLevel_13<- recode (df$AcLevel_13, '1' = "Res_Fellow")


################### WRITE DATA TO CSV #############

# when done recoding, write the data to a new file
# row.names gets rid of the first column from the dataframe.

write.csv(df, here::here("data", "data_sostf.csv"), row.names = FALSE)





