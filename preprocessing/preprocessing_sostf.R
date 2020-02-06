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


################### WRITE DATA TO CSV #############

# when done recoding, write the data to a new file
# row.names gets rid of the first column from the dataframe.

write.csv(df, here::here("data", "data_sostf.csv"), row.names = FALSE)





