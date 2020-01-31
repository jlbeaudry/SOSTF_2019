# Clean the data to use with my '.Rmd' file and analyses

library(tidyverse)
library(here)
library(plyr) #needed to mapvalues for factors; but lets see without...


# import the data file produced by '1_read_data.R'
df <- read_csv(here::here("data", "data_to_clean.csv"))

# start cleaning the data:

# need to delete my test data (participant id = 1062) & assign it to a new object. 
  # should have 240 observations rather than 241. 

df <- df %>% 
  filter(ParticipantNumber != "1062")

# selects all columns except for the junk columns, which we don't need 
  # (e.g., V1, location, etc.).
df <- df %>% 
  select (-V1:-V10, -X1, -consent, -Intro, -LocationLatitude, -LocationLongitude, 
          -LocationAccuracy)

# turns specific variables into factors & includes the labels for the values. Changin
  # this to use mutate instead, so that the labels are clearer.

df$crisis <- factor(df$crisis) %>% 
  mapvalues(
    c("1", "2", "3", "4"), 
    c("Significant Crisis", "Slight Crisis", "No Crisis", "Don't Know")
  )

#turn specific variables into factors (will rename later)
df$crisis <- factor(df$crisis)
df$OverallExperience <- factor(df$OverallExperience)
# ETC....

df %>% 
  mutate(crisis = fct_recode(crisis,
                             "Significant Crisis" = "1", 
                             "Slight Crisis" = "2", 
                             "No Crisis" = "3", 
                             "Don't Know" = "4",
                             "NA" = "NA"
                               )) %>% 
  count(crisis)

df$crisis <- factor(df$crisis) %>% 
  mutate()

# 31.1.20 I will try to turn these into functions if possible and then will probably 
  # save them in a new script that is specifically about recoding data.But, I will
  # play with this on my own branch.



df$OverallExperience <- factor(df$OverallExperience) %>% 
  mapvalues(
    c("1", "2", "3", "4"), 
    c("Unaware", "Aware, But Not Used", "Some Experience", "Extensive Experience")
  )
  
df$CodeExp <- factor(df$CodeExp) %>%
  mapvalues(
    c("1", "2", "3", "4"),
    c("Unaware", "Aware, But Not Used", "Some Use", "Regular Use")
  )

# when done cleaning, write the data to a new file
write.csv(df, here::here("data", "clean_data.csv"))





