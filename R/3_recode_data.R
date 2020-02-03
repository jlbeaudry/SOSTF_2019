# Clean the data to use with my '.Rmd' file and analyses

library(tidyverse)
library(here)
library(plyr) #need to mapvalues for factors

#################### IMPORTING DATA
# import the data file produced by '2_clean_data.R'
df <- readr::read_csv(here::here("data", "2_clean_data.csv"))

# rename wonky variable name
df <- df %>% 
  dplyr::rename(RepEstimate = RepEstimate_1)


################# RECODING VARIABLES
  
# turn specific variables into factors & rename levels

df$crisis <- factor(df$crisis) %>% 
  mapvalues(
    c("1", "2", "3", "4"), 
    c("Significant Crisis", "Slight Crisis", "No Crisis", "Don't Know")
  )

df$OverallExperience <- factor(df$OverallExperience) %>% 
  mapvalues(
    c("1", "2", "3", "4"), 
    c("Unaware", "Aware, But Not Used", "Some Experience", "Extensive Experience")
  )

df$PreregExp1 <- factor(df$PreregExp1) %>% 
  mapvalues(
    c("1", "2", "3", "4"), 
    c("Unaware", "Aware, But Not Used", "Some Experience", "Regularly Preregister")
  )

df$CodeExp <- factor(df$CodeExp) %>%
  mapvalues(
    c("1", "2", "3", "4"),
    c("Unaware", "Aware, But Not Used", "Some Use", "Regular Use")
  )


################### Writing Data

# when done recoding, write the data to a new file
write.csv(df, here::here("data", "3_data_to_use.csv"), row.names = FALSE)





