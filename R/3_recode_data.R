# Clean the data to use with my '.Rmd' file and analyses

library(tidyverse)
library(here)
#library(plyr) #needed to mapvalues for factors; but lets see without...

# import the data file produced by '2_clean_data.R'
df <- readr::read_csv(here::here("data", "2_clean_data.csv"))

# turn specific variables into factors (will rename levels later)

df$crisis <- factor(df$crisis)
df$OverallExperience <- factor(df$OverallExperience)
df$CodeExp <- factor(df$CodeExp)

# rename levels for the factors using mutate because the recoding is cleaner

df %>% 
  dplyr::mutate(crisis = fct_recode(crisis,
                             "Significant Crisis" = "1", 
                             "Slight Crisis" = "2", 
                             "No Crisis" = "3", 
                             "Don't Know" = "4"
                               )) %>% 
  count(crisis) #count, just to see numbers

df %>% 
  dplyr::mutate(OverallExperience = fct_recode(OverallExperience,
                                        "Unaware" = "1",
                                        "Aware, But Not Used" = "2", 
                                        "Some Experience" = "3", 
                                        "Extensive Experience"= "4"
                                        )) %>% 
  count(OverallExperience)


df %>% 
  dplyr::mutate(CodeExp = fct_recode(CodeExp,
                                               "Unaware" = "1",
                                               "Aware, But Not Used" = "2", 
                                               "Some Use" = "3", 
                                               "Regular Use"= "4"
  )) %>% 
  count(CodeExp)

# when done recoding, write the data to a new file
write.csv(df, here::here("data", "3_data_to_use.csv"))





