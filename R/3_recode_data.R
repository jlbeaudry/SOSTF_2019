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

df$crisis <- factor(df$crisis) %>% 
  mapvalues(
    c("1", "2", "3", "4"), 
    c("Significant Crisis", "Slight Crisis", "No Crisis", "Don't Know")
  )



df %>% 
  mutate(crisis = fct_recode(crisis,
                             "Significant Crisis" = "1", 
                             "Slight Crisis" = "2", 
                             "No Crisis" = "3", 
                             "Don't Know" = "4"
                               )) %>% 
  count(crisis)


# 31.1.20 I will try to turn these into functions if possible and then will probably 
  # save them in a new script that is specifically about recoding data.But, I will
  # play with this on my own branch. Nope, I don't think it's worth doing these a function.
  # But I am cleaning it up so it's easier, and then will add to a new R file



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

# when done recoding, write the data to a new file
write.csv(df, here::here("data", "3_data_to_use.csv"))





