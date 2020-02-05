# Clean the data to use with my '.Rmd' file and analyses

library(tidyverse)
library(here)
#library(plyr) #needed to mapvalues for factors; but lets see without...


# import the data file produced by '1_read_data.R'
df <- read_csv(here::here("data", "1_data_to_clean.csv"))

# start cleaning the data:

# need to delete my test data (participant id = 1062) & assign it to a new object. 
  # should have 240 observations rather than 241. 

df <- df %>% 
  filter(ParticipantNumber != "1062")

# selects all columns except for the junk columns, which we don't need 
  # (e.g., V1, location, etc.).
df <- df %>% 
  select (-c(X1, V1:V10, consent, Intro, LocationLatitude, LocationLongitude, 
            LocationAccuracy, PreregDef)) 

# when done cleaning, write the data to a new file
write.csv(df, here::here("data", "2_clean_data.csv"),row.names = FALSE)
# row.names gets rid of the first column from the dataframe.

# use this csv file in step 3, which recodes variables for use. See '3_recode_data.R'.



