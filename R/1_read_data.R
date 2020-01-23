# Code to read in the legacy format of Qualtrics data.

library(tidyverse)
library(here)


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
df <- here::here("data", "OS_Data_ID_Legacy.csv") %>% 
  read_qualtrics()

# write the dataframe to a 'csv' file in the data folder
write.csv(df, here("data", "data_to_clean.csv"))

# use this csv file in step 2, which cleans it for use. See '2_clean_data.R'.








