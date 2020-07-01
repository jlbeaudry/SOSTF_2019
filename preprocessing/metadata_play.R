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

#' Replaces names by reference to a metadata dataframe
#'
#' `meta_rename` allows fast replacement of variable names by key-value pairing with a metadata file.
#'
#' @param df A data.frame object for renaming.
#' @param metadata A data.frame object containing the key-value pair
#' @param old An unquoted string of the key column name in the metadata object
#' @param new An unquoted string of the value column name in the metadata object
#'
#' @return `data.frame` object with new names as per key-value pairs.
#' @export
#'
#' @examples
#'
#' testdata <-  tibble(a = 0L,
#' b = 0L,
#' c = 0L)
#'
#' meta <-  tibble(DatasetPositon = 1:3,
#'                 VariableID =	letters[1:3],
#'                 VariableType = "integer",
#'                 VariableLabel = c("item1", "item2", "item3"),
#'                 ItemText = c("This is item 1",
#'                              "This is item 2",
#'                              "This is item 3")
#' )
#'
#' meta_rename(testdata, metadata = meta, old = VariableID, new = VariableLabel)
#'
meta_rename <-  function(df, metadata, old, new) {
  
  keys   <- metadata[[deparse(substitute(old))]]
  values <- metadata[[deparse(substitute(new))]]
  rename_at(df, vars(keys), ~ values)
}

####### READ IN DATA FILE ############

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

# load in metadata
metadata <- here::here("survey", "data", "os_metadata_concerns.csv") %>% 
  read_csv(col_names = TRUE, skip_empty_rows = TRUE) %>% 
  filter(!is.na(OldVariable)) %>% 
  select(-c(MinValue:ImportID)) #does not import the extraneous variables



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

################# RECODING VARIABLES ##################

# rename wonky variable names & change FOR code columns to reflect number of digits. 

df <- df %>% 
  dplyr::rename(RepEstimate = RepEstimate_1, FORcode_4 = FORcode_2, FORcode_2 = FORcode_1) 


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

# then recode according to discipline [CHECK OUT "CASE WHEN"]

df <- df %>% 
  mutate (discipline = ifelse (FORcode_2_num %in% c('14','15','18'), "Business & Law",
                               ifelse (FORcode_2_num %in% c('13','16','19','20'), "ASSH",
                                       ifelse (FORcode_2_num %in% '2', "Physical Sciences", 
                                               ifelse (FORcode_2_num %in% c('1','3','5','6'), "Math, Chem, Enviro, & Bio Sciences", 
                                                       ifelse (FORcode_2_num %in% c('8','10'), "Tech & Comp Sciences", 
                                                               ifelse (FORcode_2_num %in% '9', "Engineering", 
                                                                       ifelse (FORcode_2_num %in% '11', "Medical & Health Sciences", 
                                                                               ifelse (FORcode_2_num %in% '17', "Psyc & Cog Sciences",
                                                                                       ifelse (FORcode_2_num %in% c('12', '99'), "Other", "Not Specified"))))))))))



################## RENAME OPEN DATA CONCERNS USING METADATA ##############

df <- meta_rename(df, metadata, old = OldVariable, new = NewVariable) 


###### NEXT STEP FOR WORKING WITH THIS DATA ###########

# wrangle this data so it's in long form & only includes endorsements.
# this works...

DataConcern_long <- df %>% 
  select (c("ParticipantNumber", 
            "DataConcern_criticise":"DataConcern_scoop",
            "FORcode_2_label")) %>%  
  select(-"DataConcern_text") %>% 
  pivot_longer(c("DataConcern_criticise":"DataConcern_scoop"),
               names_to = "DataConcern", 
               values_to = "DataConcern_Endorsement") %>% 
  filter(DataConcern_Endorsement == "1")


# recode the 'Data Concern' values with the item labels from the meta data
b <- DataConcern_long
c <- metadata %>% 
  filter(NewVariable != "DataConcern_text") # remove this variable from metadata

#I'm sure I could turn this into a function, but I don't have the time now

d <- full_join(b, c, by = c("DataConcern" = "NewVariable")) %>% 
  rename ("DataConcern_text" = "ItemText")

