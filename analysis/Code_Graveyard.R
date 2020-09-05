
#CONVERT TO A FACTOR
#convert the crisis variable to a FACTOR (fct). 
#plus it changes the names of the levels
#df$crisis <- factor(df$crisis)
#str(df$crisis) #test to make sure it's reading it as a factor
df$crisis <- factor(df$crisis) %>% 
  mapvalues(
  c("1", "2", "3", "4"), 
  c("Significant", "Slight", "No Crisis", "Don't Know")
  )
#str(df, list.len = ncol(df))  #allows me to see the column types for all variables
#&prevents it from being truncated

# mutate_if(is.character, ~factor(.)) # make it a factor

# figuring out the Academic Levels variables

# recoding them into a different variable (could also recode into same variable)
df$AcLevel_1_lab <- recode (df$AcLevel_1, '1' = "Prof")
df$AcLevel_2_lab <- recode (df$AcLevel_2, '1' = "Ass_Prof")
df$AcLevel_3_lab <- recode (df$AcLevel_3, '1' ="Sen_Lec")
df$AcLevel_4_lab <- recode (df$AcLevel_4, '1' = "Lec")
df$AcLevel_5_lab <- recode (df$AcLevel_5, '1' = "Postdoc")
df$AcLevel_6_lab <- recode (df$AcLevel_6, '1' = "PhD_student")
df$AcLevel_7_lab <- recode (df$AcLevel_7, '1' = "Masters_student")
df$AcLevel_9_lab <- recode (df$AcLevel_9, '1' = "RA")
df$AcLevel_10_lab <- recode (df$AcLevel_10, '1' = "Other")
df$AcLevel_12_lab <- recode (df$AcLevel_12, '1' = "Senior_Res_Fellow")
df$AcLevel_13_lab <- recode (df$AcLevel_13, '1' = "Res_Fellow")


# just checking the numbers to make sure I captured everything (need to remove 11)
x1 <- dplyr::count(df, AcLevel_1_lab)
x2 <- dplyr::count(df, AcLevel_2_lab)
x3 <- dplyr::count(df, AcLevel_3_lab)
x4 <- dplyr::count(df, AcLevel_4_lab)
x5 <- dplyr::count(df, AcLevel_5_lab)
x6 <- dplyr::count(df, AcLevel_6_lab)
x7 <- dplyr::count(df, AcLevel_7_lab)
x9 <- dplyr::count(df, AcLevel_9_lab)
x10 <- dplyr::count(df, AcLevel_10_lab)
x12 <- dplyr::count(df, AcLevel_12_lab)
x13 <- dplyr::count(df, AcLevel_13_lab)
print (c(x1 [1,2], x2 [1,2], x3 [1,2], x4 [1,2], x5 [1,2], x6 [1,2], x7 [1,2], x9 [1,2],
         x10 [1,2], x12[1,2], x13[1,2]))

# I will use this data to compare to the counts within the renamed variable

df2 <- filter(df, Multi_AcLevel >= 2) %>% 
  select (ParticipantNumber, AcLevel_1:AcLevel_13) #shows which participants said yes to 
# more than one AcLevel



# FIGURE OUT COLUMN NAMES & TYPES

#shows the column types for all variables &prevents it from being truncated
str(df, list.len = ncol(df))

#problems(df)
#pec(df) #shows me the column types, but for some reason this isn't working
#cols(df) gives me an error that I shouldn't get re "some 'col_types' are not 
#S3 collector objects: 1" --> there is some sort of problem with the list of 
#column names as far as I can tell, but that is a problem for future jen!

#FIGURING OUT SIMPLE DESCRIPTIVE STATS
# gives you a quick idea of how many real cases are included in the variable
plyr::count(!is.na(df$RepEstimate_1))

#PLAYING WITH FILTER

mpg

ggplot(data = mpg) +
  geom_point(mapping = 
               aes(x = displ, y = hwy))
mpg
count(mpg$hwy)

cyl4 <- filter(mpg, cyl == 4)
ggplot(data = cyl4) +
  geom_point(mapping = 
               aes(x = displ, y = hwy))


# estimates of reproducibility by levels of crisis as a stripchart 
  # This code works as long as 'crisis' isn't a factor already. Saving just in case.
  # the !is.na removes NA from the 'RepEstimate_1' variable). 
  # This works fine, but can tweak to increase font size & perhaps add boxplot & remove background grid.
p <- ggplot(data = subset(df, !is.na(RepEstimate_1)), 
            aes(x=crisis, y=RepEstimate_1)) + 
  geom_jitter(position=position_jitter(height=0,width=.15),
              fill="blue",
              colour="blue",
              size = 2.2,
              alpha=.5) +
  stat_summary(fun.y=mean,
               fun.ymin=mean,
               fun.ymax=mean,
               geom='crossbar',
               width=0.5) +
  scale_x_discrete(name = "Do you think your field is experiencing a 'Reprodubility Crisis'?",
                   limits=c("4","3","2","1"),
                   labels=c("Don't Know", "No Crisis", "Slight", "Significant")) +
  scale_y_continuous(name = "Estimated %age of reproducible studies") +
  coord_cartesian(ylim = c(0,100)) +
  theme (text = element_text(size = 12))

p + ggtitle("Estimates of reproducibility by perceived crisis in field (n = 137)")

# code for bar chart for preregistration concerns
  # as of April 2020, I am not using this because I am presenting the percentages

# this works, but with counts
p <- ggplot(PreRegCon_long, aes (x = PreRegConcern)) +
  geom_bar(fill = "lightskyblue3", colour = "black")  +
  labs (x = element_blank(),
        y = "Frequency", 
        title = title) +
  coord_flip() +
  theme_classic(base_size = 12) 
p


# this works too and calculates the percentage within the code itself. I could also use
  # "..count../..sum.." to calculate the proportion. 
p <- ggplot(PreRegCon_long) + aes (x = PreRegConcern) +
         geom_bar(aes(y = ((..count../n_prereg_con)*100)),
                      fill = "lightskyblue3", colour = "black")  +
    labs (x = element_blank(),
      y = "Percentage", 
        title = title) +
  coord_flip() +
 theme_classic(base_size = 12) 


#### CODE FOR OPEN MATERIALS / CODE EXPERIENCE ####

{r code_bar, echo = FALSE, warning = TRUE, eval = FALSE}

title = "What is your experience with open materials and/or code (n = %d)"
p <- simple_bar_graph(df, label_var = "CodeExp", the_title = title, xname = "Response given", 
                      bar_colour = "orange",line_colour = "black",the_quantity="count")
p

# arrange by descending frequency

# dplyr::arrange(a2, desc(n)) %>% 
# knitr::kable(col.names = c("Academic Levels", "Frequency", "Percentage"), caption = sprintf("Academic # Levels of Respondents (n = %d)",nvalid_ac_level)) %>% 
#  kable_styling(bootstrap_options = "striped", full_width = F, position = 'center')

# rearrange the rows



#ap <- ggplot(data = valid_ac_level, aes (AcLevel_Label)) + 
#  geom_bar() +
#  labs (x = "Frequency", 
#        y = "Academic Levels", 
#        title = sprintf("Reported Academic Levels (n = %d)",nvalid_ac_level)) +
#  coord_flip() + 
#  theme_classic(base_size = 12)
#ap

# code to save figure. If using it for slides, increase the 'base_size' from 12 to 18. 
#ggsave(here::here("figs", "aclevels_bar.png"))

###################### EFFORTS TO SPLIT OUT ASSH VS STEM RESPONDENTS ######

### Discipline

#d4 <- assh %>% 
#  dplyr::count(assh) %>% 
#   mutate (Percentage = round (n/nrow(df)*100))

#dplyr::arrange(d4) %>% 
#knitr::kable(col.names = c("Disciplines", "Frequency", "Percentage"), caption = #sprintf("Discipline Groupings of Respondents (n = %d)",nrow(df))) %>% 
#  kable_styling(bootstrap_options = "striped", full_width = F, position = 'center')

#### OS Experience ####
```{r os_experience_pie_assh, echo = FALSE, eval = FALSE}

# data for just ASSH respondents
title = "What is your experience with open science practices? HAS respondents (n = %d)"
d_assh <- df %>% filter(assh == "ASSH")

# pie chart
colours <- brewer.pal(4, "Blues")
p_assh <- simple_pie_chart(the_data = d_assh, label_var = "OverallExp",
                           the_title = title, 
                           colours,the_quantity = "percent")
p_assh

# data for just STEM respondents
title = "What is your experience with open science practices? STEM respondents (n = %d)"
d_stem <- df %>% filter(assh == "STEM")

# pie chart
colours <- brewer.pal(4, "Blues")
p_stem <- simple_pie_chart(the_data = d_stem, label_var = "OverallExp",
                           the_title = title, 
                           colours,the_quantity = "percent")
p_stem
```

#### PREREG EXPERIENCE ####

```{r prereg_exp_pie_chart_assh, echo = FALSE, warning = TRUE, eval=FALSE}

d_assh <- df %>% filter(assh == "ASSH") 
title = "What is your experience with preregistration? HAS respondents (n = %d)"

# pie chart
colours <- brewer.pal(4, "Blues")
p_assh <- simple_pie_chart(the_data = d_assh, label_var = "PreregExp1", 
                           the_title = title, 
                           colours,the_quantity = "percent")
p_assh

rm(title)
```

#### PREREG IMPORTANCE ####

```{r prereg_imp_ex_pie_chart_assh, echo = FALSE, warning = TRUE, eval = FALSE}

title = "How important is it that researchers preregister their studies? HAS respondents (n = %d)"

d_assh <- df %>% 
  filter(assh == "ASSH") %>% 
  filter(PreRegImp != "Researchers in my discipline do not conduct research studies")

# pie chart
colours <- brewer.pal(4, "Blues")
p_assh <- simple_pie_chart(the_data = d_assh, label_var = "PreRegImp", 
                           the_title = title, 
                           colours,the_quantity = "percent")
p_assh
```
#### TRYING TO RENAME VALUES WITHIN VARIABLES

# don't do this...old approach...
# recode the data so the response options have proper labels
# create a character vector with proper labels
code_con_key <- c (CodeCon_criticise = "Others might criticise my materials/code", 
                   CodeCon_diff_understand = "Others might find it difficult to understand my materials/code",
                   CodeCon_assistance = "Others might ask for my assistance with their research",
                   CodeCon_lose_control = "I might lose control over how they are used", 
                   CodeCon_errors = "Others might find errors in my published work", 
                   CodeCon_credit = "I might not receive appropriate credit",
                   CodeCon_violate = "Reuse could violate epistemological framework", 
                   CodeCon_ip = "Issues related to intellectual property", 
                   CodeCon_no_con = "No concerns")

#data_con_var <- metadata$NewVariable
#data_con_text <- metadata$ItemText
#a <- paste0(data_con_var, data_con_text, sep = " = ")
#data_con_key <- str_c(data_con_var, sep = " = ", "data_con_text")

#data_con_key <- 

#DataConcern_long


keys   <- metadata[[deparse(substitute(old))]]
values <- metadata[[deparse(substitute(new))]]
rename_at(df, vars(keys), ~ values)

# recode the values with this character vector
CodeCon_long$CodeConcern <- recode(CodeCon_long$CodeConcern, !!!code_con_key)

DataConcern_long$DataConcern_text <- DataConcern_long$DataConcern
DataConcern_long$DataConcern_text <- recode(DataConcern_long$DataConcern_text, !!!data_con_key)

b <- DataConcern_long
b$DataConcern_text <- factor(b$DataConcern_text)
c <- metadata

b$DataConcern_text <-c$ItemText[match(b$DataConcern_text, c$NewVariable)]


meta_rename <-  function(df, metadata, old, new) {
  
  keys   <- metadata[[deparse(substitute(old))]]
  values <- metadata[[deparse(substitute(new))]]
  rename_at(df, vars(keys), ~ values)
}

# this works! (and no need to force it to factor, yet, but will likely need it later)
DataConcern_long$DataConcern_text <- metadata$ItemText[match(DataConcern_long$DataConcern, metadata$NewVariable)]

# but trying to do it in tidyverse

b <- DataConcern_long
c <- metadata

#I'm sure I could turn this into a function, but I don't have the time now

d <- full_join(b, c, by = c("DataConcern" = "NewVariable")) %>% 
  rename ("DataConcern_text" = "ItemText")


# need to figure out an easier way to rename the variables to use the item text
data_con <- meta_rename(DataConcern_long, metadata, old = NewVariable, new = ItemText)


#### DESPERATELY TRYING TO FIGURE OUT HOW TO REARRANGE THE OPEN DATA CONCERNS BY 
  # ONE VARIABLE (DATACONCERN) WHILE USING ANOTHER VARIABLE FOR THE LABELS (DATACONCERN_TEXT)
  # NO MATTER WHAT I DID, THE ORDER CHANGED WHEN I USED THE TEXT VARIABLE...
  # I EVENTUALLY GAVE UP AND JUST ORDERED THE FACTOR ACCORDING TO THE TEXT VARIABLE

# reorder the rows according to the DataConcern (because it has less text)
tb3 <- tb2 %>% 
  dplyr::mutate(DataConcern = factor(DataConcern, 
                                     levels = c("DataConcern_other", 
                                                "DataConcern_devalues",
                                                "DataConcern_unfair", 
                                                "DataConcern_criticise",
                                                "DataConcern_effort", 
                                                "DataConcern_control",
                                                "DataConcern_credit",
                                                "DataConcern_scoop",
                                                "DataConcern_ip",
                                                "DataConcern_privacy",
                                                "DataConcern_ethics",
                                                "DataConcern_none"))) %>% 
  dplyr::mutate(DataConcern_text = factor(DataConcern_text)) #turn into factors

tb5 <- tb2 %>% 
  dplyr::mutate(DataConcern = factor(DataConcern)) %>% 
  relevel(DataConcern, "DataConcern_none") %>% 
  reorder()

DataConcern_fct <- factor(tb2$DataConcern)
relevel(tb2, DataConcern_fct, "DataConcern_none")

con_short <- tb3$DataConcern
con_text <- tb3$DataConcern_text
plyr::rename(tb3$DataConcern, con_text)

# ??? rename in dplyr??

tb4 <- vars_select(names(tb3), DataConcern = DataConcern_text)

con_short <- tb3$DataConcern
con_text <- tb3$DataConcern_text
plyr::rename(tb3$DataConcern, con_text)

# ??? rename in dplyr??

tb4 <- vars_select(names(tb3), DataConcern = DataConcern_text)

# this caption actually works, but the order still changed
caption2 <- tb3$DataConcern_text %>% (function(x) str_wrap(x, width = 50))


# STILL TRYING TO USE ONE DATA SET FOR THE PERCENTAGE & ONE FOR THE NAMES....

# reorder the rows according to the DataConcern (because it has less text)
tb3 <- tb2 %>% 
  dplyr::mutate(DataConcern = factor(DataConcern, 
                                     levels = c("DataConcern_other", 
                                                "DataConcern_devalues",
                                                "DataConcern_unfair", 
                                                "DataConcern_criticise",
                                                "DataConcern_effort", 
                                                "DataConcern_control",
                                                "DataConcern_credit",
                                                "DataConcern_scoop",
                                                "DataConcern_ip",
                                                "DataConcern_privacy",
                                                "DataConcern_ethics",
                                                "DataConcern_none"))) %>% 
  dplyr::mutate(DataConcern_text = factor(DataConcern_text)) #turn into factors


https://raw.githubusercontent.com/janhove/janhove.github.io/master/RCode/sortLvls.R

# HIS ORIGINAL CODE

sortLvls.fnc <- function(oldFactor, levelOrder) {
  if(!is.factor(oldFactor)) stop("The variable you want to reorder isn't a factor.")
  
  if(!is.numeric(levelOrder)) stop("'order' should be a numeric vector.")
  
  if(max(levelOrder) > length(levels(oldFactor))) stop("The largest number in 'order' can't be larger than the number of levels in the factor.")
  
  if(length(levelOrder) > length(levels(oldFactor))) stop("You can't have more elements in 'order' than there are levels in the factor.")
  
  if(length(levelOrder) == length(levels(oldFactor))) {
    reorderedFactor <- factor(oldFactor, levels = levels(oldFactor)[levelOrder])
  }
  
  if(length(levelOrder) < length(levels(oldFactor))) {
    levelOrderAll <- c(levelOrder, (1:length(levels(oldFactor)))[-levelOrder])
    reorderedFactor <- factor(oldFactor, levels = levels(oldFactor)[levelOrderAll])
  }
  
  return(reorderedFactor)
}

### ME PLAYING WITH IT! IT WORKS, in theory, BUT IT STILL DOESN'T FORCE GGPLOT TO USE THE TEXT

sortLvls.fnc <- function(oldFactor, levelOrderFctr) {
  if(!is.factor(oldFactor)) stop("The variable you want to reorder isn't a factor.")
  
  if(!is.factor(levelOrderFctr)) stop("The level variable you want to use isn't a factor.")
  
  if(length(levelOrderFctr) == length(levels(oldFactor))) {
    reorderedFactor <- factor(oldFactor, levels = levels(oldFactor)[levelOrderFctr])
  }
  
  return(reorderedFactor)
}

tb3$DataConcern_text_order <- sortLvls.fnc(oldFactor = tb3$DataConcern_text, levelOrderFctr = tb3$DataConcern)

# HIS ORIGINAL CODE

sortLvlsByVar.fnc <- function(oldFactor, sortingVariable, ascending = TRUE) {
  
  require("dplyr")
  require("magrittr")
  
  # Combine into data frame
  df <- data.frame(oldFactor, sortingVariable)
  
  ###
  ### If you want to sort the levels by, say, the median, sd etc. instead of the mean,
  ### just change 'mean(sortingVariable)' below to, say, 'median(sortingVariable)'.
  ###
  
  # Compute average of sortingVariable and arrange (ascending)
  if (ascending == TRUE) {
    df_av <- df %>% group_by(oldFactor) %>% summarise(meanSortingVariable = mean(sortingVariable)) %>% 
      arrange(meanSortingVariable)
  }
  
  # Compute average of sortingVariable and arrange (descending)
  if (ascending == FALSE) {
    df_av <- df %>% group_by(oldFactor) %>% summarise(meanSortingVariable = mean(sortingVariable)) %>% 
      arrange(desc(meanSortingVariable))
  }
  
  # Return factor with new level order
  newFactor <- factor(oldFactor, levels = df_av$oldFactor)
  return(newFactor)
}

caption <- tb3$DataConcern_text_order %>% (function(x) str_wrap(x, width = 50))


# create a character vector for the colours for each bar
cols <- c("lightskyblue3", "lightskyblue3", "lightskyblue3", "lightskyblue3", 
          "lightskyblue3", "lightskyblue3", "lightskyblue3", "lightskyblue3", 
          "lightskyblue3", "lightskyblue3", "lightskyblue3", "grey50")


# build the bar chart
title = sprintf("Concerns about open data (n = %d)", n_data_con)

p <- ggplot(tb3) + 
  aes(x = DataConcern, y = Percentage) +
  geom_bar(stat = "identity", colour = "black", fill = cols) +
  labs (x = element_blank(),
        y = "Percentage", 
        title = title) +
  coord_flip(ylim = c(0,60)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 50)) + #wrap the labels
  theme_classic(base_size = 12) + 
  theme(plot.title = element_text(hjust = 0.5)) # centre the title
p

# RECODING (now done with meta rename)

# recode preregistration concerns & make them factors [could simplify this with
# meta rename that I used with data concern]
df$PreRegCon_delay <- factor(df$PreregConcern_4)
df$PreRegCon_look <- factor(df$PreregConcern_5)
df$PreRegCon_prevent_exp <- factor(df$PreregConcern_6)
df$PreRegCon_stifle_creativity <- factor(df$PreregConcern_7)
df$PreRegCon_scooping <- factor(df$PreregConcern_8)
df$PreRegCon_prevent_sig <- factor(df$PreregConcern_10)
df$PreRegCon_diff_pub <- factor(df$PreregConcern_11)
df$PreRegCon_no_con <- factor(df$PreregConcern_12)

# recode open code concerns [could simplify this with
# meta rename that I used with data concern]

df$CodeCon_criticise <- factor(df$CodeConcern_4)
df$CodeCon_diff_understand <- factor(df$CodeConcern_5)
df$CodeCon_assistance <- factor(df$CodeConcern_6)
df$CodeCon_lose_control <- factor(df$CodeConcern_7)
df$CodeCon_errors <- factor(df$CodeConcern_8)
df$CodeCon_credit <- factor(df$CodeConcern_9)
df$CodeCon_no_con <- factor(df$CodeConcern_12)
df$CodeCon_violate <- factor(df$CodeConcern_13)
df$CodeCon_ip <- factor(df$CodeConcern_14)

# use character vector to change labels
prereg_con_key <- c (PreRegCon_delay = "Delays data collection", 
                     PreRegCon_look = "Need to look at data to analyse it",
                     PreRegCon_prevent_exp = "Prevents exploratory research",
                     PreRegCon_stifle_creativity = "Stifles creativity", 
                     PreRegCon_scooping = "Risk of scooping", 
                     PreRegCon_prevent_sig = "More difficult to find significant results",
                     PreRegCon_diff_pub = "More difficult to publish in certain journals", 
                     PreRegCon_no_con = "No concerns")

# recode the values with this character vector 
PreRegCon_long$PreRegConcern <- recode(PreRegCon_long$PreRegConcern, !!!prereg_con_key)

# same code but for code concerns
# recode the data so the response options have proper labels
# create a character vector with proper labels
code_con_key <- c (CodeCon_criticise = "Others might criticise my materials/code", 
                   CodeCon_diff_understand = "Others might find it difficult to understand my materials/code",
                   CodeCon_assistance = "Others might ask for my assistance with their research",
                   CodeCon_lose_control = "I might lose control over how they are used", 
                   CodeCon_errors = "Others might find errors in my published work", 
                   CodeCon_credit = "I might not receive appropriate credit",
                   CodeCon_violate = "Reuse could violate epistemological framework", 
                   CodeCon_ip = "Issues related to intellectual property", 
                   CodeCon_no_con = "I do not share any of these concerns")

# recode the values with this character vector
CodeCon_long$CodeConcern <- recode(CodeCon_long$CodeConcern, !!!code_con_key)



# OLD WAY OF RECODING ACADEMIC LEVELS USING IFELSE STATEMENTS
  # replaced with `case_when`

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

# Trying to get rid of rows that have NAs in all relevant columns..

https://blog.exploratory.io/applying-filter-condition-to-multiple-columns-together-with-filter-at-filter-if-commands-379281ac0a3b

  # shockingly this code actually works, except it gets rid of ALL rows with NAs 
  # anywhere, rather than only those rows with NAs in every single row....

df_imp_ex <- df_imp %>% filter_at(vars(ends_with("Imp")), all_vars(!is.na(.)))
# the solution is to use any_vars! Keeps the rows as long as the values of at 
  # least one of the columns satisfies the condition! 

df_imp_ex <- df_imp %>% filter_at(vars(ends_with("Imp")), any_vars(!is.na(.)))


# CONVOLUTED WAY OF BUILDING THE TIBBLES FOR STACKED BARS

# try creating tibbles for each variable that I can join? 
# filter out irrelevant responses, but keep NAs for now

# filter(!is.na(PreRegImp))

# PreRegImp

a <- df %>% 
  select(c("ParticipantNumber", "PreRegImp", "FORcode_2_label")) %>% 
  filter(PreRegImp != "Researchers in my discipline do not conduct research studies") %>% 
  mutate_if(is.character, ~factor(.))

# CodeImp

b <- df %>% 
  select(c("ParticipantNumber", "CodeImp", "FORcode_2_label")) %>% 
  filter(CodeImp != "Researchers in my discipline do not use materials and/or code") %>% 
  mutate_if(is.character, ~factor(.))

# DataImp

c <- df %>% 
  select(c("ParticipantNumber", "DataImp", "FORcode_2_label")) %>% 
  filter(DataImp != "Research publications in my field are not based on data") %>% 
  mutate_if(is.character, ~factor(.))

# PrePubImp

d <- df %>% 
  select(c("ParticipantNumber", "PrePubImp", "FORcode_2_label")) %>% 
  mutate_if(is.character, ~factor(.))

# join the tibbles together

df_imp <- a %>% full_join(b) %>% full_join(c) %>% full_join(d)


#### NOTES ABOUT SETTINGS WHEN RENDERING THE DOCUMENT ####

  ### NOTES FROM THE R SETUP CHUNK ###

# If I knit as a word doc:
# switch auto_format to 'false'; 'true' for pdf or html
# add 'fig.width = 8, fig.height = 8' to opts_chunk 

# If knit as a pdf or html:
# switch auto_format to 'true' 
# include width and height parameters in the plot_ly function
#  width = 300, 
#  height = NULL,
# along with all other parameters
# need to also figure out how to modify the margins (maybe the orca package or 
# maybe the webshot settings? More research needed.)

  ### NOTES FROM THE YAML ###

output:
  # bookdown::pdf_document2: #another option for pdf output, but it makes things very wonky, 
  # including the captions & the text becomes centred.
    # keep_tex: true
    # toc: false
  #word_document: 
  # fig_width: 10 # this works, but it changes the font of plot_ly, etc.
  # fig_height: 12 # so, don't use for now
  # rmdformats::readthedown:
  #  self_contained: true
  # thumbnails: true
  #lightbox: true
  #gallery: false
#fig_retina: 1
#toc_depth: 3
# html_document:  
# number_sections: true
# toc: true
pdf_document: default # this works now
always_allow_html: false # true for word; false for pdf (?)

### ADD IN LATEX FONTS FOR GGPLOT
  # How do I add latex fonts for ggplot? 
  # Pay attention to instructions at the bottom re: downloading the appropriate files
https://medium.com/@fulowa/latex-font-in-a-ggplot-9120caaa5250
  # this all gets quite complicated though. I can't just knit this file with this 
  # font (though I can just make the figures by running the code, so now I have
  # to separately load in the figures)



