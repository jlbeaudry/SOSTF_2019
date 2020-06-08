
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


