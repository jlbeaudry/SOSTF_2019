
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