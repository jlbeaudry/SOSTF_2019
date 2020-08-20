# Career level is coded as AClevel 1:10
df <- df %>%
  mutate(CareerLevel = case_when(
    AcLevel_1 == 1 ~ "Professor",
    AcLevel_2 == 1 ~ "Associate Professor",
    AcLevel_3 == 1 ~ "Senior Lecturer",
    AcLevel_4 == 1 ~ "Lecturer",
    AcLevel_5 == 1 ~ "Postdoctoral Fellow",
    AcLevel_6 == 1 ~ "PhD Student",
    AcLevel_7 == 1 ~ "Research Masters Student",
    AcLevel_9 == 1 ~ "Research Assistant",
    AcLevel_12 == 1 ~ "Senior Research Fellow",
    AcLevel_13 == 1 ~ "Research Fellow",
    AcLevel_10 == 1 ~ AcLevel_10_TEXT,
    TRUE ~ "Unlisted"
  ))