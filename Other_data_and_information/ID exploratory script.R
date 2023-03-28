# ID script exploring the ID's
library(tidyverse)
library(readr)
All_IDs <- read_csv("Other_data_and_information/All_IDs.csv")
View(All_IDs)

AD <- All_IDs %>%
  mutate(Group_Status = SRS_Category <= 'TD')

view(Group_Status)

# Rename TRUE FALSE to more meaningful labels.
AD$Group_Status[AD$Group_Status == 'TRUE'] <- "TD"
AD$Group_Status[AD$Group_Status == 'FALSE'] <- "HAT"
view(AD)

# Create subset data lists
ASC_Group <- filter(AD, Group_Status == "HAT")
TD_Group <- filter(AD, Group_Status == "TD")

# average???

All_IDs %>% 
  group_by(SRS_total_score_t) %>%
summarise(mean(SRS_total_score_t), sd(SRS_total_score_t))

