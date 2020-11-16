# PART 1 in Terminal #

# I assume that all your .asc files are in one folder, and none are duplicated

# 1. Open terminal
# 2. Type in 'cd' and then the path to the folder that your .asc files are in
#     e.g. I type: cd Documents/'R Analyses'/Prediction-Integration/RAW_asc
#     If you have a space in any of the folder names, write it in '' quotes
#     like I do with 'R Analyses'.
#     Then press enter.
# 3. Now type in the following:
#     head -n 1 *.asc > filenames.csv
#     Then press enter.
# 4. You will now have a new csv file in the folder where the .asc files are stored
#     Move this file to the folder where your R project and this R script are stored


# PART 2 in R #

library(tidyverse)

# read in the csv file
fn <- read_csv('filenames.csv', col_names = FALSE)

# split the 1 column into 2
fn <- fn %>%
  mutate(ind = rep(c(1, 2),length.out = n())) %>%
  group_by(ind) %>%
  mutate(id = row_number()) %>%
  spread(ind, X1) %>%
  select(-id)

# take just the substrings you need
fn$`1` <- substring(fn$`1`, 5, 10)
fn$`2` <- gsub(".*Desktop","",fn$`2`) # remove the text before 'Desktop' (including 'Desktop')
fn$`2` <- gsub(" using.*","",fn$`2`) # remove the text after 'EDF' (including 'EDF')

write_csv(fn, path = 'filenames.csv')

# IGNORE:
duplicated(fn$`2`)

