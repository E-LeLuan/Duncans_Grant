library(tidyverse)


# FA Processing
  # read in the data

# separate the columns into file and count
FA_detect_script_error <- read_csv("FixAlign/FA_asc_files/detect_script_error_asc.csv", col_names = FALSE) %>%
  separate(col = X1, sep = ":", into = c("file", "count"))


# replace asc with DA1 - this is what .lst needs
FA_detect_script_error$file <- str_replace(string = FA_detect_script_error$file, 
                                        pattern = "asc", 
                                        replacement = "DA1")

# non-problematic trials
FA_detect_script_error %>% 
  filter(count == 0) %>%
  select(file) %>%
  write_csv(path = 'FA_filenames_corr.csv', col_names = FALSE)

# problematic trials
FA_detect_script_error %>% 
  filter(count > 0) %>%
  select(file) %>%
  write_csv(path = 'FA_filenames_error.csv', col_names = FALSE)

# where count = 2, likely due to recalibration


#### CREATING BATCHES - FixAlign
#This was done in previous versions as there was problems with certain files that would stop
#the fixalign process. However, these files were not relevant to this study and have simply been removed.
#As such batching participant data out is not needed.

# files with no error in script (correct)
# read in data
FA_filenames_corr <- read_csv('FA_filenames_corr.csv', col_names = FALSE) 
#41 obs. 15, 15, 11  

# assign batch numbers
batch_numbers_FA_corr <- c(rep("batch1", 15),
rep("batch2", 15),
rep("batch3", 11))

# add batch numbers as a column
FA_filenames_corr <- FA_filenames_corr %>%
cbind(batch_numbers_FA_corr)

# write separate .csv files
FA_filenames_corr %>%
filter(batch_numbers_FA_corr == "batch1") %>%
select(X1) %>%
write_csv(path = 'FA_batch1_corr.csv', col_names = FALSE)

FA_filenames_corr %>%
filter(batch_numbers_FA_corr == "batch2") %>%
select(X1) %>%
write_csv(path = 'FA_batch2_corr.csv', col_names = FALSE)

FA_filenames_corr %>%
filter(batch_numbers_FA_corr == "batch3") %>%
select(X1) %>%
write_csv(path = 'FA_batch3_corr.csv', col_names = FALSE)

# files with error in script 
#read in data
FA_filenames_error <- read_csv('FA_filenames_error.csv', col_names = FALSE) 
#25 obs. 15, 10

# assign batch numbers
batch_numbers_FA_error <- c(rep("batch4", 15),
                      rep("batch5", 10))

# add batch numbers as a column
FA_filenames_error <- FA_filenames_error %>%
  cbind(batch_numbers_FA_error)

# write separate csv files
FA_filenames_error %>%
filter(batch_numbers_FA_error == "batch4") %>%
select(X1) %>%
write_csv(path = 'FA_batch4_error.csv', col_names = FALSE)

FA_filenames_error %>%
filter(batch_numbers_FA_error == "batch5") %>%
select(X1) %>%
write_csv(path = 'FA_batch5_error.csv', col_names = FALSE)
