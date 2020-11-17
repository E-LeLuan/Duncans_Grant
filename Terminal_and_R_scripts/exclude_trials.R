library(tidyverse)

# This is not finished yet, but is designed for excluding specific problematic trials.

# Function Call

# sink() prints the trial number output to a csv

for (f in asc_files) {
  sink_name <- paste0(f, ".csv")
  sink(sink_name, type = c("output"))
  fix_align(start_pts = start_pts,
            asc_files = f,
            fa_dir="/Users/duncan/Documents/R analyses/Prediction-Integration/FixAlign")
  sink() 
}
sink(file = NULL)

# Exclude Trials

# Reading in csv files
csv_list <- lapply(list.files(path = "RAW_asc/wider_aois", pattern = ".csv", full.names = TRUE), read_csv)

# Setting null values for objects used in loops
exclude_trials <- NULL
maxi <- NULL

# just first time
# set up exclude_trials with all of the file names
if (length(exclude_trials) < 1) {
  for (i in csv_list) {
    title <- (substring(names(i[1]), 18, 43))
    exclude_trials <- as_tibble(rbind(exclude_trials, title)) } 
}


# If the final trial has been processed successfully,
# there will be a zero in the output
# in which case, add 0 to the list.
# If not, find the maximum trial number
# maxi updates each time with values for all participants
for (i in csv_list) {
  if (min(i) == 0) {
    maxi = c(maxi, 0)
  } else {
    maxi = c(maxi, max(i))
  }
}

# update exclude_trials with maxi (and give the column a unique name)
# then reset maxi
exclude_trials <- as_tibble(cbind(exclude_trials, maxi), .name_repair = "unique")
maxi <- NULL

# for all rows in exclude_trials
# update the trial list by excluding trials in exclude_trials
for (i in rownames(exclude_trials)) {
  trial_list <- 1:n_trials
  exclude_these <- which(trial_list %in% exclude_trials[i,])
  if (length(exclude_these) == 0) {
    trial_list <- 1:n_trials
  } else { trial_list <- trial_list[ - which(trial_list %in% exclude_these)]
  }
}

write_csv(exclude_trials, path = "exclude_trials")




########### REPREX #


trial_list = 1:20

# setting null values for objects used in loops
exclude_trials <- NULL
maxi <- NULL
csv_list <- NULL

# creating two objects with the same structure as the Fix_Align trial output
# file1 stopped on trial 15
# file2 went all the way to trial 20
# to differentiate between cases where 20 is a problematic trial,
# and where it is the successful final trial,
# a zero is added to the output at the end of the whole process. 
# This final addition to be a number, so that the vector consists of numbers only.
file1 <- as_tibble(c(0:20)) %>% rename(file1 = value)
file2 <- as_tibble(c(0:20)) %>% rename(file2 = value)

csv_list <- lst(file1, file2)

# just first time
# set up exclude_trials with all of the file names
if (length(exclude_trials) < 1) {
  for (i in csv_list) {
    title <- names(i[1])
    exclude_trials <- as_tibble(rbind(exclude_trials, title)) } 
}

# Find the maximum trial number in the output
# If it's the final trial, put a zero
# If it's anything else, put that number on the end
# maxi updates each time with values for all participants
for (i in csv_list) {
  if (min(i) == 0) {
    maxi = c(maxi, 0)
  } else {
    maxi = c(maxi, max(i))
  }
}

# update exclude_trials with maxi (and give the column a unique name)
# then reset maxi
exclude_trials <- as_tibble(cbind(exclude_trials, maxi), .name_repair = "unique")
exclude_trials
maxi <- NULL

# for all rows in exclude_trials
# update the trial list by excluding trials in exclude_trials
for (i in rownames(exclude_trials)) {
  print(exclude_trials[i,]) 
  trial_list <- 1:20
  exclude_these <- which(trial_list %in% exclude_trials[i,])
  if (length(exclude_these) == 0) {
    trial_list <- 1:20
  } else { trial_list <- trial_list[ - which(trial_list %in% exclude_these)]
  }
  print(trial_list)
}


