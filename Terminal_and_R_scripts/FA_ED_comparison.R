library(tidyverse)
library(plyr)
library(dplyr)
library(naniar)
library(reshape2)
library(fitdistrplus)
library(lme4)
library(lmerTest)
library(broom)
library(buildmer)
library(janitor)

# reading in total time fix_align files
TT_FA_batch1_corr <- read_csv('FA_files/TT_FA_batch1_corr')
TT_FA_batch2_corr <- read_csv('FA_files/TT_FA_batch2_corr')
TT_FA_batch3_corr <- read_csv('FA_files/TT_FA_batch3_corr')
TT_FA_batch4_error <- read_csv('FA_files/TT_FA_batch4_error')
TT_FA_batch5_error <- read_csv('FA_files/TT_FA_batch5_error')

# removing rows with no data
# removing R8 column, which has no data
TT_FA_b1_c <- TT_FA_batch1_corr %>%
  filter(seq != 0) %>%
  filter(cond != 0) %>%
  dplyr::select(- R8)

TT_FA_b2_c <- TT_FA_batch2_corr %>%
  filter(seq != 0) %>%
  filter(cond != 0) %>%
  dplyr::select(- R8)

TT_FA_b3_c <- TT_FA_batch3_corr %>%
  filter(seq != 0) %>%
  filter(cond != 0) %>%
  dplyr::select(- R8)

TT_FA_b4_e <- TT_FA_batch4_error %>%
  filter(seq != 0) %>%
  filter(cond != 0) %>%
  dplyr::select(- R8)

TT_FA_b5_e <- TT_FA_batch5_error %>%
  filter(seq != 0) %>%
  filter(cond != 0) %>%
  dplyr::select(- R8)

# reading in the file lists
FA_b1_lst <- read_csv('Batches/FA_batch1_corr.lst', col_names = FALSE)
FA_b2_lst <- read_csv('Batches/FA_batch2_corr.lst', col_names = FALSE)
FA_b3_lst <- read_csv('Batches/FA_batch3_corr.lst', col_names = FALSE)
FA_b4_lst <- read_csv('Batches/FA_batch4_error.lst', col_names = FALSE)
FA_b5_lst <- read_csv('Batches/FA_batch5_error.lst', col_names = FALSE)

# extract just participant numbers from file names
FA_b1_lst$X1 <- FA_b1_lst$X1 %>%
  str_sub(6, 7)

FA_b2_lst$X1 <- FA_b2_lst$X1 %>%
  str_sub(6, 7)

FA_b3_lst$X1 <- FA_b3_lst$X1 %>%
  str_sub(6, 7)

FA_b4_lst$X1 <- FA_b4_lst$X1 %>%
  str_sub(6, 7)

FA_b5_lst$X1 <- FA_b5_lst$X1 %>%
  str_sub(6, 7)

# replacing participant numbers in the data files with participant numbers
# from the batch files
# this didn't work properly
#for (i in unique(TT_FA_b1_c$subj)) {
#  print(i)
#  print(FA_b1_lst$X1[i])
#  TT_FA_b1_c <- TT_FA_b1_c %>% 
#    mutate(subj = replace(subj, subj == i, FA_b1_lst$X1[i]))
#}

# replacing participant numbers in the data files with participant numbers
# from the batch files
TT_FA_b1_c$subj <- mapvalues(TT_FA_b1_c$subj, unique(TT_FA_b1_c$subj), FA_b1_lst$X1)
TT_FA_b2_c$subj <- mapvalues(TT_FA_b2_c$subj, unique(TT_FA_b2_c$subj), FA_b2_lst$X1)
TT_FA_b3_c$subj <- mapvalues(TT_FA_b3_c$subj, unique(TT_FA_b3_c$subj), FA_b3_lst$X1)
TT_FA_b4_e$subj <- mapvalues(TT_FA_b4_e$subj, unique(TT_FA_b4_e$subj), FA_b4_lst$X1)
TT_FA_b5_e$subj <- mapvalues(TT_FA_b5_e$subj, unique(TT_FA_b5_e$subj), FA_b5_lst$X1)


# binding all together
TT_FA <- rbind(TT_FA_b1_c,
               TT_FA_b2_c,
               TT_FA_b3_c,
               TT_FA_b4_e,
               TT_FA_b5_e) %>%
  mutate(method = "FA")

# visualising missing data
vis_miss(TT_FA)

#----------------------------------

# reading in total time Eye Doctor files 
TT_ED_b1_c <- read_csv('ED_files/TT_ED_batch1_corr.csv')
TT_ED_b2_c <- read_csv('ED_files/TT_ED_batch2B_corr.csv')
TT_ED_b3_c <- read_csv('ED_files/TT_ED_batch3_corr.csv')
TT_ED_b4_e <- read_csv('ED_files/TT_ED_batch4_error.csv')
TT_ED_b5_e <- read_csv('ED_files/TT_ED_batch5_error.csv')

# reading in the file lists
ED_b1_lst <- read_csv('Batches/ED_batch1_corr.lst', col_names = FALSE)
ED_b2_lst <- read_csv('Batches/ED_batch2_corr.lst', col_names = FALSE)
ED_b3_lst <- read_csv('Batches/ED_batch3_corr.lst', col_names = FALSE)
ED_b4_lst <- read_csv('Batches/ED_batch4_error.lst', col_names = FALSE)
ED_b5_lst <- read_csv('Batches/ED_batch5_error.lst', col_names = FALSE)

# extract just participant numbers from file names
ED_b1_lst$X1 <- ED_b1_lst$X1 %>%
  str_sub(4, 5)

ED_b2_lst$X1 <- ED_b2_lst$X1 %>%
  str_sub(4, 5)

ED_b3_lst$X1 <- ED_b3_lst$X1 %>%
  str_sub(4, 5)

ED_b4_lst$X1 <- ED_b4_lst$X1 %>%
  str_sub(4, 5)

ED_b5_lst$X1 <- ED_b5_lst$X1 %>%
  str_sub(4, 5)

# replacing participant numbers in the data files with participant numbers
# from the batch files
TT_ED_b1_c$subj <- mapvalues(TT_ED_b1_c$subj, unique(TT_ED_b1_c$subj), ED_b1_lst$X1)
TT_ED_b2_c$subj <- mapvalues(TT_ED_b2_c$subj, unique(TT_ED_b2_c$subj), ED_b2_lst$X1)
TT_ED_b3_c$subj <- mapvalues(TT_ED_b3_c$subj, unique(TT_ED_b3_c$subj), ED_b3_lst$X1)
TT_ED_b4_e$subj <- mapvalues(TT_ED_b4_e$subj, unique(TT_ED_b4_e$subj), ED_b4_lst$X1)
TT_ED_b5_e$subj <- mapvalues(TT_ED_b5_e$subj, unique(TT_ED_b5_e$subj), ED_b5_lst$X1)

# Batch2 has an error message because the data has 19 rows but there are
# 20 replacement participant numbers
# Participant 52 was not included, so this is removed from the replacement list:
ED_b2_lst <- ED_b2_lst %>%
  filter(X1 != 52)

# the participant numbers are replaced using the same code as before
# but now with P52 removed from the 'to' dataframe
TT_ED_b2_c$subj <- mapvalues(TT_ED_b2_c$subj, unique(TT_ED_b2_c$subj), ED_b2_lst$X1)

# binding all together
TT_ED <- rbind(TT_ED_b1_c,
               TT_ED_b2_c,
               TT_ED_b3_c,
               TT_ED_b4_e,
               TT_ED_b5_e) %>%
  mutate(R7 = NA,
         method = "ED") 
  

#-----------------------------------------

n <- c(FA_b1_lst$X1, FA_b2_lst$X1, FA_b3_lst$X1, FA_b4_lst$X1, FA_b5_lst$X1)
FAnums <- data.frame(n)
duplicated(FAnums$n)
#none duplicated

# Participant 52 is not in the FA data
52 %in% FAnums$n

# entire total time dataset with both methods
TT <- rbind(TT_ED, TT_FA) 

TT$subj <- as.character(TT$subj)
str(TT$subj)

# filtering to include only participants in the fixalign dataset
TT <- TT %>% filter(TT$subj %in% FAnums$n)

# demonstrating there are 64 rows for each participant
# 32 items per participant x2 for the 2 different processing methods = 64
TT %>% ggplot(aes(subj)) +
  geom_bar()
count(TT$subj)

# plot showing total fixation times for the two methods
plot <- TT %>% 
  pivot_longer(cols = R1:R7, names_to = "regions", values_to = "values") %>%
  ggplot(aes(x = regions, 
             y = values,
             colour = method)) +
  geom_point(position = position_jitterdodge(),
             alpha = 0.1) +
  labs(y = "Total Fixation Time (ms)",
       x = NULL,
       colour = "Processing Method:") +
  scale_color_manual(labels = c("Manual", "Automatic"), 
                     values = c("darkblue", "orange")) +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size=10))) +
  theme_minimal(base_size = 20) +
  theme(legend.position="bottom")

plot

ggsave("plot1", plot, device = "png", path = "Batches",
       height = 4.5,
       width = 7)
  
#find NAs
TT %>% 
  group_by(method) %>%
  summarise_all(~ sum(is.na(.)))

TT %>% 
  group_by(method) %>%
  summarise_all(~ sum(!is.na(.)))

TT %>% 
  group_by(method) %>%
  summarise(R7 = sum(!is.na(R7)))

# mean and sd for both groups
TT %>%
  group_by(method) %>%
  summarise_at(vars(R1:R7), mean, na.rm = TRUE)

TT %>%
  group_by(method) %>%
  summarise_at(vars(R1:R7), sd, na.rm = TRUE)

TT %>%
  group_by(method) %>%
  summarise_at(vars(R1:R7), n, na.rm = TRUE)

# grand total of fixations
totals <- aggregate(R1 ~ method, data = TT, FUN = sum) %>%
  cbind("R2" = 
          aggregate(R2 ~ method, data = TT, FUN = sum)$R2,
        "R3" =
          aggregate(R3 ~ method, data = TT, FUN = sum)$R3,
        "R4" = 
          aggregate(R4 ~ method, data = TT, FUN = sum)$R4,
        "R5" = 
          aggregate(R5 ~ method, data = TT, FUN = sum)$R5,
        "R6" = 
          aggregate(R6 ~ method, data = TT, FUN = sum)$R6,
        "R7" = 
          c("0", aggregate(R7 ~ method, data = TT, FUN = sum)$R7))

# melting the dataframe to convert into long format
totals <- melt(totals, id = "method")

# changing value column to numeric
totals$value <- as.numeric(totals$value)

# visualising the different between totals for the two methods
totals %>% ggplot(aes(x = variable,
                       y = value,
                       colour = method)) +
  geom_point()

# pivoting to wider: 1 column for each method
totals <- totals %>%  pivot_wider(names_from = method, values_from = value)

# summing all fixations for each method
sum(totals$ED)
sum(totals$FA)

#calculate and visualise difference (subtraction) between the same observations
diffs <- TT %>% 
  dplyr::select(subj, item, cond, R1, R2, R3, R4, R5, R6, R7, method) %>%
  pivot_wider(names_from = method, 
              values_from = c(R1, R2, R3,R4, R5, R6, R7)) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate(
    R1diff = R1_ED - R1_FA,
    R2diff = R2_ED - R2_FA,
    R3diff = R3_ED - R3_FA,
    R4diff = R4_ED - R4_FA,
    R5diff = R5_ED - R5_FA,
    R6diff = R6_ED - R6_FA,
    R7diff = R7_ED - R7_FA) %>%
  dplyr::select(subj, item, cond, R1diff, R2diff, R3diff, R4diff, R5diff, R6diff, R7diff)

# mean of differences summarised
diffs %>%
  summarise_at(vars(R1diff:R7diff), mean, na.rm = TRUE)

# standard deviation of differences summarised
diffs %>%
  summarise_at(vars(R1diff:R7diff), sd, na.rm = TRUE)

# pivoting diffs df to longer
diffs_longer <- diffs %>%
  pivot_longer(cols = R1diff:R7diff, names_to = "regions", values_to = "values")

# plotting the differences - roughly symmetrical about 0
plot <- diffs_longer %>% ggplot(aes(x = regions,
                     y = values)) +
  geom_jitter(width = 0.1,
              alpha = 0.05) +
  labs(y = "Difference in \nTotal Fixation Time (ms)",
       x = NULL) +
  scale_x_discrete(labels= c("R1", "R2", "R3", "R4", "R5", "R6", "R7")) +
  theme_minimal(base_size = 20)

plot

ggsave("plot1", plot, device = "png", path = "Batches",
       height = 4.5,
       width = 7)


# ANALYSIS ####

# in the analysis I will use all available participants from the Eye Doctor process
# so rather than using the combined dataframe, I use the separate dataframes
length(unique(TT_ED$subj))
length(unique(TT_FA$subj))


# EYE DOCTOR ANALYSIS ####

# TOTAL TIME #
# using na.omit also introduces additional info (location of omitted values)
# (see https://statisticsglobe.com/na-omit-r-example/ )
# this is removed using as.numeric()
descdist(as.numeric(na.omit(TT_ED$R4)))
# looks closest to a gamma distribution
descdist(as.numeric(na.omit(TT_ED$R5)))
# this is closest to lognormal but will use gamma

# FIRST PASS #
 
# reading in the files
# reading in total time fix_align files
FP_ED_b1_c <- read_csv('ED_files/FP_ED_batch1_corr.csv')
FP_ED_b2_c <- read_csv('ED_files/FP_ED_batch2B_corr.csv')
FP_ED_b3_c <- read_csv('ED_files/FP_ED_batch3_corr.csv')
FP_ED_b4_e <- read_csv('ED_files/FP_ED_batch4_error.csv')
FP_ED_b5_e <- read_csv('ED_files/FP_ED_batch5_error.csv')

# replacing participant numbers with numbers from participant lists
FP_ED_b1_c$subj <- mapvalues(FP_ED_b1_c$subj, unique(FP_ED_b1_c$subj), ED_b1_lst$X1)
FP_ED_b2_c$subj <- mapvalues(FP_ED_b2_c$subj, unique(FP_ED_b2_c$subj), ED_b2_lst$X1)

# Batch2 has an error message because the data has 19 rows but there are
# 20 replacement participant numbers
# Participant 52 was not included, so this is removed from the replacement list:
ED_b2_lst <- ED_b2_lst %>%
  filter(X1 != 52)

# the participant numbers are replaced using the same code as before
# but now with P52 removed from the 'to' dataframe
FP_ED_b2_c$subj <- mapvalues(FP_ED_b2_c$subj, unique(FP_ED_b2_c$subj), ED_b2_lst$X1)

FP_ED_b3_c$subj <- mapvalues(FP_ED_b3_c$subj, unique(FP_ED_b3_c$subj), ED_b3_lst$X1)
FP_ED_b4_e$subj <- mapvalues(FP_ED_b4_e$subj, unique(FP_ED_b4_e$subj), ED_b4_lst$X1)
FP_ED_b5_e$subj <- mapvalues(FP_ED_b5_e$subj, unique(FP_ED_b5_e$subj), ED_b5_lst$X1)

# binding all together
FP_ED <- rbind(FP_ED_b1_c,
               FP_ED_b2_c,
               FP_ED_b3_c,
               FP_ED_b4_e,
               FP_ED_b5_e) 


descdist(as.numeric(na.omit(FP_ED$R4)))
# looks closest to a gamma distribution
descdist(as.numeric(na.omit(FP_ED$R5)))
# this is closest to lognormal but will use gamma

# REGRESSION PATH #
  
# reading in the files
# reading in total time fix_align files
RP_ED_b1_c <- read_csv('ED_files/RP_ED_batch1_corr.csv')
RP_ED_b2_c <- read_csv('ED_files/RP_ED_batch2B_corr.csv')
RP_ED_b3_c <- read_csv('ED_files/RP_ED_batch3_corr.csv')
RP_ED_b4_e <- read_csv('ED_files/RP_ED_batch4_error.csv')
RP_ED_b5_e <- read_csv('ED_files/RP_ED_batch5_error.csv')

# replacing participant numbers with numbers from participant lists
RP_ED_b1_c$subj <- mapvalues(RP_ED_b1_c$subj, unique(RP_ED_b1_c$subj), ED_b1_lst$X1)
RP_ED_b2_c$subj <- mapvalues(RP_ED_b2_c$subj, unique(RP_ED_b2_c$subj), ED_b2_lst$X1)

# Batch2 has an error message because the data has 19 rows but there are
# 20 replacement participant numbers
# Participant 52 was not included, so this is removed from the replacement list:
ED_b2_lst <- ED_b2_lst %>%
  filter(X1 != 52)

# the participant numbers are replaced using the same code as before
# but now with P52 removed from the 'to' dataframe
RP_ED_b2_c$subj <- mapvalues(RP_ED_b2_c$subj, unique(RP_ED_b2_c$subj), ED_b2_lst$X1)

RP_ED_b3_c$subj <- mapvalues(RP_ED_b3_c$subj, unique(RP_ED_b3_c$subj), ED_b3_lst$X1)
RP_ED_b4_e$subj <- mapvalues(RP_ED_b4_e$subj, unique(RP_ED_b4_e$subj), ED_b4_lst$X1)
RP_ED_b5_e$subj <- mapvalues(RP_ED_b5_e$subj, unique(RP_ED_b5_e$subj), ED_b5_lst$X1)

# binding all together
RP_ED <- rbind(RP_ED_b1_c,
               RP_ED_b2_c,
               RP_ED_b3_c,
               RP_ED_b4_e,
               RP_ED_b5_e) 

descdist(as.numeric(na.omit(RP_ED$R4)))
# looks closest to a gamma distribution
descdist(as.numeric(na.omit(RP_ED$R5)))
# this is closest to lognormal but will use gamma

# FIXALIGN ANALYSIS ####

# TOTAL TIME #
descdist(as.numeric(na.omit(TT_FA$R4)))
# looks closest to a gamma distribution
descdist(as.numeric(na.omit(TT_FA$R5)))
# this is closest to lognormal but will use gamma

# FIRST PASS #

# reading in total time fix_align files
FP_FA_batch1_corr <- read_csv('FA_files/FP_FA_batch1_corr')
FP_FA_batch2_corr <- read_csv('FA_files/FP_FA_batch2_corr')
FP_FA_batch3_corr <- read_csv('FA_files/FP_FA_batch3_corr')
FP_FA_batch4_error <- read_csv('FA_files/FP_FA_batch4_error')
FP_FA_batch5_error <- read_csv('FA_files/FP_FA_batch5_error')

# removing rows with no data
# removing R8 column, which has no data
FP_FA_b1_c <- FP_FA_batch1_corr %>%
  filter(seq != 0) %>%
  filter(cond != 0) %>%
  dplyr::select(- R8)

FP_FA_b2_c <- FP_FA_batch2_corr %>%
  filter(seq != 0) %>%
  filter(cond != 0) %>%
  dplyr::select(- R8)

FP_FA_b3_c <- FP_FA_batch3_corr %>%
  filter(seq != 0) %>%
  filter(cond != 0) %>%
  dplyr::select(- R8)

FP_FA_b4_e <- FP_FA_batch4_error %>%
  filter(seq != 0) %>%
  filter(cond != 0) %>%
  dplyr::select(- R8)

FP_FA_b5_e <- FP_FA_batch5_error %>%
  filter(seq != 0) %>%
  filter(cond != 0) %>%
  dplyr::select(- R8)

# replacing participant numbers in the data files with participant numbers
# from the batch files
FP_FA_b1_c$subj <- mapvalues(FP_FA_b1_c$subj, unique(FP_FA_b1_c$subj), FA_b1_lst$X1)
FP_FA_b2_c$subj <- mapvalues(FP_FA_b2_c$subj, unique(FP_FA_b2_c$subj), FA_b2_lst$X1)
FP_FA_b3_c$subj <- mapvalues(FP_FA_b3_c$subj, unique(FP_FA_b3_c$subj), FA_b3_lst$X1)
FP_FA_b4_e$subj <- mapvalues(FP_FA_b4_e$subj, unique(FP_FA_b4_e$subj), FA_b4_lst$X1)
FP_FA_b5_e$subj <- mapvalues(FP_FA_b5_e$subj, unique(FP_FA_b5_e$subj), FA_b5_lst$X1)

# binding all together
FP_FA <- rbind(FP_FA_b1_c,
               FP_FA_b2_c,
               FP_FA_b3_c,
               FP_FA_b4_e,
               FP_FA_b5_e)

descdist(as.numeric(na.omit(FP_FA$R4)))
# looks closest to a gamma distribution
descdist(as.numeric(na.omit(FP_FA$R5)))
# this is closest to lognormal but will use gamma

# REGRESSION PATH #

# reading in total time fix_align files
RP_FA_batch1_corr <- read_csv('FA_files/RP_FA_batch1_corr')
RP_FA_batch2_corr <- read_csv('FA_files/RP_FA_batch2_corr')
RP_FA_batch3_corr <- read_csv('FA_files/RP_FA_batch3_corr')
RP_FA_batch4_error <- read_csv('FA_files/RP_FA_batch4_error')
RP_FA_batch5_error <- read_csv('FA_files/RP_FA_batch5_error')

# removing rows with no data
# removing R8 column, which has no data
RP_FA_b1_c <- RP_FA_batch1_corr %>%
  filter(seq != 0) %>%
  filter(cond != 0) %>%
  dplyr::select(- R8)

RP_FA_b2_c <- RP_FA_batch2_corr %>%
  filter(seq != 0) %>%
  filter(cond != 0) %>%
  dplyr::select(- R8)

RP_FA_b3_c <- RP_FA_batch3_corr %>%
  filter(seq != 0) %>%
  filter(cond != 0) %>%
  dplyr::select(- R8)

RP_FA_b4_e <- RP_FA_batch4_error %>%
  filter(seq != 0) %>%
  filter(cond != 0) %>%
  dplyr::select(- R8)

RP_FA_b5_e <- RP_FA_batch5_error %>%
  filter(seq != 0) %>%
  filter(cond != 0) %>%
  dplyr::select(- R8)

# replacing participant numbers in the data files with participant numbers
# from the batch files
RP_FA_b1_c$subj <- mapvalues(RP_FA_b1_c$subj, unique(RP_FA_b1_c$subj), FA_b1_lst$X1)
RP_FA_b2_c$subj <- mapvalues(RP_FA_b2_c$subj, unique(RP_FA_b2_c$subj), FA_b2_lst$X1)
RP_FA_b3_c$subj <- mapvalues(RP_FA_b3_c$subj, unique(RP_FA_b3_c$subj), FA_b3_lst$X1)
RP_FA_b4_e$subj <- mapvalues(RP_FA_b4_e$subj, unique(RP_FA_b4_e$subj), FA_b4_lst$X1)
RP_FA_b5_e$subj <- mapvalues(RP_FA_b5_e$subj, unique(RP_FA_b5_e$subj), FA_b5_lst$X1)

# binding all together
RP_FA <- rbind(RP_FA_b1_c,
               RP_FA_b2_c,
               RP_FA_b3_c,
               RP_FA_b4_e,
               RP_FA_b5_e)

descdist(as.numeric(na.omit(RP_FA$R4)))
# looks closest to a gamma distribution
descdist(as.numeric(na.omit(RP_FA$R5)))
# this is closest to lognormal but will use gamma


# MODEL-BUILDING LOOPS ####

# listing all 6 dataframes
datasets <- list(TT_ED, FP_ED, RP_ED, TT_FA, FP_FA, RP_FA) 

# names of all 6 datasets
dataset_names <- c("TT_ED", "FP_ED", "RP_ED", "TT_FA", "FP_FA", "RP_FA")

# creating models df from these two vectors
models <- as.data.frame(cbind(datasets, dataset_names))


# in the script 'Questions 01_10_18' 
# condition 1 is where prediction is facilitated
# condition 2 is where prediction is unfacilitated
# (for further verification: the error in item 16 occurred on the unfacilitated condition,
# which was condition 2)
for (i in 1:nrow(models)) {
  models[[i, 1]]$cond <- recode(models[[i, 1]]$cond, `1` = "Facilitated", `2` = "Unfacilitated")
  models[[i, 1]]$cond <- as.factor(models[[i, 1]]$cond)
  models[[i, 1]]$subj <- as.character(models[[i, 1]]$subj)
  models[[i, 1]]$item <- as.character(models[[i, 1]]$item)
}


# creating blank results dataframe
results_table_R4 <- data.frame()

# loop for R4 models
for (i in 1:nrow(models)) {
  model_name <- paste0(models[[i, 2]], "_R4") # generate the model name
  LMM <- glmer(R4 ~ cond + (1 | subj) + (1 | item), 
               family = Gamma(link = "log"), 
               data = models[[i, 1]]) # build the model
  assign(model_name, LMM) # assign the model with the model name
  
  # add the results to the results table
  results_table_R4 <- tidy(LMM) %>%
    filter(group == "fixed",
           term != "(Intercept)") %>%
    cbind(Model = rep(model_name)) %>%
    dplyr::select(- group) %>%
    rbind(results_table_R4) %>%
    arrange(Model)
}

# creating blank results dataframe
results_table_R5 <- data.frame()

# loop for building models (like before)
for (i in 1:nrow(models)) {
  model_name <- paste0(models[[i, 2]], "_R5")
  LMM <- glmer(R5 ~ cond + (1 | subj) + (1 | item), 
               family = Gamma(link = "log"), 
               data = models[[i, 1]])
  assign(model_name, LMM)
  
  results_table_R5 <- tidy(LMM) %>%
    filter(group == "fixed",
           term != "(Intercept)") %>%
    cbind(Model = rep(model_name)) %>%
    dplyr::select(- group) %>%
    rbind(results_table_R5) %>%
    arrange(Model)
}

# rounding numbers in the model tables
results_table_R4$estimate <- round(results_table_R4$estimate, 2)
results_table_R4$std.error <- round(results_table_R4$std.error, 2)
results_table_R4$statistic <- round(results_table_R4$statistic, 2)
results_table_R4$p.value <- round(results_table_R4$p.value, 3)

results_table_R5$estimate <- round(results_table_R5$estimate, 2)
results_table_R5$std.error <- round(results_table_R5$std.error, 2)
results_table_R5$statistic <- round(results_table_R5$statistic, 2)
results_table_R5$p.value <- round(results_table_R5$p.value, 3)

# writing table as a csv 
write_csv(results_table_R4, file.path("results_table_R4"))
write_csv(results_table_R5, file.path("results_table_R5"))

# loop for descriptive statistics (means)
stats_table_R4 <- data.frame()
stats_table_R5 <- data.frame()

for (i in 1:nrow(models)) {
  
  stats_table_R4 <- models[[i, 1]] %>%
    group_by(cond) %>% 
    dplyr::summarise(mean_R4 = mean(R4, na.rm = TRUE)) %>%
    t() %>%
    row_to_names(1) %>%
    data.frame() %>%
    cbind(dataset = models[[i, 2]]) %>%
    rbind(stats_table_R4) %>%
    arrange(dataset)
  
  stats_table_R5 <- models[[i, 1]] %>%
    group_by(cond) %>% 
    dplyr::summarise(mean_R5 = mean(R5, na.rm = TRUE)) %>%
    t() %>%
    row_to_names(1) %>%
    data.frame() %>%
    cbind(dataset = models[[i, 2]]) %>%
    rbind(stats_table_R5) %>%
    arrange(dataset)
    
}

# converting to double
stats_table_R4$Facilitated <- as.double(stats_table_R4$Facilitated)
stats_table_R4$Unfacilitated <- as.double(stats_table_R4$Unfacilitated)

stats_table_R5$Facilitated <- as.double(stats_table_R5$Facilitated)
stats_table_R5$Unfacilitated <- as.double(stats_table_R5$Unfacilitated)

# USING BUILDMER ####
# to determine the maximal structure for each model

# R4 loop
for (i in 1:nrow(models)) {
  maximal <- buildmer(R4 ~ cond + (1 + cond | subj) + (1 + cond | item), 
                      family = Gamma(link = "log"), 
                      data = models[[i, 1]])
  fm <- formula(maximal)
  resultsList <- append(resultsList, fm)
}

# R5 loop
for (i in 1:nrow(models)) {
  maximal <- buildmer(R5 ~ cond + (1 + cond | subj) + (1 + cond | item), 
                      family = Gamma(link = "log"), 
                      data = models[[i, 1]])
  fm <- formula(maximal)
  resultsList <- append(resultsList, fm)
}

