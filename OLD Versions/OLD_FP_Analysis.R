# Load packages
library(Matrix)
library(lme4)
library(lmerTest)
library(emmeans)
library(stats)
library(brms)
library(fitdistrplus)
library(tidyverse)
library(buildmer)
library(performance)
library(see)

# Analysis of First Pass data

#import the data set batch 1
#Load in the data sets
library(readr)
FP_ED_batch_corr <- read_csv("First_pass/FP_ED/FP_ED_batch_corr.csv")
FP_ED_batch_error <- read_csv("First_pass/FP_ED/FP_ED_batch_error.csv")

#Rename the participant numbers in the batches back to their original participant numbers.
FP_ED_batch_corr$subj[FP_ED_batch_corr$subj == 54] <-"84"
FP_ED_batch_corr$subj[FP_ED_batch_corr$subj == 53] <-"83"
FP_ED_batch_corr$subj[FP_ED_batch_corr$subj == 52] <-"82"
FP_ED_batch_corr$subj[FP_ED_batch_corr$subj == 51] <-"81"
FP_ED_batch_corr$subj[FP_ED_batch_corr$subj == 50] <-"80"
FP_ED_batch_corr$subj[FP_ED_batch_corr$subj == 49] <-"79"
FP_ED_batch_corr$subj[FP_ED_batch_corr$subj == 48] <-"78"
FP_ED_batch_corr$subj[FP_ED_batch_corr$subj == 47] <-"77"
FP_ED_batch_corr$subj[FP_ED_batch_corr$subj == 46] <-"76"
FP_ED_batch_corr$subj[FP_ED_batch_corr$subj == 45] <-"75"
FP_ED_batch_corr$subj[FP_ED_batch_corr$subj == 44] <-"74"
FP_ED_batch_corr$subj[FP_ED_batch_corr$subj == 43] <-"73"
FP_ED_batch_corr$subj[FP_ED_batch_corr$subj == 42] <-"72"
FP_ED_batch_corr$subj[FP_ED_batch_corr$subj == 41] <-"71"
FP_ED_batch_corr$subj[FP_ED_batch_corr$subj == 40] <-"70"
FP_ED_batch_corr$subj[FP_ED_batch_corr$subj == 39] <-"69"
FP_ED_batch_corr$subj[FP_ED_batch_corr$subj == 38] <-"68"
FP_ED_batch_corr$subj[FP_ED_batch_corr$subj == 37] <-"67"
FP_ED_batch_corr$subj[FP_ED_batch_corr$subj == 36] <-"66"
FP_ED_batch_corr$subj[FP_ED_batch_corr$subj == 35] <-"64"
FP_ED_batch_corr$subj[FP_ED_batch_corr$subj == 34] <-"62"
FP_ED_batch_corr$subj[FP_ED_batch_corr$subj == 33] <-"60"
FP_ED_batch_corr$subj[FP_ED_batch_corr$subj == 32] <-"58"
FP_ED_batch_corr$subj[FP_ED_batch_corr$subj == 31] <-"56"
FP_ED_batch_corr$subj[FP_ED_batch_corr$subj == 30] <-"54"
FP_ED_batch_corr$subj[FP_ED_batch_corr$subj == 29] <-"52"
FP_ED_batch_corr$subj[FP_ED_batch_corr$subj == 28] <-"50"
FP_ED_batch_corr$subj[FP_ED_batch_corr$subj == 27] <-"48"
FP_ED_batch_corr$subj[FP_ED_batch_corr$subj == 26] <-"46"
FP_ED_batch_corr$subj[FP_ED_batch_corr$subj == 25] <-"44"
FP_ED_batch_corr$subj[FP_ED_batch_corr$subj == 24] <-"43"
FP_ED_batch_corr$subj[FP_ED_batch_corr$subj == 23] <-"42"
FP_ED_batch_corr$subj[FP_ED_batch_corr$subj == 22] <-"40"
FP_ED_batch_corr$subj[FP_ED_batch_corr$subj == 21] <-"39"
FP_ED_batch_corr$subj[FP_ED_batch_corr$subj == 20] <-"38"
FP_ED_batch_corr$subj[FP_ED_batch_corr$subj == 19] <-"36"
FP_ED_batch_corr$subj[FP_ED_batch_corr$subj == 18] <-"35"
FP_ED_batch_corr$subj[FP_ED_batch_corr$subj == 17] <-"34"
FP_ED_batch_corr$subj[FP_ED_batch_corr$subj == 16] <-"32"
FP_ED_batch_corr$subj[FP_ED_batch_corr$subj == 15] <-"30"
FP_ED_batch_corr$subj[FP_ED_batch_corr$subj == 14] <-"28"
FP_ED_batch_corr$subj[FP_ED_batch_corr$subj == 13] <-"26"
FP_ED_batch_corr$subj[FP_ED_batch_corr$subj == 12] <-"24"
FP_ED_batch_corr$subj[FP_ED_batch_corr$subj == 11] <-"22"
FP_ED_batch_corr$subj[FP_ED_batch_corr$subj == 10] <-"20"
FP_ED_batch_corr$subj[FP_ED_batch_corr$subj == 9] <-"18"
FP_ED_batch_corr$subj[FP_ED_batch_corr$subj == 8] <-"16"
FP_ED_batch_corr$subj[FP_ED_batch_corr$subj == 7] <-"14"
FP_ED_batch_corr$subj[FP_ED_batch_corr$subj == 6] <-"12"
FP_ED_batch_corr$subj[FP_ED_batch_corr$subj == 5] <-"10"
FP_ED_batch_corr$subj[FP_ED_batch_corr$subj == 4] <-"8"
FP_ED_batch_corr$subj[FP_ED_batch_corr$subj == 3] <-"6"
FP_ED_batch_corr$subj[FP_ED_batch_corr$subj == 2] <-"4"
FP_ED_batch_corr$subj[FP_ED_batch_corr$subj == 1] <-"2"
FP_ED_batch_error$subj[FP_ED_batch_error$subj == 31] <-"65"
FP_ED_batch_error$subj[FP_ED_batch_error$subj == 30] <-"63"
FP_ED_batch_error$subj[FP_ED_batch_error$subj == 29] <-"61"
FP_ED_batch_error$subj[FP_ED_batch_error$subj == 28] <-"59"
FP_ED_batch_error$subj[FP_ED_batch_error$subj == 27] <-"57"
FP_ED_batch_error$subj[FP_ED_batch_error$subj == 26] <-"55"
FP_ED_batch_error$subj[FP_ED_batch_error$subj == 25] <-"53"
FP_ED_batch_error$subj[FP_ED_batch_error$subj == 24] <-"51"
FP_ED_batch_error$subj[FP_ED_batch_error$subj == 23] <-"49"
FP_ED_batch_error$subj[FP_ED_batch_error$subj == 22] <-"47"
FP_ED_batch_error$subj[FP_ED_batch_error$subj == 21] <-"45"
FP_ED_batch_error$subj[FP_ED_batch_error$subj == 20] <-"41"
FP_ED_batch_error$subj[FP_ED_batch_error$subj == 19] <-"37"
FP_ED_batch_error$subj[FP_ED_batch_error$subj == 18] <-"35"
FP_ED_batch_error$subj[FP_ED_batch_error$subj == 17] <-"33"
FP_ED_batch_error$subj[FP_ED_batch_error$subj == 16] <-"31"
FP_ED_batch_error$subj[FP_ED_batch_error$subj == 15] <-"29"
FP_ED_batch_error$subj[FP_ED_batch_error$subj == 14] <-"27"
FP_ED_batch_error$subj[FP_ED_batch_error$subj == 13] <-"25"
FP_ED_batch_error$subj[FP_ED_batch_error$subj == 12] <-"23"
FP_ED_batch_error$subj[FP_ED_batch_error$subj == 11] <-"21"
FP_ED_batch_error$subj[FP_ED_batch_error$subj == 10] <-"19"
FP_ED_batch_error$subj[FP_ED_batch_error$subj == 9] <-"17"
FP_ED_batch_error$subj[FP_ED_batch_error$subj == 8] <-"15"
FP_ED_batch_error$subj[FP_ED_batch_error$subj == 7] <-"13"
FP_ED_batch_error$subj[FP_ED_batch_error$subj == 6] <-"11"
FP_ED_batch_error$subj[FP_ED_batch_error$subj == 5] <-"9"
FP_ED_batch_error$subj[FP_ED_batch_error$subj == 4] <-"7"
FP_ED_batch_error$subj[FP_ED_batch_error$subj == 3] <-"5"
FP_ED_batch_error$subj[FP_ED_batch_error$subj == 2] <-"3"

#Check the subject numbers have been redefined correctly
#View(FP_ED_batch_corr)
#View(FP_ED_batch_error)

#Let's combine the data
all_data <- rbind(FP_ED_batch_corr, FP_ED_batch_error)

#make subj a factor
all_data$subj <- as.factor(all_data$subj)

#Import Individual difference measures
All_IDs <- read_csv("All_IDs.csv")
#View(All_IDs)

# Rename Participabt in ID_measures to subj to be the same as current data set
All_IDs <- rename(All_IDs, subj = Participant)
All_IDs$subj <- as.factor(All_IDs$subj)

# Add the ID's to the data frame
all_data_join <- inner_join(all_data, All_IDs, by = "subj")
#view(all_data_join)

# Assign condition labels, 1 = prediction facilitated, 2 = prediction unfacilitated
#(this will make it easier to interpret)
all_data_join$cond <- recode(all_data_join$cond, "1" = "facilitated", "2" = "unfacilitated")

#Create a new CSV file with all the combined variables for future analysis
#write.csv(all_data_join,"C:\\Users\\elizabethle-luan\\Desktop\\Prediction Study 1\\all_data_join.csv", row.names = TRUE)
#view(all_data_join)
#C_Users_elizabethle_luan_Desktop_Prediction_Study_1_all_data_join <- read_csv("C:\\Users\\elizabethle-luan\\Desktop\\Prediction Study 1\\all_data_join.csv")
#View(C_Users_elizabethle_luan_Desktop_Prediction_Study_1_all_data_join)

#Let's have a look at region 4

#set condition as a factor
all_data_join$cond <- as.factor(all_data_join$cond)
# Throw away zeroes
all_data_join <- all_data_join %>% filter(R4 != 0)

#Visualisation
all_data_join %>% 
  ggplot(aes(x = cond, y = R4, colour = cond)) + ggtitle("First Pass for Critical Region: Question") +
  labs(y = "Reading time in ms.", x = "Prediction") +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(colour = FALSE)

#Descriptives
all_data_join %>% 
  group_by(cond) %>%
  summarise(mean(R4), sd(R4))

# Model assuming normality of residuals maximal structure
model.nullR4 <- lmer(R4 ~ (1 + cond | subj) + (1 + cond | item), all_data_join) 
modelR4 <- lmer(R4 ~ cond + (1 + cond | subj) + (1 + cond | item), all_data_join) 
summary(modelR4)

anova(modelR4, model.nullR4)

#All the data for this model looks pretty normal.
check_model(modelR4)
qqnorm(residuals(modelR4))
qqline(residuals(modelR4))
descdist(all_data_join$R4)

#Let's include some co-variates! Region 4

#Step 1: Scale the ID measures...
all_data_join$SRS_total_score_t <- scale(all_data_join$SRS_total_score_t)
all_data_join$EQ <- scale(all_data_join$EQ)
all_data_join$Total_reading_cluster <- scale(all_data_join$Total_reading_cluster)
all_data_join$Total_RAN <- scale(all_data_join$Total_RAN)
all_data_join$"WI _RPI" <- scale(all_data_join$"WI _RPI")

# Model including covariates
model_alldatacov_R4 <- lmer(R4 ~ cond + SRS_total_score_t + EQ + Total_reading_cluster + Total_RAN +
                                (1 + cond | subj) +  (1 + cond | item) , data = all_data_join, REML = TRUE)

model_alldatacov_R4_null <- lmer(R4 ~ SRS_total_score_t + EQ + Total_reading_cluster + Total_RAN +
                               (1 + cond | subj) +  (1 + cond | item) , data = all_data_join, REML = TRUE)

summary(model_alldatacov_R4)
anova(model_alldatacov_R4_null, model_alldatacov_R4)
check_model(model_alldatacov_R4)

#Remove other variables that do not affect the data just for exploratory purpose
model_alldatacov_R42 <- lmer(R4 ~ cond + Total_RAN +
                              (1 + cond | subj) +  (1 + cond | item), data = all_data_join, REML = TRUE)

summary(model_alldatacov_R42)

# Error in anova.merMod(modelR4, model_alldatacov_R4): models were not all fitted to the same size of dataset
#How to get around this?
anova(modelR4, model_alldatacov_R4)

ranef(model_alldatacov_R4)

#Let's have a look at region 5

#set condition as a factor
all_data_join$cond <- as.factor(all_data_join$cond)
# Throw away zeroes
all_data_join <- all_data_join %>% filter(R5 != 0)

#Visualisation
all_data_join %>% 
  ggplot(aes(x = cond, y = R5, colour = cond)) + ggtitle("First Pass for Post-Critical Region: Reply") +
  labs(y = "Reading time in ms.", x = "Prediction") +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(colour = FALSE)

#Descriptives
all_data_join %>% 
  group_by(cond) %>%
  summarise(mean(R5), sd(R5))

# Model assuming normality of residuals maximal structure
model.nullR5 <- lmer(R5 ~ (1 + cond | subj) + (1 + cond | item), all_data_join) 
modelR5 <- lmer(R5 ~ cond + (1 + cond | subj) + (1 + cond | item), all_data_join) 
summary(modelR5)

anova(modelR5, model.nullR5)

#All the data for this model looks pretty normal.
check_model(modelR5)
qqnorm(residuals(modelR5))
qqline(residuals(modelR5))
descdist(all_data_join$R5)

#Let's include some co-variates! Region 5

#Step 1: Scale the ID measures...
all_data_join$SRS_total_score_t <- scale(all_data_join$SRS_total_score_t)
all_data_join$EQ <- scale(all_data_join$EQ)
all_data_join$Total_reading_cluster <- scale(all_data_join$Total_reading_cluster)
all_data_join$Total_RAN <- scale(all_data_join$Total_RAN)
all_data_join$"WI _RPI" <- scale(all_data_join$"WI _RPI")

# Model including covariates
model_alldatacov_R5 <- lmer(R5 ~ cond + SRS_total_score_t + EQ + Total_reading_cluster + Total_RAN +
                              (1 + cond | subj) +  (1 + cond | item) , data = all_data_join, REML = TRUE)

model_alldatacov_R5_null <- lmer(R5 ~ SRS_total_score_t + EQ + Total_reading_cluster + Total_RAN +
                                   (1 + cond | subj) +  (1 + cond | item) , data = all_data_join, REML = TRUE)

summary(model_alldatacov_R5)
anova(model_alldatacov_R5_null, model_alldatacov_R5)
check_model(model_alldatacov_R5)

# Error in anova.merMod(modelR5, model_alldatacov_R5): models were not all fitted to the same size of dataset
#How to get around this?
anova(modelR5, model_alldatacov_R5)

ranef(model_alldatacov_R5)
