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
RI_ED_batch_corr <- read_csv("EyeDry Analysis/Regressions_In/RI_ED/RI_ED_batch_corr.csv")
RI_ED_batch_error <- read_csv("EyeDry Analysis/Regressions_In/RI_ED/RI_ED_batch_error.csv")

#Rename the participant numbers in the batches back to their original participant numbers.
RI_ED_batch_corr$subj[RI_ED_batch_corr$subj == 54] <-"84"
RI_ED_batch_corr$subj[RI_ED_batch_corr$subj == 53] <-"83"
RI_ED_batch_corr$subj[RI_ED_batch_corr$subj == 52] <-"82"
RI_ED_batch_corr$subj[RI_ED_batch_corr$subj == 51] <-"81"
RI_ED_batch_corr$subj[RI_ED_batch_corr$subj == 50] <-"80"
RI_ED_batch_corr$subj[RI_ED_batch_corr$subj == 49] <-"79"
RI_ED_batch_corr$subj[RI_ED_batch_corr$subj == 48] <-"78"
RI_ED_batch_corr$subj[RI_ED_batch_corr$subj == 47] <-"77"
RI_ED_batch_corr$subj[RI_ED_batch_corr$subj == 46] <-"76"
RI_ED_batch_corr$subj[RI_ED_batch_corr$subj == 45] <-"75"
RI_ED_batch_corr$subj[RI_ED_batch_corr$subj == 44] <-"74"
RI_ED_batch_corr$subj[RI_ED_batch_corr$subj == 43] <-"73"
RI_ED_batch_corr$subj[RI_ED_batch_corr$subj == 42] <-"72"
RI_ED_batch_corr$subj[RI_ED_batch_corr$subj == 41] <-"71"
RI_ED_batch_corr$subj[RI_ED_batch_corr$subj == 40] <-"70"
RI_ED_batch_corr$subj[RI_ED_batch_corr$subj == 39] <-"69"
RI_ED_batch_corr$subj[RI_ED_batch_corr$subj == 38] <-"68"
RI_ED_batch_corr$subj[RI_ED_batch_corr$subj == 37] <-"67"
RI_ED_batch_corr$subj[RI_ED_batch_corr$subj == 36] <-"66"
RI_ED_batch_corr$subj[RI_ED_batch_corr$subj == 35] <-"64"
RI_ED_batch_corr$subj[RI_ED_batch_corr$subj == 34] <-"62"
RI_ED_batch_corr$subj[RI_ED_batch_corr$subj == 33] <-"60"
RI_ED_batch_corr$subj[RI_ED_batch_corr$subj == 32] <-"58"
RI_ED_batch_corr$subj[RI_ED_batch_corr$subj == 31] <-"56"
RI_ED_batch_corr$subj[RI_ED_batch_corr$subj == 30] <-"54"
RI_ED_batch_corr$subj[RI_ED_batch_corr$subj == 29] <-"52"
RI_ED_batch_corr$subj[RI_ED_batch_corr$subj == 28] <-"50"
RI_ED_batch_corr$subj[RI_ED_batch_corr$subj == 27] <-"48"
RI_ED_batch_corr$subj[RI_ED_batch_corr$subj == 26] <-"46"
RI_ED_batch_corr$subj[RI_ED_batch_corr$subj == 25] <-"44"
RI_ED_batch_corr$subj[RI_ED_batch_corr$subj == 24] <-"43"
RI_ED_batch_corr$subj[RI_ED_batch_corr$subj == 23] <-"42"
RI_ED_batch_corr$subj[RI_ED_batch_corr$subj == 22] <-"40"
RI_ED_batch_corr$subj[RI_ED_batch_corr$subj == 21] <-"39"
RI_ED_batch_corr$subj[RI_ED_batch_corr$subj == 20] <-"38"
RI_ED_batch_corr$subj[RI_ED_batch_corr$subj == 19] <-"36"
RI_ED_batch_corr$subj[RI_ED_batch_corr$subj == 18] <-"35"
RI_ED_batch_corr$subj[RI_ED_batch_corr$subj == 17] <-"34"
RI_ED_batch_corr$subj[RI_ED_batch_corr$subj == 16] <-"32"
RI_ED_batch_corr$subj[RI_ED_batch_corr$subj == 15] <-"30"
RI_ED_batch_corr$subj[RI_ED_batch_corr$subj == 14] <-"28"
RI_ED_batch_corr$subj[RI_ED_batch_corr$subj == 13] <-"26"
RI_ED_batch_corr$subj[RI_ED_batch_corr$subj == 12] <-"24"
RI_ED_batch_corr$subj[RI_ED_batch_corr$subj == 11] <-"22"
RI_ED_batch_corr$subj[RI_ED_batch_corr$subj == 10] <-"20"
RI_ED_batch_corr$subj[RI_ED_batch_corr$subj == 9] <-"18"
RI_ED_batch_corr$subj[RI_ED_batch_corr$subj == 8] <-"16"
RI_ED_batch_corr$subj[RI_ED_batch_corr$subj == 7] <-"14"
RI_ED_batch_corr$subj[RI_ED_batch_corr$subj == 6] <-"12"
RI_ED_batch_corr$subj[RI_ED_batch_corr$subj == 5] <-"10"
RI_ED_batch_corr$subj[RI_ED_batch_corr$subj == 4] <-"8"
RI_ED_batch_corr$subj[RI_ED_batch_corr$subj == 3] <-"6"
RI_ED_batch_corr$subj[RI_ED_batch_corr$subj == 2] <-"4"
RI_ED_batch_corr$subj[RI_ED_batch_corr$subj == 1] <-"2"
RI_ED_batch_error$subj[RI_ED_batch_error$subj == 31] <-"65"
RI_ED_batch_error$subj[RI_ED_batch_error$subj == 30] <-"63"
RI_ED_batch_error$subj[RI_ED_batch_error$subj == 29] <-"61"
RI_ED_batch_error$subj[RI_ED_batch_error$subj == 28] <-"59"
RI_ED_batch_error$subj[RI_ED_batch_error$subj == 27] <-"57"
RI_ED_batch_error$subj[RI_ED_batch_error$subj == 26] <-"55"
RI_ED_batch_error$subj[RI_ED_batch_error$subj == 25] <-"53"
RI_ED_batch_error$subj[RI_ED_batch_error$subj == 24] <-"51"
RI_ED_batch_error$subj[RI_ED_batch_error$subj == 23] <-"49"
RI_ED_batch_error$subj[RI_ED_batch_error$subj == 22] <-"47"
RI_ED_batch_error$subj[RI_ED_batch_error$subj == 21] <-"45"
RI_ED_batch_error$subj[RI_ED_batch_error$subj == 20] <-"41"
RI_ED_batch_error$subj[RI_ED_batch_error$subj == 19] <-"37"
RI_ED_batch_error$subj[RI_ED_batch_error$subj == 18] <-"35"
RI_ED_batch_error$subj[RI_ED_batch_error$subj == 17] <-"33"
RI_ED_batch_error$subj[RI_ED_batch_error$subj == 16] <-"31"
RI_ED_batch_error$subj[RI_ED_batch_error$subj == 15] <-"29"
RI_ED_batch_error$subj[RI_ED_batch_error$subj == 14] <-"27"
RI_ED_batch_error$subj[RI_ED_batch_error$subj == 13] <-"25"
RI_ED_batch_error$subj[RI_ED_batch_error$subj == 12] <-"23"
RI_ED_batch_error$subj[RI_ED_batch_error$subj == 11] <-"21"
RI_ED_batch_error$subj[RI_ED_batch_error$subj == 10] <-"19"
RI_ED_batch_error$subj[RI_ED_batch_error$subj == 9] <-"17"
RI_ED_batch_error$subj[RI_ED_batch_error$subj == 8] <-"15"
RI_ED_batch_error$subj[RI_ED_batch_error$subj == 7] <-"13"
RI_ED_batch_error$subj[RI_ED_batch_error$subj == 6] <-"11"
RI_ED_batch_error$subj[RI_ED_batch_error$subj == 5] <-"9"
RI_ED_batch_error$subj[RI_ED_batch_error$subj == 4] <-"7"
RI_ED_batch_error$subj[RI_ED_batch_error$subj == 3] <-"5"
RI_ED_batch_error$subj[RI_ED_batch_error$subj == 2] <-"3"

#Check the subject numbers have been redefined correctly
#View(RI_ED_batch_corr)
#View(RI_ED_batch_error)

#Let's combine the data
all_data <- rbind(RI_ED_batch_corr, RI_ED_batch_error)

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
view(all_data_join)

# Assign condition labels, 1 = prediction facilitated, 2 = prediction unfacilitated
#(this will make it easier to interpret)
all_data_join$cond <- recode(all_data_join$cond, "1" = "facilitated", "2" = "unfacilitated")

#Create a new CSV file with all the combined variables for future analysis
#write.csv(all_data_join,"C:\\Users\\elizabethle-luan\\Desktop\\Prediction Study 1\\all_data_join.csv", row.names = TRUE)
#view(all_data_join)
#C_Users_elizabethle_luan_Desktop_Prediction_Study_1_all_data_join <- read_csv("C:\\Users\\elizabethle-luan\\Desktop\\Prediction Study 1\\all_data_join.csv")
#View(C_Users_elizabethle_luan_Desktop_Prediction_Study_1_all_data_join)

#Let's have a look at region 4
#Let's start with Region 4 "the question"
#REGION 4 model using glmer

all_data_join$R4[all_data_join$R4 == 1] <-"100"

all_data_join %>% 
  ggplot(aes(x = cond, y = R4, colour = cond)) + ggtitle("Regression In for Critical Region: Question") +
  labs(y = "Regression", x = "Prediction") +
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(colour = FALSE)

all_data_join %>% 
  ggplot(aes(x = cond, y = R4, colour = cond)) + ggtitle("Regression In for Critical Region: Question") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(y = "Regression", x = "Prediction") +
  geom_jitter(alpha = .2, width = .1) +
  guides(colour = FALSE)

all_data_join %>% 
  group_by(cond) %>%
  summarise(mean(R4), sd(R4))

all_data_join$R4 <- as.factor(all_data_join$R4)

# Just eye tracking
modelR4 <- glmer(R4 ~ cond + (1 + cond | subj) + (1 + cond | item), all_data_join, family = "binomial") 
summary(modelR4)

modelR4.null <- glmer(R4 ~ (1 + cond | subj) + (1 + cond | item), all_data_join, family = "binomial") 

anova(modelR4, modelR4.null)
check_model(modelR4)

#Let's include some covariates! Region 4

#Step 1: Scale the ID measures...
all_data_join$SRS_total_score_t <- scale(all_data_join$SRS_total_score_t)
all_data_join$EQ <- scale(all_data_join$EQ)
all_data_join$Total_reading_cluster <- scale(all_data_join$Total_reading_cluster)
all_data_join$Total_RAN <- scale(all_data_join$Total_RAN)
all_data_join$"WI _RPI" <- scale(all_data_join$"WI _RPI")

model_alldatacov_R4 <- glmer(R4 ~ cond + SRS_total_score_t + EQ + Total_reading_cluster + Total_RAN +
                               (1 | subj) +  (1 | item) , data = all_data_join, family = "binomial")

model_alldatacov_R4_null <- glmer(R4 ~ SRS_total_score_t + EQ + Total_reading_cluster + Total_RAN +
                                    (1 | subj) +  (1 | item) , data = all_data_join, family = "binomial")

summary(model_alldatacov_R4)
anova(model_alldatacov_R4_null, model_alldatacov_R4)
check_model(model_alldatacov_R4)

# Error in anova.merMod(modelR4, model_alldatacov_R4): models were not all fitted to the same size of dataset
#How to get around this?
anova(modelR4, model_alldatacov_R4)

ranef(model_alldatacov_R4)

#What does region 5 "the reply" look like?

all_data_join$R5[all_data_join$R5 == 1] <-"100"

# Visualise
all_data_join %>% 
  ggplot(aes(x = cond, y = R5, colour = cond)) + ggtitle("Regression In for Post-Critical Region: Reply") +
  labs(y = "Regression", x = "Prediction") +
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(colour = FALSE)

all_data_join %>% 
  group_by(cond) %>%
  summarise(mean(R5), sd(R5))

library(psych)
describeBy(all_data_join$R5, group=all_data_join$cond)

all_data_join$R5 <- as.factor(all_data_join$R5)

modelR5 <- glmer(R5 ~ cond + (1 + cond | subj) + (1 + cond | item), all_data_join, family = "binomial") 
summary(modelR5)

modelR5.null <- glmer(R5 ~ (1 + cond | subj) + (1 + cond | item), all_data_join, family = "binomial") 

anova(modelR5, modelR5.null)

check_model(modelR5)
qqnorm(residuals(model))
qqline(residuals(model))

#Let's include some covariates! Region 5

#Step 1: Scale the ID measures...
all_data_join$SRS_total_score_t <- scale(all_data_join$SRS_total_score_t)
all_data_join$EQ <- scale(all_data_join$EQ)
all_data_join$Total_reading_cluster <- scale(all_data_join$Total_reading_cluster)
all_data_join$Total_RAN <- scale(all_data_join$Total_RAN)
all_data_join$"WI _RPI" <- scale(all_data_join$"WI _RPI")

model_alldatacov_R5 <- glmer(R5 ~ cond + SRS_total_score_t + EQ + Total_reading_cluster + Total_RAN +
                               (1 | subj) +  (1 | item) , data = all_data_join, family = "binomial")

model_alldatacov_R5_null <- glmer(R5 ~ SRS_total_score_t + EQ + Total_reading_cluster + Total_RAN +
                                    (1 | subj) +  (1 | item) , data = all_data_join, family = "binomial")

summary(model_alldatacov_R5)
anova(model_alldatacov_R5_null, model_alldatacov_R5)
check_model(model_alldatacov_R5)

# Error in anova.merMod(modelR5, model_alldatacov_R5): models were not all fitted to the same size of dataset
#How to get around this?
anova(modelR5, model_alldatacov_R5)

ranef(model_alldatacov_R5)

library(Hmisc)
#Measuring Correlations
EQscore <- all_data_join %>% group_by(subj) %>% summarise(mean = mean(EQ)) %>% pull(mean)
SRS2 <- all_data_join %>% group_by(subj) %>% summarise(mean = mean(SRS_total_score_t)) %>% pull(mean)
WRMT <- all_data_join %>% group_by(subj) %>% summarise(mean = mean(Total_reading_cluster)) %>% pull(mean)
RAN <- all_data_join %>% group_by(subj) %>% summarise(mean = mean(Total_RAN)) %>% pull(mean)
rcorr(EQscore, SRS2)
rcorr(EQscore, WRMT)
rcorr(EQscore, RAN)
rcorr(SRS2, WRMT)
rcorr(SRS2, RAN)
rcorr(WRMT, RAN)


#rcorr(all_data_join$SRS2_total_score_t, all_data_join$EQ)
#library(psych)
#describeBy(all_data_join$R4, group=all_data_join$cond)
