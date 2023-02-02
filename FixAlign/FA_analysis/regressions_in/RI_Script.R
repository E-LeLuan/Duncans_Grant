# Load packages
library(Matrix)
library(lme4)
library(lmerTest)
library(emmeans)
library(stats)
library(brms)
library(fitdistrplus)
library(Hmisc)
library(tidyverse)
library(buildmer)
library(performance)
library(see)

# Analysis of regressions in data
RI_FA_corr <- read_csv("FixAlign/FA_analysis/regressions_in/RI_FA_corr.csv")
RI_FA_error <- read_csv("FixAlign/FA_analysis/regressions_in/RI_FA_error.csv")
#Rename the participant numbers in the batches back to their original participant numbers.
RI_FA_corr$subj[RI_FA_corr$subj == 50] <-"83"
RI_FA_corr$subj[RI_FA_corr$subj == 49] <-"82"
RI_FA_corr$subj[RI_FA_corr$subj == 48] <-"81"
RI_FA_corr$subj[RI_FA_corr$subj == 47] <-"80"
RI_FA_corr$subj[RI_FA_corr$subj == 46] <-"79"
RI_FA_corr$subj[RI_FA_corr$subj == 45] <-"78"
RI_FA_corr$subj[RI_FA_corr$subj == 44] <-"77"
RI_FA_corr$subj[RI_FA_corr$subj == 43] <-"76"
RI_FA_corr$subj[RI_FA_corr$subj == 42] <-"75"
RI_FA_corr$subj[RI_FA_corr$subj == 41] <-"74"
RI_FA_corr$subj[RI_FA_corr$subj == 40] <-"73"
RI_FA_corr$subj[RI_FA_corr$subj == 39] <-"72"
RI_FA_corr$subj[RI_FA_corr$subj == 38] <-"71"
RI_FA_corr$subj[RI_FA_corr$subj == 37] <-"70"
RI_FA_corr$subj[RI_FA_corr$subj == 36] <-"69"
RI_FA_corr$subj[RI_FA_corr$subj == 35] <-"68"
RI_FA_corr$subj[RI_FA_corr$subj == 34] <-"67"
RI_FA_corr$subj[RI_FA_corr$subj == 33] <-"66"
RI_FA_corr$subj[RI_FA_corr$subj == 32] <-"64"
RI_FA_corr$subj[RI_FA_corr$subj == 31] <-"62"
RI_FA_corr$subj[RI_FA_corr$subj == 30] <-"60"
RI_FA_corr$subj[RI_FA_corr$subj == 29] <-"58"
RI_FA_corr$subj[RI_FA_corr$subj == 28] <-"56"
RI_FA_corr$subj[RI_FA_corr$subj == 27] <-"54"
RI_FA_corr$subj[RI_FA_corr$subj == 26] <-"50"
RI_FA_corr$subj[RI_FA_corr$subj == 25] <-"48"
RI_FA_corr$subj[RI_FA_corr$subj == 24] <-"46"
RI_FA_corr$subj[RI_FA_corr$subj == 23] <-"44"
RI_FA_corr$subj[RI_FA_corr$subj == 22] <-"43"
RI_FA_corr$subj[RI_FA_corr$subj == 21] <-"42"
RI_FA_corr$subj[RI_FA_corr$subj == 20] <-"40"
RI_FA_corr$subj[RI_FA_corr$subj == 19] <-"39"
RI_FA_corr$subj[RI_FA_corr$subj == 18] <-"38"
RI_FA_corr$subj[RI_FA_corr$subj == 17] <-"36"
RI_FA_corr$subj[RI_FA_corr$subj == 16] <-"34"
RI_FA_corr$subj[RI_FA_corr$subj == 15] <-"32"
RI_FA_corr$subj[RI_FA_corr$subj == 14] <-"30"
RI_FA_corr$subj[RI_FA_corr$subj == 13] <-"28"
RI_FA_corr$subj[RI_FA_corr$subj == 12] <-"26"
RI_FA_corr$subj[RI_FA_corr$subj == 11] <-"22"
RI_FA_corr$subj[RI_FA_corr$subj == 10] <-"20"
RI_FA_corr$subj[RI_FA_corr$subj == 9] <-"18"
RI_FA_corr$subj[RI_FA_corr$subj == 8] <-"16"
RI_FA_corr$subj[RI_FA_corr$subj == 7] <-"14"
RI_FA_corr$subj[RI_FA_corr$subj == 6] <-"12"
RI_FA_corr$subj[RI_FA_corr$subj == 5] <-"10"
RI_FA_corr$subj[RI_FA_corr$subj == 4] <-"8"
RI_FA_corr$subj[RI_FA_corr$subj == 3] <-"6"
RI_FA_corr$subj[RI_FA_corr$subj == 2] <-"4"
RI_FA_corr$subj[RI_FA_corr$subj == 1] <-"2"
RI_FA_error$subj[RI_FA_error$subj == 29] <-"65"
RI_FA_error$subj[RI_FA_error$subj == 28] <-"63"
RI_FA_error$subj[RI_FA_error$subj == 27] <-"61"
RI_FA_error$subj[RI_FA_error$subj == 26] <-"59"
RI_FA_error$subj[RI_FA_error$subj == 25] <-"57"
RI_FA_error$subj[RI_FA_error$subj == 24] <-"55"
RI_FA_error$subj[RI_FA_error$subj == 23] <-"53"
RI_FA_error$subj[RI_FA_error$subj == 22] <-"51"
RI_FA_error$subj[RI_FA_error$subj == 21] <-"49"
RI_FA_error$subj[RI_FA_error$subj == 20] <-"47"
RI_FA_error$subj[RI_FA_error$subj == 19] <-"45"
RI_FA_error$subj[RI_FA_error$subj == 18] <-"41"
RI_FA_error$subj[RI_FA_error$subj == 17] <-"37"
RI_FA_error$subj[RI_FA_error$subj == 16] <-"33"
RI_FA_error$subj[RI_FA_error$subj == 15] <-"31"
RI_FA_error$subj[RI_FA_error$subj == 14] <-"29"
RI_FA_error$subj[RI_FA_error$subj == 13] <-"25"
RI_FA_error$subj[RI_FA_error$subj == 12] <-"23"
RI_FA_error$subj[RI_FA_error$subj == 11] <-"21"
RI_FA_error$subj[RI_FA_error$subj == 10] <-"19"
RI_FA_error$subj[RI_FA_error$subj == 9] <-"17"
RI_FA_error$subj[RI_FA_error$subj == 8] <-"15"
RI_FA_error$subj[RI_FA_error$subj == 7] <-"13"
RI_FA_error$subj[RI_FA_error$subj == 6] <-"11"
RI_FA_error$subj[RI_FA_error$subj == 5] <-"9"
RI_FA_error$subj[RI_FA_error$subj == 4] <-"7"
RI_FA_error$subj[RI_FA_error$subj == 3] <-"5"
RI_FA_error$subj[RI_FA_error$subj == 2] <-"3"

#Check the subject numbers have been redefined correctly
#View(RI_FA_corr)
#View(RI_FA_error)

#Let's combine the data
all_data <- rbind(RI_FA_corr, RI_FA_error)

#make subj a factor
all_data$subj <- as.factor(all_data$subj)

#Import Individual difference measures
All_IDs <- read_csv("FixAlign/FA_analysis/All_IDs_FA.csv")
#View(All_IDs)

# Rename Participabt in ID_measures to subj to be the same as current data set
All_IDs <- rename(All_IDs, subj = Participant)
All_IDs$subj <- as.factor(All_IDs$subj)

# Add the ID's to the data frame
all_data_join <- inner_join(all_data, All_IDs, by = "subj")
# view(all_data_join)

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

#all_data_join %>% 
#  group_by(cond) %>%
#  summarise(mean(R4), sd(R4))

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

#model_alldatacov_R4_null <- glmer(R4 ~ SRS_total_score_t + EQ + Total_reading_cluster + Total_RAN +
#                                    (1 | subj) +  (1 | item) , data = all_data_join, family = "binomial")

summary(model_alldatacov_R4)
#anova(model_alldatacov_R4_null, model_alldatacov_R4)
check_model(model_alldatacov_R4)

# NO RAN - Our condition is still significant
model_alldatacov_R4_noRAN <- glmer(R4 ~ cond + SRS_total_score_t + EQ + Total_reading_cluster +
                               (1 | subj) +  (1 | item) , data = all_data_join, family = "binomial")
summary(model_alldatacov_R4_noRAN)

# Include Interaction between RAN and condition
model_alldatacov_R4_int <- glmer(R4 ~ cond + SRS_total_score_t + EQ + Total_reading_cluster + Total_RAN + Total_RAN:cond +
                                     (1 | subj) +  (1 | item) , data = all_data_join, family = "binomial")
summary(model_alldatacov_R4_int)


# Error in anova.merMod(modelR4, model_alldatacov_R4): models were not all fitted to the same size of dataset
# How to get around this?
# anova(modelR4, model_alldatacov_R4)

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

#all_data_join %>% 
#  group_by(cond) %>%
#  summarise(mean(R5), sd(R5))

#library(psych)
#describeBy(all_data_join$R5, group=all_data_join$cond)

all_data_join$R5 <- as.factor(all_data_join$R5)

modelR5 <- glmer(R5 ~ cond + (1 | subj) + (1 | item), all_data_join, family = "binomial") 
summary(modelR5)

modelR5.null <- glmer(R5 ~ (1 | subj) + (1 | item), all_data_join, family = "binomial") 

anova(modelR5, modelR5.null)

check_model(modelR5)
qqnorm(residuals(model))
qqline(residuals(model))

#Let's include some covariates! Region 5

#Step 1: Scale the ID measures...
#all_data_join$SRS_total_score_t <- scale(all_data_join$SRS_total_score_t)
#all_data_join$EQ <- scale(all_data_join$EQ)
#all_data_join$Total_reading_cluster <- scale(all_data_join$Total_reading_cluster)
#all_data_join$Total_RAN <- scale(all_data_join$Total_RAN)
#all_data_join$"WI _RPI" <- scale(all_data_join$"WI _RPI")

model_alldatacov_R5 <- glmer(R5 ~ cond + SRS_total_score_t + EQ + Total_reading_cluster + Total_RAN +
                               (1 | subj) +  (1 | item) , data = all_data_join, family = "binomial")

#model_alldatacov_R5_null <- glmer(R5 ~ SRS_total_score_t + EQ + Total_reading_cluster + Total_RAN +
   #                                 (1 + cond | subj) +  (1 + cond | item) , data = all_data_join, family = "binomial")

summary(model_alldatacov_R5)
#anova(model_alldatacov_R5_null, model_alldatacov_R5)
check_model(model_alldatacov_R5)

# Error in anova.merMod(modelR5, model_alldatacov_R5): models were not all fitted to the same size of dataset
#How to get around this?
#anova(modelR5, model_alldatacov_R5)

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

#What does Region 3 "the Manipulation" look like?

all_data_join$R3[all_data_join$R3 == 1] <-"100"

# Visualise
all_data_join %>% 
  ggplot(aes(x = cond, y = R3, colour = cond)) + ggtitle("Regression In for Post-Critical Region: Manipulation") +
  labs(y = "Regression", x = "Prediction") +
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(colour = FALSE)

#all_data_join %>% 
#  group_by(cond) %>%
#  summarise(mean(R3), sd(R3))

#library(psych)
#describeBy(all_data_join$R3, group=all_data_join$cond)

all_data_join$R3 <- as.factor(all_data_join$R3)

modelR3 <- glmer(R3 ~ cond + (1 | subj) + (1 + cond | item), all_data_join, family = "binomial") 
summary(modelR3)

modelR3.null <- glmer(R3 ~ (1 | subj) + (1 + cond | item), all_data_join, family = "binomial") 

anova(modelR3, modelR3.null)

check_model(modelR3)
qqnorm(residuals(model))
qqline(residuals(model))

#Let's include some covariates! Region 3

#Step 1: Scale the ID measures...
#all_data_join$SRS_total_score_t <- scale(all_data_join$SRS_total_score_t)
#all_data_join$EQ <- scale(all_data_join$EQ)
#all_data_join$Total_reading_cluster <- scale(all_data_join$Total_reading_cluster)
#all_data_join$Total_RAN <- scale(all_data_join$Total_RAN)
#all_data_join$"WI _RPI" <- scale(all_data_join$"WI _RPI")

model_alldatacov_R3 <- glmer(R3 ~ cond + SRS_total_score_t + EQ + Total_reading_cluster + Total_RAN +
                               (1 | subj) +  (1 + cond | item) , data = all_data_join, family = "binomial")

#model_alldatacov_R3_null <- glmer(R3 ~ SRS_total_score_t + EQ + Total_reading_cluster + Total_RAN +
#                                 (1 + cond | subj) +  (1 + cond | item) , data = all_data_join, family = "binomial")

summary(model_alldatacov_R3)
#anova(model_alldatacov_R3_null, model_alldatacov_R3)
check_model(model_alldatacov_R3)

# Error in anova.merMod(modelR3, model_alldatacov_R3): models were not all fitted to the same size of dataset
#How to get around this?
#anova(modelR3, model_alldatacov_R3)

ranef(model_alldatacov_R3)

