# Load packages
library(Matrix)
library(lme4)
library(lmerTest)
library(emmeans)
library(stats)
library(fitdistrplus)
library(tidyverse)
library(buildmer)
library(performance)
library(see)
#Set seed for random number generation
set.seed(42)
knitr::opts_chunk$set(cache.extra = knitr::rand_seed)
library(readr)
RP_FA_corr <- read_csv("FixAlign/FA_analysis/regression_path/RP_FA_corr.csv")
RP_FA_error <- read_csv("FixAlign/FA_analysis/regression_path/RP_FA_error.csv")
#Rename the participant numbers in the batches back to their original participant numbers.
RP_FA_corr$subj[RP_FA_corr$subj == 50] <-"83"
RP_FA_corr$subj[RP_FA_corr$subj == 49] <-"82"
RP_FA_corr$subj[RP_FA_corr$subj == 48] <-"81"
RP_FA_corr$subj[RP_FA_corr$subj == 47] <-"80"
RP_FA_corr$subj[RP_FA_corr$subj == 46] <-"79"
RP_FA_corr$subj[RP_FA_corr$subj == 45] <-"78"
RP_FA_corr$subj[RP_FA_corr$subj == 44] <-"77"
RP_FA_corr$subj[RP_FA_corr$subj == 43] <-"76"
RP_FA_corr$subj[RP_FA_corr$subj == 42] <-"75"
RP_FA_corr$subj[RP_FA_corr$subj == 41] <-"74"
RP_FA_corr$subj[RP_FA_corr$subj == 40] <-"73"
RP_FA_corr$subj[RP_FA_corr$subj == 39] <-"72"
RP_FA_corr$subj[RP_FA_corr$subj == 38] <-"71"
RP_FA_corr$subj[RP_FA_corr$subj == 37] <-"70"
RP_FA_corr$subj[RP_FA_corr$subj == 36] <-"69"
RP_FA_corr$subj[RP_FA_corr$subj == 35] <-"68"
RP_FA_corr$subj[RP_FA_corr$subj == 34] <-"67"
RP_FA_corr$subj[RP_FA_corr$subj == 33] <-"66"
RP_FA_corr$subj[RP_FA_corr$subj == 32] <-"64"
RP_FA_corr$subj[RP_FA_corr$subj == 31] <-"62"
RP_FA_corr$subj[RP_FA_corr$subj == 30] <-"60"
RP_FA_corr$subj[RP_FA_corr$subj == 29] <-"58"
RP_FA_corr$subj[RP_FA_corr$subj == 28] <-"56"
RP_FA_corr$subj[RP_FA_corr$subj == 27] <-"54"
RP_FA_corr$subj[RP_FA_corr$subj == 26] <-"50"
RP_FA_corr$subj[RP_FA_corr$subj == 25] <-"48"
RP_FA_corr$subj[RP_FA_corr$subj == 24] <-"46"
RP_FA_corr$subj[RP_FA_corr$subj == 23] <-"44"
RP_FA_corr$subj[RP_FA_corr$subj == 22] <-"43"
RP_FA_corr$subj[RP_FA_corr$subj == 21] <-"42"
RP_FA_corr$subj[RP_FA_corr$subj == 20] <-"40"
RP_FA_corr$subj[RP_FA_corr$subj == 19] <-"39"
RP_FA_corr$subj[RP_FA_corr$subj == 18] <-"38"
RP_FA_corr$subj[RP_FA_corr$subj == 17] <-"36"
RP_FA_corr$subj[RP_FA_corr$subj == 16] <-"34"
RP_FA_corr$subj[RP_FA_corr$subj == 15] <-"32"
RP_FA_corr$subj[RP_FA_corr$subj == 14] <-"30"
RP_FA_corr$subj[RP_FA_corr$subj == 13] <-"28"
RP_FA_corr$subj[RP_FA_corr$subj == 12] <-"26"
RP_FA_corr$subj[RP_FA_corr$subj == 11] <-"22"
RP_FA_corr$subj[RP_FA_corr$subj == 10] <-"20"
RP_FA_corr$subj[RP_FA_corr$subj == 9] <-"18"
RP_FA_corr$subj[RP_FA_corr$subj == 8] <-"16"
RP_FA_corr$subj[RP_FA_corr$subj == 7] <-"14"
RP_FA_corr$subj[RP_FA_corr$subj == 6] <-"12"
RP_FA_corr$subj[RP_FA_corr$subj == 5] <-"10"
RP_FA_corr$subj[RP_FA_corr$subj == 4] <-"8"
RP_FA_corr$subj[RP_FA_corr$subj == 3] <-"6"
RP_FA_corr$subj[RP_FA_corr$subj == 2] <-"4"
RP_FA_corr$subj[RP_FA_corr$subj == 1] <-"2"
RP_FA_error$subj[RP_FA_error$subj == 29] <-"65"
RP_FA_error$subj[RP_FA_error$subj == 28] <-"63"
RP_FA_error$subj[RP_FA_error$subj == 27] <-"61"
RP_FA_error$subj[RP_FA_error$subj == 26] <-"59"
RP_FA_error$subj[RP_FA_error$subj == 25] <-"57"
RP_FA_error$subj[RP_FA_error$subj == 24] <-"55"
RP_FA_error$subj[RP_FA_error$subj == 23] <-"53"
RP_FA_error$subj[RP_FA_error$subj == 22] <-"51"
RP_FA_error$subj[RP_FA_error$subj == 21] <-"49"
RP_FA_error$subj[RP_FA_error$subj == 20] <-"47"
RP_FA_error$subj[RP_FA_error$subj == 19] <-"45"
RP_FA_error$subj[RP_FA_error$subj == 18] <-"41"
RP_FA_error$subj[RP_FA_error$subj == 17] <-"37"
RP_FA_error$subj[RP_FA_error$subj == 16] <-"33"
RP_FA_error$subj[RP_FA_error$subj == 15] <-"31"
RP_FA_error$subj[RP_FA_error$subj == 14] <-"29"
RP_FA_error$subj[RP_FA_error$subj == 13] <-"25"
RP_FA_error$subj[RP_FA_error$subj == 12] <-"23"
RP_FA_error$subj[RP_FA_error$subj == 11] <-"21"
RP_FA_error$subj[RP_FA_error$subj == 10] <-"19"
RP_FA_error$subj[RP_FA_error$subj == 9] <-"17"
RP_FA_error$subj[RP_FA_error$subj == 8] <-"15"
RP_FA_error$subj[RP_FA_error$subj == 7] <-"13"
RP_FA_error$subj[RP_FA_error$subj == 6] <-"11"
RP_FA_error$subj[RP_FA_error$subj == 5] <-"9"
RP_FA_error$subj[RP_FA_error$subj == 4] <-"7"
RP_FA_error$subj[RP_FA_error$subj == 3] <-"5"
RP_FA_error$subj[RP_FA_error$subj == 2] <-"3"

#Check the subject numbers have been redefined correctly
#View(RP_FA_corr)
#View(RP_FA_error)

#Let's combine the data
all_data <- rbind(RP_FA_corr, RP_FA_error)

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
#view(all_data_join)

# Assign condition labels, 1 = prediction facilitated, 2 = prediction unfacilitated
#(this will make it easier to interpret)
all_data_join$cond <- recode(all_data_join$cond, "1" = "facilitated", "2" = "unfacilitated")

#Let's have a look at region 4

#set condition as a factor
all_data_join$cond <- as.factor(all_data_join$cond)
# Throw away zeroes
all_data_join <- all_data_join %>% filter(R4 != 0)

#Let's have a look at region 4

#set condition as a factor
all_data_join$cond <- as.factor(all_data_join$cond)
# Throw away zeroes
all_data_join <- all_data_join %>% filter(R4 != 0)

#Visualisation
all_data_join %>% 
  ggplot(aes(x = cond, y = R4, colour = cond)) + ggtitle("Regression Path for Critical Region: Question") +
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
#model.nullR4 <- lmer(R4 ~ (1 + cond | subj) + (1 + cond | item), all_data_join) 
modelR4 <- lmer(R4 ~ cond + (1 + cond | subj) + (1 + cond | item), all_data_join) 
summary(modelR4)

#anova(modelR4, model.nullR4)

#All the data for this model looks pretty normal.
check_model(modelR4)
#qqnorm(residuals(modelR4))
#qqline(residuals(modelR4))
#descdist(all_data_join$R4)
#Let's include some co-variates! Region 4

#Step 1: Scale the ID measures...
all_data_join$SRS_total_score_t <- scale(all_data_join$SRS_total_score_t)
all_data_join$EQ <- scale(all_data_join$EQ)
all_data_join$Total_reading_cluster <- scale(all_data_join$Total_reading_cluster)
all_data_join$Total_RAN <- scale(all_data_join$Total_RAN)
all_data_join$"WI _RPI" <- scale(all_data_join$"WI _RPI")
# Model including covariates
model_alldatacov_R4 <- lmer(R4 ~ SRS_total_score_t + EQ + Total_reading_cluster + Total_RAN + cond + (1 + cond | subj) +  (1 + cond | item) , data = all_data_join, REML = TRUE)

#model_alldatacov_R4_null <- lmer(R4 ~ SRS_total_score_t + EQ + Total_reading_cluster + #Total_RAN +
#                               (1 + cond | subj) +  (1 + cond | item) , data = #all_data_join, REML = TRUE)

summary(model_alldatacov_R4)

#anova(model_alldatacov_R4_null, model_alldatacov_R4)
check_model(model_alldatacov_R4)
ranef(model_alldatacov_R4)

# summary of Results for region 4, the question.
# only RAN influences processing and not as a function of our condition.

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


#Let's have a look at region 5

#set condition as a factor
all_data_join$cond <- as.factor(all_data_join$cond)
# Throw away zeroes
all_data_join <- all_data_join %>% filter(R5 != 0)

#Let's have a look at region 5

#set condition as a factor
all_data_join$cond <- as.factor(all_data_join$cond)
# Throw away zeroes
all_data_join <- all_data_join %>% filter(R5 != 0)

#Visualisation
all_data_join %>% 
  ggplot(aes(x = cond, y = R5, colour = cond)) + ggtitle("Regression Path for Post-Critical Region: Reply") +
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
#model.nullR5 <- lmer(R5 ~ (1 + cond | subj) + (1 + cond | item), all_data_join) 
modelR5 <- lmer(R5 ~ cond + (1 + cond | subj) + (1 + cond | item), all_data_join) 
summary(modelR5)

#anova(modelR5, model.nullR5)

#All the data for this model looks pretty normal.
check_model(modelR5)
#qqnorm(residuals(modelR5))
#qqline(residuals(modelR5))
#descdist(all_data_join$R5)
#Let's include some co-variates! region 5

#Step 1: Scale the ID measures...
all_data_join$SRS_total_score_t <- scale(all_data_join$SRS_total_score_t)
all_data_join$EQ <- scale(all_data_join$EQ)
all_data_join$Total_reading_cluster <- scale(all_data_join$Total_reading_cluster)
all_data_join$Total_RAN <- scale(all_data_join$Total_RAN)
all_data_join$"WI _RPI" <- scale(all_data_join$"WI _RPI")

# Model including covariates
model_alldatacov_R5 <- lmer(R5 ~ SRS_total_score_t + EQ + Total_reading_cluster + Total_RAN + cond + (1 + cond | subj) +  (1 + cond | item) , data = all_data_join, REML = TRUE)

#model_alldatacov_R5_null <- lmer(R5 ~ SRS_total_score_t + EQ + Total_reading_cluster + #Total_RAN +
#                               (1 + cond | subj) +  (1 + cond | item) , data = #all_data_join, REML = TRUE)

summary(model_alldatacov_R5)

#anova(model_alldatacov_R5_null, model_alldatacov_R5)
check_model(model_alldatacov_R5)

ranef(model_alldatacov_R5)