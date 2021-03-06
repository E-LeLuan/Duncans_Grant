#.libPaths("C:Program Files/R/R-3.5/library")
#devtools::install_github("crsh/papaja")
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
FP_FA_corr <- read_csv("FixAlign/FA_analysis/first_pass/FP_FA_corr.csv")
FP_FA_error <- read_csv("FixAlign/FA_analysis/first_pass/FP_FA_error.csv")
#Rename the participant numbers in the batches back to their original participant numbers.
FP_FA_corr$subj[FP_FA_corr$subj == 50] <-"83"
FP_FA_corr$subj[FP_FA_corr$subj == 49] <-"82"
FP_FA_corr$subj[FP_FA_corr$subj == 48] <-"81"
FP_FA_corr$subj[FP_FA_corr$subj == 47] <-"80"
FP_FA_corr$subj[FP_FA_corr$subj == 46] <-"79"
FP_FA_corr$subj[FP_FA_corr$subj == 45] <-"78"
FP_FA_corr$subj[FP_FA_corr$subj == 44] <-"77"
FP_FA_corr$subj[FP_FA_corr$subj == 43] <-"76"
FP_FA_corr$subj[FP_FA_corr$subj == 42] <-"75"
FP_FA_corr$subj[FP_FA_corr$subj == 41] <-"74"
FP_FA_corr$subj[FP_FA_corr$subj == 40] <-"73"
FP_FA_corr$subj[FP_FA_corr$subj == 39] <-"72"
FP_FA_corr$subj[FP_FA_corr$subj == 38] <-"71"
FP_FA_corr$subj[FP_FA_corr$subj == 37] <-"70"
FP_FA_corr$subj[FP_FA_corr$subj == 36] <-"69"
FP_FA_corr$subj[FP_FA_corr$subj == 35] <-"68"
FP_FA_corr$subj[FP_FA_corr$subj == 34] <-"67"
FP_FA_corr$subj[FP_FA_corr$subj == 33] <-"66"
FP_FA_corr$subj[FP_FA_corr$subj == 32] <-"64"
FP_FA_corr$subj[FP_FA_corr$subj == 31] <-"62"
FP_FA_corr$subj[FP_FA_corr$subj == 30] <-"60"
FP_FA_corr$subj[FP_FA_corr$subj == 29] <-"58"
FP_FA_corr$subj[FP_FA_corr$subj == 28] <-"56"
FP_FA_corr$subj[FP_FA_corr$subj == 27] <-"54"
FP_FA_corr$subj[FP_FA_corr$subj == 26] <-"50"
FP_FA_corr$subj[FP_FA_corr$subj == 25] <-"48"
FP_FA_corr$subj[FP_FA_corr$subj == 24] <-"46"
FP_FA_corr$subj[FP_FA_corr$subj == 23] <-"44"
FP_FA_corr$subj[FP_FA_corr$subj == 22] <-"43"
FP_FA_corr$subj[FP_FA_corr$subj == 21] <-"42"
FP_FA_corr$subj[FP_FA_corr$subj == 20] <-"40"
FP_FA_corr$subj[FP_FA_corr$subj == 19] <-"39"
FP_FA_corr$subj[FP_FA_corr$subj == 18] <-"38"
FP_FA_corr$subj[FP_FA_corr$subj == 17] <-"36"
FP_FA_corr$subj[FP_FA_corr$subj == 16] <-"34"
FP_FA_corr$subj[FP_FA_corr$subj == 15] <-"32"
FP_FA_corr$subj[FP_FA_corr$subj == 14] <-"30"
FP_FA_corr$subj[FP_FA_corr$subj == 13] <-"28"
FP_FA_corr$subj[FP_FA_corr$subj == 12] <-"26"
FP_FA_corr$subj[FP_FA_corr$subj == 11] <-"22"
FP_FA_corr$subj[FP_FA_corr$subj == 10] <-"20"
FP_FA_corr$subj[FP_FA_corr$subj == 9] <-"18"
FP_FA_corr$subj[FP_FA_corr$subj == 8] <-"16"
FP_FA_corr$subj[FP_FA_corr$subj == 7] <-"14"
FP_FA_corr$subj[FP_FA_corr$subj == 6] <-"12"
FP_FA_corr$subj[FP_FA_corr$subj == 5] <-"10"
FP_FA_corr$subj[FP_FA_corr$subj == 4] <-"8"
FP_FA_corr$subj[FP_FA_corr$subj == 3] <-"6"
FP_FA_corr$subj[FP_FA_corr$subj == 2] <-"4"
FP_FA_corr$subj[FP_FA_corr$subj == 1] <-"2"
FP_FA_error$subj[FP_FA_error$subj == 29] <-"65"
FP_FA_error$subj[FP_FA_error$subj == 28] <-"63"
FP_FA_error$subj[FP_FA_error$subj == 27] <-"61"
FP_FA_error$subj[FP_FA_error$subj == 26] <-"59"
FP_FA_error$subj[FP_FA_error$subj == 25] <-"57"
FP_FA_error$subj[FP_FA_error$subj == 24] <-"55"
FP_FA_error$subj[FP_FA_error$subj == 23] <-"53"
FP_FA_error$subj[FP_FA_error$subj == 22] <-"51"
FP_FA_error$subj[FP_FA_error$subj == 21] <-"49"
FP_FA_error$subj[FP_FA_error$subj == 20] <-"47"
FP_FA_error$subj[FP_FA_error$subj == 19] <-"45"
FP_FA_error$subj[FP_FA_error$subj == 18] <-"41"
FP_FA_error$subj[FP_FA_error$subj == 17] <-"37"
FP_FA_error$subj[FP_FA_error$subj == 16] <-"33"
FP_FA_error$subj[FP_FA_error$subj == 15] <-"31"
FP_FA_error$subj[FP_FA_error$subj == 14] <-"29"
FP_FA_error$subj[FP_FA_error$subj == 13] <-"25"
FP_FA_error$subj[FP_FA_error$subj == 12] <-"23"
FP_FA_error$subj[FP_FA_error$subj == 11] <-"21"
FP_FA_error$subj[FP_FA_error$subj == 10] <-"19"
FP_FA_error$subj[FP_FA_error$subj == 9] <-"17"
FP_FA_error$subj[FP_FA_error$subj == 8] <-"15"
FP_FA_error$subj[FP_FA_error$subj == 7] <-"13"
FP_FA_error$subj[FP_FA_error$subj == 6] <-"11"
FP_FA_error$subj[FP_FA_error$subj == 5] <-"9"
FP_FA_error$subj[FP_FA_error$subj == 4] <-"7"
FP_FA_error$subj[FP_FA_error$subj == 3] <-"5"
FP_FA_error$subj[FP_FA_error$subj == 2] <-"3"

#Check the subject numbers have been redefined correctly
#View(FP_FA_corr)
#View(FP_FA_error)

#Let's combine the data
all_data <- rbind(FP_FA_corr, FP_FA_error)

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
model_alldatacov_R4 <- lmer(R4 ~ SRS_total_score_t + EQ + Total_reading_cluster + Total_RAN + cond + (1 + cond | subj) +  (1 | item) , data = all_data_join, REML = TRUE)

#model_alldatacov_R4_null <- lmer(R4 ~ SRS_total_score_t + EQ + Total_reading_cluster + #Total_RAN +
#                               (1 + cond | subj) +  (1 + cond | item) , data = #all_data_join, REML = TRUE)

summary(model_alldatacov_R4)

#anova(model_alldatacov_R4_null, model_alldatacov_R4)
check_model(model_alldatacov_R4)

ranef(model_alldatacov_R4)

# summary of Results for region 4, the question.
#After controlling for individual differences participants are significantly faster at reading facilitated conditions compared to un-facilitated conditions where they take an extra 73 milliseconds to complete their first pass read through of the text. There is a 136 millisecond increase in reading times with each millisecond increase of the RAN. In other words, the slower your rapid naming times (indicative of poorer verbal fluency) the longer it takes you to integrate contextual information into a mental representation of the scenario encountered.  However, it is likely Total_RAN explains overall reading time differences, but not anything to do with the difference between our experimental conditions - otherwise we'd have seen an interaction effect.
#Regardless of whether the individual predictors are present/absent, the effect of our condition is pretty much the same - suggesting to me that the variance explained by our experimental manipulation doesn't overlap with the variance explained by our individual difference measures. 

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
