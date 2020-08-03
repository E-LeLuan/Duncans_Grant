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

#As readers do we predict upcoming information before it is encountered in order to better understand the unfolding discourse? If prediction is not utilized do we rely on more on basic integration to understand an unfolding discourse context? Do individual differences contribute to our ability to predict and/or integrate upcoming information? How do these contribute? 
library(readr)
FP_ED_batch_corr <- read_csv("FP_ED_batch_corr.csv")
FP_ED_batch_error <- read_csv("FP_ED_batch_error.csv")

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
All_IDs <- read_csv("../../All_IDs.csv")
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

# Region 4: The question
#Let's have a look at various models for our critical region of interest, the question.

#Let's have a look at region 4

#set condition as a factor
all_data_join$cond <- as.factor(all_data_join$cond)
# Throw away zeroes
all_data_join <- all_data_join %>% filter(R4 != 0)

## Eye-tracking only model
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
```

## Individual differences as controlling factors
#Let's include some co-variates! Region 4

#Step 1: Scale the ID measures...
all_data_join$SRS_total_score_t <- scale(all_data_join$SRS_total_score_t)
all_data_join$EQ <- scale(all_data_join$EQ)
all_data_join$Total_reading_cluster <- scale(all_data_join$Total_reading_cluster)
all_data_join$Total_RAN <- scale(all_data_join$Total_RAN)
all_data_join$"WI _RPI" <- scale(all_data_join$"WI _RPI")

# Model including covariates
model_alldatacov_R4 <- lmer(R4 ~ SRS_total_score_t + EQ + Total_reading_cluster + Total_RAN + cond + (1 | subj) +  (1 | item) , data = all_data_join, REML = TRUE)

#model_alldatacov_R4_null <- lmer(R4 ~ SRS_total_score_t + EQ + Total_reading_cluster + #Total_RAN +
#                               (1 + cond | subj) +  (1 + cond | item) , data = #all_data_join, REML = TRUE)

summary(model_alldatacov_R4)

#So Total_RAN and our condition are significant. If we drop the Total_RAN the effect of condition is still there (see below) - suggesting to me that the effect of Total_RAN is separate from the effect of condition.

model_alldatacov_R4_noRAN <- lmer(R4 ~ SRS_total_score_t + EQ + Total_reading_cluster + cond + (1 | subj) +  (1 | item) , data = all_data_join, REML = TRUE)
summary(model_alldatacov_R4_noRAN)


#Does Total_RAN interact with our condition? No (see below.)

model_alldatacov_R4_RAN_int <- lmer(R4 ~ SRS_total_score_t + EQ + Total_reading_cluster + Total_RAN + cond + cond:Total_RAN + (1 | subj) +  (1 | item) , data = all_data_join, REML = TRUE)
summary(model_alldatacov_R4_RAN_int)

#anova(model_alldatacov_R4_null, model_alldatacov_R4)
check_model(model_alldatacov_R4)

ranef(model_alldatacov_R4)


# summary of Results for region 4, the question.
#After controlling for individual differences participants are significantly faster at reading facilitated conditions compared to un-facilitated conditions where they take an extra 73 milliseconds to complete their first pass read through of the text. There is a 136 millisecond increase in reading times with each millisecond increase of the RAN. In other words, the slower your rapid naming times (indicative of poorer verbal fluency) the longer it takes you to integrate contextual information into a mental representation of the scenario encountered.  However, it is likely Total_RAN explains overall reading time differences, but not anything to do with the difference between our experimental conditions - otherwise we'd have seen an interaction effect. Regardless of whether the individual predictors are present/absent, the effect of our condition is pretty much the same - suggesting to me that the variance explained by our experimental manipulation doesn't overlap with the variance explained by our individual difference measures. 

# Correlations 

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
```

# Region 5: The reply
#Let's have a look at various models for our post-critical region of interest, the reply.

## Eye-tracking only model
#set condition as a factor
all_data_join$cond <- as.factor(all_data_join$cond)
# Throw away zeroes
all_data_join <- all_data_join %>% filter(R5 != 0)


## Eye-tracking only model
#Let's have a look at region 4
#set condition as a factor
all_data_join$cond <- as.factor(all_data_join$cond)
# Throw away zeroes
all_data_join <- all_data_join %>% filter(R5 != 0)

#Visualisation
all_data_join %>% 
  ggplot(aes(x = cond, y = R5, colour = cond)) + ggtitle("First Pass for Critical Region: Question") +
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


## Individual differences as controlling factors

#Let's include some co-variates! Region 4

#Step 1: Scale the ID measures...
all_data_join$SRS_total_score_t <- scale(all_data_join$SRS_total_score_t)
all_data_join$EQ <- scale(all_data_join$EQ)
all_data_join$Total_reading_cluster <- scale(all_data_join$Total_reading_cluster)
all_data_join$Total_RAN <- scale(all_data_join$Total_RAN)
all_data_join$"WI _RPI" <- scale(all_data_join$"WI _RPI")

# Model including covariates
model_alldatacov_R5 <- lmer(R5 ~ SRS_total_score_t + EQ + Total_reading_cluster + Total_RAN + cond + (1 | subj) +  (1 | item) , data = all_data_join, REML = TRUE)

#model_alldatacov_R5_null <- lmer(R5 ~ SRS_total_score_t + EQ + Total_reading_cluster + #Total_RAN +
#                               (1 + cond | subj) +  (1 + cond | item) , data = #all_data_join, REML = TRUE)

summary(model_alldatacov_R5)


#So Total_RAN and our condition are significant. If we drop the Total_RAN the effect of condition is still there (see below) - suggesting to me that the effect of Total_RAN is separate from the effect of condition.

model_alldatacov_R5_noRAN <- lmer(R5 ~ SRS_total_score_t + EQ + Total_reading_cluster + cond + (1 | subj) +  (1 | item) , data = all_data_join, REML = TRUE)
summary(model_alldatacov_R5_noRAN)


#Does Total_RAN interact with our condition? No (see below.)

model_alldatacov_R5_RAN_int <- lmer(R5 ~ SRS_total_score_t + EQ + Total_reading_cluster + Total_RAN + cond + cond:Total_RAN + (1 | subj) +  (1 | item) , data = all_data_join, REML = TRUE)
summary(model_alldatacov_R5_RAN_int)

#anova(model_alldatacov_R5_null, model_alldatacov_R5)
check_model(model_alldatacov_R5)

ranef(model_alldatacov_R5)

# summary of Results for region 5, the reply
#After controlling for individual differences participants are significantly faster at reading facilitated conditions compared to un-facilitated conditions where they take an extra 73 milliseconds to complete their first pass read through of the text. There is a 136 millisecond increase in reading times with each millisecond increase of the RAN. In other words, the slower your rapid naming times (indicative of poorer verbal fluency) the longer it takes you to integrate contextual information into a mental representation of the scenario encountered.  However, it is likely Total_RAN explains overall reading time differences, but not anything to do with the difference between our experimental conditions - otherwise we'd have seen an interaction effect. Regardless of whether the individual predictors are present/absent, the effect of our condition is pretty much the same - suggesting to me that the variance explained by our experimental manipulation doesn't overlap with the variance explained by our individual difference measures. 

#After controlling for individual differences participants are significantly faster at reading facilitated conditions compared to un-facilitated conditions where they take an extra 51 milliseconds to complete their first pass read through of the text. There is a 102 millisecond increase in reading times with each millisecond increase of the RAN. In other words, the slower your rapid naming times (indicative of poorer verbal fluency) the longer it takes you to integrate contextual information into a mental representation of the scenario encountered.  HAVE I INTERPRETED THIS RIGHT?????? (The ran interpretation confused me a bit).
