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
TT_ED_batch1_corr <- read_csv("Duncans-Grant-master/TT_ED/TT_ED_batch1_corr.csv")

#Rename the participant numbers in the batches back to their original participant numbers.
TT_ED_batch1_corr$subj[TT_ED_batch1_corr$subj == 20] <-"38"
TT_ED_batch1_corr$subj[TT_ED_batch1_corr$subj == 19] <-"36"
TT_ED_batch1_corr$subj[TT_ED_batch1_corr$subj == 18] <-"35"
TT_ED_batch1_corr$subj[TT_ED_batch1_corr$subj == 17] <-"34"
TT_ED_batch1_corr$subj[TT_ED_batch1_corr$subj == 16] <-"32"
TT_ED_batch1_corr$subj[TT_ED_batch1_corr$subj == 15] <-"30"
TT_ED_batch1_corr$subj[TT_ED_batch1_corr$subj == 14] <-"28"
TT_ED_batch1_corr$subj[TT_ED_batch1_corr$subj == 13] <-"26"
TT_ED_batch1_corr$subj[TT_ED_batch1_corr$subj == 12] <-"24"
TT_ED_batch1_corr$subj[TT_ED_batch1_corr$subj == 11] <-"22"
TT_ED_batch1_corr$subj[TT_ED_batch1_corr$subj == 10] <-"20"
TT_ED_batch1_corr$subj[TT_ED_batch1_corr$subj == 9] <-"18"
TT_ED_batch1_corr$subj[TT_ED_batch1_corr$subj == 8] <-"16"
TT_ED_batch1_corr$subj[TT_ED_batch1_corr$subj == 7] <-"14"
TT_ED_batch1_corr$subj[TT_ED_batch1_corr$subj == 6] <-"12"
TT_ED_batch1_corr$subj[TT_ED_batch1_corr$subj == 5] <-"10"
TT_ED_batch1_corr$subj[TT_ED_batch1_corr$subj == 4] <-"8"
TT_ED_batch1_corr$subj[TT_ED_batch1_corr$subj == 3] <-"6"
TT_ED_batch1_corr$subj[TT_ED_batch1_corr$subj == 2] <-"4"
TT_ED_batch1_corr$subj[TT_ED_batch1_corr$subj == 1] <-"2"

#import the data set batch 2B
TT_ED_batch2B_corr <- read_csv("Duncans-Grant-master/TT_ED/TT_ED_batch2B_corr.csv")

#Rename the participant numbers in the batches back to their original participant numbers.
TT_ED_batch2B_corr$subj[TT_ED_batch2B_corr$subj == 19] <-"71"
TT_ED_batch2B_corr$subj[TT_ED_batch2B_corr$subj == 18] <-"70"
TT_ED_batch2B_corr$subj[TT_ED_batch2B_corr$subj == 17] <-"69"
TT_ED_batch2B_corr$subj[TT_ED_batch2B_corr$subj == 16] <-"68"
TT_ED_batch2B_corr$subj[TT_ED_batch2B_corr$subj == 15] <-"66"
TT_ED_batch2B_corr$subj[TT_ED_batch2B_corr$subj == 14] <-"64"
TT_ED_batch2B_corr$subj[TT_ED_batch2B_corr$subj == 13] <-"62"
TT_ED_batch2B_corr$subj[TT_ED_batch2B_corr$subj == 12] <-"60"
TT_ED_batch2B_corr$subj[TT_ED_batch2B_corr$subj == 11] <-"58"
TT_ED_batch2B_corr$subj[TT_ED_batch2B_corr$subj == 10] <-"56"
TT_ED_batch2B_corr$subj[TT_ED_batch2B_corr$subj == 9] <-"54"
TT_ED_batch2B_corr$subj[TT_ED_batch2B_corr$subj == 8] <-"50"
TT_ED_batch2B_corr$subj[TT_ED_batch2B_corr$subj == 7] <-"48"
TT_ED_batch2B_corr$subj[TT_ED_batch2B_corr$subj == 6] <-"46"
TT_ED_batch2B_corr$subj[TT_ED_batch2B_corr$subj == 5] <-"44"
TT_ED_batch2B_corr$subj[TT_ED_batch2B_corr$subj == 4] <-"43"
TT_ED_batch2B_corr$subj[TT_ED_batch2B_corr$subj == 3] <-"42"
TT_ED_batch2B_corr$subj[TT_ED_batch2B_corr$subj == 2] <-"40"
TT_ED_batch2B_corr$subj[TT_ED_batch2B_corr$subj == 1] <-"39"

#import the data set batch 3
TT_ED_batch3_corr <- read_csv("Duncans-Grant-master/TT_ED/TT_ED_batch3_corr.csv")

#Rename the participant numbers in the batches back to their original participant numbers.
TT_ED_batch3_corr$subj[TT_ED_batch3_corr$subj == 21] <-"99"
TT_ED_batch3_corr$subj[TT_ED_batch3_corr$subj == 20] <-"95"
TT_ED_batch3_corr$subj[TT_ED_batch3_corr$subj == 19] <-"92"
TT_ED_batch3_corr$subj[TT_ED_batch3_corr$subj == 18] <-"91"
TT_ED_batch3_corr$subj[TT_ED_batch3_corr$subj == 17] <-"89"
TT_ED_batch3_corr$subj[TT_ED_batch3_corr$subj == 16] <-"87"
TT_ED_batch3_corr$subj[TT_ED_batch3_corr$subj == 15] <-"86"
TT_ED_batch3_corr$subj[TT_ED_batch3_corr$subj == 14] <-"85"
TT_ED_batch3_corr$subj[TT_ED_batch3_corr$subj == 13] <-"84"
TT_ED_batch3_corr$subj[TT_ED_batch3_corr$subj == 12] <-"83"
TT_ED_batch3_corr$subj[TT_ED_batch3_corr$subj == 11] <-"82"
TT_ED_batch3_corr$subj[TT_ED_batch3_corr$subj == 10] <-"81"
TT_ED_batch3_corr$subj[TT_ED_batch3_corr$subj == 9] <-"80"
TT_ED_batch3_corr$subj[TT_ED_batch3_corr$subj == 8] <-"79"
TT_ED_batch3_corr$subj[TT_ED_batch3_corr$subj == 7] <-"78"
TT_ED_batch3_corr$subj[TT_ED_batch3_corr$subj == 6] <-"77"
TT_ED_batch3_corr$subj[TT_ED_batch3_corr$subj == 5] <-"76"
TT_ED_batch3_corr$subj[TT_ED_batch3_corr$subj == 4] <-"75"
TT_ED_batch3_corr$subj[TT_ED_batch3_corr$subj == 3] <-"74"
TT_ED_batch3_corr$subj[TT_ED_batch3_corr$subj == 2] <-"73"
TT_ED_batch3_corr$subj[TT_ED_batch3_corr$subj == 1] <-"72"

#import the data set batch 4
TT_ED_batch4_error <- read_csv("Duncans-Grant-master/TT_ED/TT_ED_batch4_error.csv")

#Rename the participant numbers in the batches back to their original participant numbers.
TT_ED_batch4_error$subj[TT_ED_batch4_error$subj == 20] <-"41"
TT_ED_batch4_error$subj[TT_ED_batch4_error$subj == 19] <-"37"
TT_ED_batch4_error$subj[TT_ED_batch4_error$subj == 18] <-"35"
TT_ED_batch4_error$subj[TT_ED_batch4_error$subj == 17] <-"33"
TT_ED_batch4_error$subj[TT_ED_batch4_error$subj == 16] <-"31"
TT_ED_batch4_error$subj[TT_ED_batch4_error$subj == 15] <-"29"
TT_ED_batch4_error$subj[TT_ED_batch4_error$subj == 14] <-"27"
TT_ED_batch4_error$subj[TT_ED_batch4_error$subj == 13] <-"25"
TT_ED_batch4_error$subj[TT_ED_batch4_error$subj == 12] <-"23"
TT_ED_batch4_error$subj[TT_ED_batch4_error$subj == 11] <-"21"
TT_ED_batch4_error$subj[TT_ED_batch4_error$subj == 10] <-"19"
TT_ED_batch4_error$subj[TT_ED_batch4_error$subj == 9] <-"17"
TT_ED_batch4_error$subj[TT_ED_batch4_error$subj == 8] <-"15"
TT_ED_batch4_error$subj[TT_ED_batch4_error$subj == 7] <-"13"
TT_ED_batch4_error$subj[TT_ED_batch4_error$subj == 6] <-"11"
TT_ED_batch4_error$subj[TT_ED_batch4_error$subj == 5] <-"9"
TT_ED_batch4_error$subj[TT_ED_batch4_error$subj == 4] <-"7"
TT_ED_batch4_error$subj[TT_ED_batch4_error$subj == 3] <-"5"
TT_ED_batch4_error$subj[TT_ED_batch4_error$subj == 2] <-"3"
TT_ED_batch4_error$subj[TT_ED_batch4_error$subj == 1] <-"1"

#import the data set batch 5
TT_ED_batch5_error <- read_csv("Duncans-Grant-master/TT_ED/TT_ED_batch5_error.csv")

#Rename the participant numbers in the batches back to their original participant numbers.
TT_ED_batch5_error$subj[TT_ED_batch5_error$subj == 19] <-"98"
TT_ED_batch5_error$subj[TT_ED_batch5_error$subj == 18] <-"97"
TT_ED_batch5_error$subj[TT_ED_batch5_error$subj == 17] <-"96"
TT_ED_batch5_error$subj[TT_ED_batch5_error$subj == 16] <-"94"
TT_ED_batch5_error$subj[TT_ED_batch5_error$subj == 15] <-"93"
TT_ED_batch5_error$subj[TT_ED_batch5_error$subj == 14] <-"90"
TT_ED_batch5_error$subj[TT_ED_batch5_error$subj == 13] <-"88"
TT_ED_batch5_error$subj[TT_ED_batch5_error$subj == 12] <-"67"
TT_ED_batch5_error$subj[TT_ED_batch5_error$subj == 11] <-"65"
TT_ED_batch5_error$subj[TT_ED_batch5_error$subj == 10] <-"63"
TT_ED_batch5_error$subj[TT_ED_batch5_error$subj == 9] <-"61"
TT_ED_batch5_error$subj[TT_ED_batch5_error$subj == 8] <-"59"
TT_ED_batch5_error$subj[TT_ED_batch5_error$subj == 7] <-"57"
TT_ED_batch5_error$subj[TT_ED_batch5_error$subj == 6] <-"55"
TT_ED_batch5_error$subj[TT_ED_batch5_error$subj == 5] <-"53"
TT_ED_batch5_error$subj[TT_ED_batch5_error$subj == 4] <-"51"
TT_ED_batch5_error$subj[TT_ED_batch5_error$subj == 3] <-"49"
TT_ED_batch5_error$subj[TT_ED_batch5_error$subj == 2] <-"47"
TT_ED_batch5_error$subj[TT_ED_batch5_error$subj == 1] <-"45"

#Double check it all looks right now
#view(TT_ED_batch1_corr)
#view(TT_ED_batch2B_corr)
#view(TT_ED_batch3_corr)
#view(TT_ED_batch4_error)
#view(TT_ED_batch5_error)

#Let's combine the data
all_data <- rbind(TT_ED_batch1_corr, TT_ED_batch2B_corr, TT_ED_batch3_corr, TT_ED_batch4_error, 
                  TT_ED_batch5_error)

all_data$subj <- as.factor(all_data$subj)


#Import Individual difference measures
ID_Measures <- read_csv("ID Measures.csv")
#View(ID_Measures)

# Rename Participabt in ID_measures to subj to be the same as current data set
ID_Measures <- rename(ID_Measures, subj = Participant)
ID_Measures$subj <- as.factor(ID_Measures$subj)

#Remove participants with missing data
ID_Measures_removed <- ID_Measures %>% filter(subj != 16)
ID_Measures_removed <- ID_Measures_removed %>% filter(subj != 67)
ID_Measures_removed <- ID_Measures_removed %>% filter(subj != 34)
ID_Measures_removed <- ID_Measures_removed %>% filter(subj != 39)
ID_Measures_removed <- ID_Measures_removed %>% filter(subj != 42)
ID_Measures_removed <- ID_Measures_removed %>% filter(subj != 43)
ID_Measures_removed <- ID_Measures_removed %>% filter(subj != 44)
ID_Measures_removed <- ID_Measures_removed %>% filter(subj != 45)
ID_Measures_removed <- ID_Measures_removed %>% filter(subj != 69)
ID_Measures_removed <- ID_Measures_removed %>% filter(subj != 70)
ID_Measures_removed <- ID_Measures_removed %>% filter(subj != 71)
ID_Measures_removed <- ID_Measures_removed %>% filter(subj != 72)
ID_Measures_removed <- ID_Measures_removed %>% filter(subj != 73)
ID_Measures_removed <- ID_Measures_removed %>% filter(subj != 74)
ID_Measures_removed <- ID_Measures_removed %>% filter(subj != 75)
ID_Measures_removed <- ID_Measures_removed %>% filter(subj != 76)
ID_Measures_removed <- ID_Measures_removed %>% filter(subj != 77)
ID_Measures_removed <- ID_Measures_removed %>% filter(subj != 78)
ID_Measures_removed <- ID_Measures_removed %>% filter(subj != 79)
ID_Measures_removed <- ID_Measures_removed %>% filter(subj != 80)
ID_Measures_removed <- ID_Measures_removed %>% filter(subj != 81)
ID_Measures_removed <- ID_Measures_removed %>% filter(subj != 82)
ID_Measures_removed <- ID_Measures_removed %>% filter(subj != 83)
ID_Measures_removed <- ID_Measures_removed %>% filter(subj != 84)
ID_Measures_removed <- ID_Measures_removed %>% filter(subj != 85)
ID_Measures_removed <- ID_Measures_removed %>% filter(subj != 86)
ID_Measures_removed <- ID_Measures_removed %>% filter(subj != 87)

#remove Participant who we do not have ID data for.
all_data_removed <- all_data %>% filter(subj != 16)
all_data_removed <- all_data_removed %>% filter(subj != 34)
all_data_removed <- all_data_removed %>% filter(subj != 39)
all_data_removed <- all_data_removed %>% filter(subj != 42)
all_data_removed <- all_data_removed %>% filter(subj != 43)
all_data_removed <- all_data_removed %>% filter(subj != 44)
all_data_removed <- all_data_removed %>% filter(subj != 45)
all_data_removed <- all_data_removed %>% filter(subj != 67)
all_data_removed <- all_data_removed %>% filter(subj != 69)
all_data_removed <- all_data_removed %>% filter(subj != 70)
all_data_removed <- all_data_removed %>% filter(subj != 71)
all_data_removed <- all_data_removed %>% filter(subj != 72)
all_data_removed <- all_data_removed %>% filter(subj != 73)
all_data_removed <- all_data_removed %>% filter(subj != 74)
all_data_removed <- all_data_removed %>% filter(subj != 75)
all_data_removed <- all_data_removed %>% filter(subj != 76)
all_data_removed <- all_data_removed %>% filter(subj != 77)
all_data_removed <- all_data_removed %>% filter(subj != 78)
all_data_removed <- all_data_removed %>% filter(subj != 79)
all_data_removed <- all_data_removed %>% filter(subj != 80)
all_data_removed <- all_data_removed %>% filter(subj != 81)
all_data_removed <- all_data_removed %>% filter(subj != 82)
all_data_removed <- all_data_removed %>% filter(subj != 83)
all_data_removed <- all_data_removed %>% filter(subj != 84)
all_data_removed <- all_data_removed %>% filter(subj != 85)
all_data_removed <- all_data_removed %>% filter(subj != 86)
all_data_removed <- all_data_removed %>% filter(subj != 87)                                       
all_data_removed <- all_data_removed %>% filter(subj != 88)    
all_data_removed <- all_data_removed %>% filter(subj != 89)    
all_data_removed <- all_data_removed %>% filter(subj != 90)    
all_data_removed <- all_data_removed %>% filter(subj != 91)    
all_data_removed <- all_data_removed %>% filter(subj != 92)    
all_data_removed <- all_data_removed %>% filter(subj != 93)    
all_data_removed <- all_data_removed %>% filter(subj != 94)    
all_data_removed <- all_data_removed %>% filter(subj != 95)    
all_data_removed <- all_data_removed %>% filter(subj != 96)    
all_data_removed <- all_data_removed %>% filter(subj != 97)    
all_data_removed <- all_data_removed %>% filter(subj != 98)    
all_data_removed <- all_data_removed %>% filter(subj != 99)    

# Add the ID's to the data frame
all_data_join <- inner_join(all_data_removed, ID_Measures_removed, by = "subj")

#Now that we've got a full data set ready to be analysed let's first do some
# visualisation on the data to get a feel for how it is looking as well as the appropriate method.
# condition labels, 1 = prediction facilitated, 2 = prediction unfacilitated 

all_data_join$cond <- recode(all_data_join$cond, "1" = "facilitated", "2" = "unfacilitated")

all_data_join$cond <- as.factor(all_data_join$cond)

#Let's start with Region 4 "the question"
# Throw away zeroes
all_data_join <- all_data_join %>% filter(R4 != 0)

# Visualise
all_data_join %>% 
  ggplot(aes(x = cond, y = R4, colour = cond)) +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(colour = FALSE)

all_data_join %>% 
  ggplot(aes(x = cond, y = R4, colour = cond)) + ggtitle("Total Time for Critical Region: Question") +
  labs(y = "Reading time in ms.", x = "Prediction") +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(colour = FALSE)

all_data_join %>% 
  group_by(cond) %>%
  summarise(mean(R4), sd(R4))

# Model assuming normality of residuals 
modelR4.null <- lmer(R4 ~ (1 + cond | subj) + (1 + cond | item), all_data_join) 
modelR4 <- lmer(R4 ~ cond + (1 + cond | subj) + (1 + cond | item), all_data_join) 

summary(modelR4)
anova(modelR4, modelR4.null)

qqnorm(residuals(modelR4))
qqline(residuals(modelR4))
descdist(all_data_join$R4)

#Let's include some covariates! Region 4

#Step 1: Scale the ID measures...
all_data_join$SRS2_total_score_t <- scale(all_data_join$SRS2_total_score_t)
all_data_join$EQ <- scale(all_data_join$EQ)
all_data_join$WRMT_total_reading_score <- scale(all_data_join$WRMT_total_reading_score)
all_data_join$WRMT_WI_raw <- scale(all_data_join$WRMT_WI_raw)

# Maximal model 
model_alldata_ranefR4 <- lmer(R4 ~ cond + SRS2_total_score_t + EQ + WRMT_total_reading_score + WRMT_WI_raw +
                                (1 + cond | subj) +  (1 + cond | item) , data = all_data_join, REML = TRUE)

#same structure as model fails to converge so settle with singular fit
model_alldataR4_null <- lmer(R4 ~ SRS2_total_score_t + EQ + WRMT_total_reading_score + WRMT_WI_raw +
                                (1 + cond | subj) +  (1 + cond | item) , data = all_data_join, REML = TRUE)

summary(model_alldata_ranefR4)
anova(model_alldataR4_null, model_alldata_ranefR4)
check_model(model_alldata_ranefR4)
# Which model is best? with controlling factors or just eye tracking data
anova(modelR4, model_alldata_ranefR4)

#Just one correlate
model_alldata_ranefR4 <- lmer(R4 ~ cond * SRS2_total_score_t +
                                (1 + cond | subj) +  (1 + cond | item) , data = all_data_join, REML = TRUE)
summary(model_alldata_ranefR4)

model_alldata_ranefR42 <- lmer(R4 ~ cond +
                                 (1 + cond | subj) +  (1 + cond | item) , data = all_data_join, REML = TRUE)
summary(model_alldata_ranefR42)
anova(model_alldata_ranefR4, model_alldata_ranefR42)

#What does region 5 "the reply" look like?

all_data_join <- all_data_join %>% filter(R5 != 0)

# Visualise
all_data_join %>% 
  ggplot(aes(x = cond, y = R5, colour = cond)) +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(colour = FALSE)

all_data_join %>% 
  ggplot(aes(x = cond, y = R5, colour = cond)) + ggtitle("Total Time for Post-Critical Region: Reply") +
  labs(y = "Reading time in ms.", x = "Prediction") +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(colour = FALSE)

all_data_join %>% 
  group_by(cond) %>%
  summarise(mean(R5), sd(R5))

modelR5.null <- lmer(R5 ~ (1 + cond | subj) + (1 + cond | item), all_data_join) 
modelR5 <- lmer(R5 ~ cond + (1 + cond | subj) + (1 + cond | item), all_data_join) 
summary(modelR5)
anova(modelR5, modelR5.null)

check_model(modelR5)
qqnorm(residuals(modelR5))
qqline(residuals(modelR5))
descdist(all_data_join$R5)

#Let's include some covariates! Region 5

# IGNORING SINGULAR FIT 
model_alldata_ranefR5 <- lmer(R5 ~ cond + SRS2_total_score_t + EQ + WRMT_total_reading_score + WRMT_WI_raw +
                                      (1 | subj) +  (1 + cond | item) , data = all_data_join, REML = TRUE)

model_alldata_R5_null <- lmer(R5 ~ SRS2_total_score_t + EQ + WRMT_total_reading_score + WRMT_WI_raw +
                                     (1 + cond | subj) +  (1 + cond | item) , data = all_data_join, REML = TRUE)

anova(model_alldata_R5_null, model_alldata_ranefR5)
summary(model_alldata_ranefR5)
check_model(model_alldata_ranefR5)

anova(modelR5, model_alldata_ranefR5)
