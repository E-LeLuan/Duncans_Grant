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

#import the data set batch 1
FP_ED_batch1_corr <- read_csv("Duncans-Grant-master/FP_ED/FP_ED_batch1_corr.csv")

#Rename the participant numbers in the batches back to their original participant numbers.
FP_ED_batch1_corr$subj[FP_ED_batch1_corr$subj == 20] <-"38"
FP_ED_batch1_corr$subj[FP_ED_batch1_corr$subj == 19] <-"36"
FP_ED_batch1_corr$subj[FP_ED_batch1_corr$subj == 18] <-"35"
FP_ED_batch1_corr$subj[FP_ED_batch1_corr$subj == 17] <-"34"
FP_ED_batch1_corr$subj[FP_ED_batch1_corr$subj == 16] <-"32"
FP_ED_batch1_corr$subj[FP_ED_batch1_corr$subj == 15] <-"30"
FP_ED_batch1_corr$subj[FP_ED_batch1_corr$subj == 14] <-"28"
FP_ED_batch1_corr$subj[FP_ED_batch1_corr$subj == 13] <-"26"
FP_ED_batch1_corr$subj[FP_ED_batch1_corr$subj == 12] <-"24"
FP_ED_batch1_corr$subj[FP_ED_batch1_corr$subj == 11] <-"22"
FP_ED_batch1_corr$subj[FP_ED_batch1_corr$subj == 10] <-"20"
FP_ED_batch1_corr$subj[FP_ED_batch1_corr$subj == 9] <-"18"
FP_ED_batch1_corr$subj[FP_ED_batch1_corr$subj == 8] <-"16"
FP_ED_batch1_corr$subj[FP_ED_batch1_corr$subj == 7] <-"14"
FP_ED_batch1_corr$subj[FP_ED_batch1_corr$subj == 6] <-"12"
FP_ED_batch1_corr$subj[FP_ED_batch1_corr$subj == 5] <-"10"
FP_ED_batch1_corr$subj[FP_ED_batch1_corr$subj == 4] <-"8"
FP_ED_batch1_corr$subj[FP_ED_batch1_corr$subj == 3] <-"6"
FP_ED_batch1_corr$subj[FP_ED_batch1_corr$subj == 2] <-"4"
FP_ED_batch1_corr$subj[FP_ED_batch1_corr$subj == 1] <-"2"

#import the data set batch 2B
FP_ED_batch2B_corr <- read_csv("Duncans-Grant-master/FP_ED/FP_ED_batch2B_corr.csv")

#Rename the participant numbers in the batches back to their original participant numbers.
FP_ED_batch2B_corr$subj[FP_ED_batch2B_corr$subj == 19] <-"71"
FP_ED_batch2B_corr$subj[FP_ED_batch2B_corr$subj == 18] <-"70"
FP_ED_batch2B_corr$subj[FP_ED_batch2B_corr$subj == 17] <-"69"
FP_ED_batch2B_corr$subj[FP_ED_batch2B_corr$subj == 16] <-"68"
FP_ED_batch2B_corr$subj[FP_ED_batch2B_corr$subj == 15] <-"66"
FP_ED_batch2B_corr$subj[FP_ED_batch2B_corr$subj == 14] <-"64"
FP_ED_batch2B_corr$subj[FP_ED_batch2B_corr$subj == 13] <-"62"
FP_ED_batch2B_corr$subj[FP_ED_batch2B_corr$subj == 12] <-"60"
FP_ED_batch2B_corr$subj[FP_ED_batch2B_corr$subj == 11] <-"58"
FP_ED_batch2B_corr$subj[FP_ED_batch2B_corr$subj == 10] <-"56"
FP_ED_batch2B_corr$subj[FP_ED_batch2B_corr$subj == 9] <-"54"
FP_ED_batch2B_corr$subj[FP_ED_batch2B_corr$subj == 8] <-"50"
FP_ED_batch2B_corr$subj[FP_ED_batch2B_corr$subj == 7] <-"48"
FP_ED_batch2B_corr$subj[FP_ED_batch2B_corr$subj == 6] <-"46"
FP_ED_batch2B_corr$subj[FP_ED_batch2B_corr$subj == 5] <-"44"
FP_ED_batch2B_corr$subj[FP_ED_batch2B_corr$subj == 4] <-"43"
FP_ED_batch2B_corr$subj[FP_ED_batch2B_corr$subj == 3] <-"42"
FP_ED_batch2B_corr$subj[FP_ED_batch2B_corr$subj == 2] <-"40"
FP_ED_batch2B_corr$subj[FP_ED_batch2B_corr$subj == 1] <-"39"

#import the data set batch 3
FP_ED_batch3_corr <- read_csv("Duncans-Grant-master/FP_ED/FP_ED_batch3_corr.csv")

#Rename the participant numbers in the batches back to their original participant numbers.
FP_ED_batch3_corr$subj[FP_ED_batch3_corr$subj == 21] <-"99"
FP_ED_batch3_corr$subj[FP_ED_batch3_corr$subj == 20] <-"95"
FP_ED_batch3_corr$subj[FP_ED_batch3_corr$subj == 19] <-"92"
FP_ED_batch3_corr$subj[FP_ED_batch3_corr$subj == 18] <-"91"
FP_ED_batch3_corr$subj[FP_ED_batch3_corr$subj == 17] <-"89"
FP_ED_batch3_corr$subj[FP_ED_batch3_corr$subj == 16] <-"87"
FP_ED_batch3_corr$subj[FP_ED_batch3_corr$subj == 15] <-"86"
FP_ED_batch3_corr$subj[FP_ED_batch3_corr$subj == 14] <-"85"
FP_ED_batch3_corr$subj[FP_ED_batch3_corr$subj == 13] <-"84"
FP_ED_batch3_corr$subj[FP_ED_batch3_corr$subj == 12] <-"83"
FP_ED_batch3_corr$subj[FP_ED_batch3_corr$subj == 11] <-"82"
FP_ED_batch3_corr$subj[FP_ED_batch3_corr$subj == 10] <-"81"
FP_ED_batch3_corr$subj[FP_ED_batch3_corr$subj == 9] <-"80"
FP_ED_batch3_corr$subj[FP_ED_batch3_corr$subj == 8] <-"79"
FP_ED_batch3_corr$subj[FP_ED_batch3_corr$subj == 7] <-"78"
FP_ED_batch3_corr$subj[FP_ED_batch3_corr$subj == 6] <-"77"
FP_ED_batch3_corr$subj[FP_ED_batch3_corr$subj == 5] <-"76"
FP_ED_batch3_corr$subj[FP_ED_batch3_corr$subj == 4] <-"75"
FP_ED_batch3_corr$subj[FP_ED_batch3_corr$subj == 3] <-"74"
FP_ED_batch3_corr$subj[FP_ED_batch3_corr$subj == 2] <-"73"
FP_ED_batch3_corr$subj[FP_ED_batch3_corr$subj == 1] <-"72"

#import the data set batch 4
FP_ED_batch4_error <- read_csv("Duncans-Grant-master/FP_ED/FP_ED_batch4_error.csv")

#Rename the participant numbers in the batches back to their original participant numbers.
FP_ED_batch4_error$subj[FP_ED_batch4_error$subj == 20] <-"41"
FP_ED_batch4_error$subj[FP_ED_batch4_error$subj == 19] <-"37"
FP_ED_batch4_error$subj[FP_ED_batch4_error$subj == 18] <-"35"
FP_ED_batch4_error$subj[FP_ED_batch4_error$subj == 17] <-"33"
FP_ED_batch4_error$subj[FP_ED_batch4_error$subj == 16] <-"31"
FP_ED_batch4_error$subj[FP_ED_batch4_error$subj == 15] <-"29"
FP_ED_batch4_error$subj[FP_ED_batch4_error$subj == 14] <-"27"
FP_ED_batch4_error$subj[FP_ED_batch4_error$subj == 13] <-"25"
FP_ED_batch4_error$subj[FP_ED_batch4_error$subj == 12] <-"23"
FP_ED_batch4_error$subj[FP_ED_batch4_error$subj == 11] <-"21"
FP_ED_batch4_error$subj[FP_ED_batch4_error$subj == 10] <-"19"
FP_ED_batch4_error$subj[FP_ED_batch4_error$subj == 9] <-"17"
FP_ED_batch4_error$subj[FP_ED_batch4_error$subj == 8] <-"15"
FP_ED_batch4_error$subj[FP_ED_batch4_error$subj == 7] <-"13"
FP_ED_batch4_error$subj[FP_ED_batch4_error$subj == 6] <-"11"
FP_ED_batch4_error$subj[FP_ED_batch4_error$subj == 5] <-"9"
FP_ED_batch4_error$subj[FP_ED_batch4_error$subj == 4] <-"7"
FP_ED_batch4_error$subj[FP_ED_batch4_error$subj == 3] <-"5"
FP_ED_batch4_error$subj[FP_ED_batch4_error$subj == 2] <-"3"
FP_ED_batch4_error$subj[FP_ED_batch4_error$subj == 1] <-"1"

#import the data set batch 5
FP_ED_batch5_error <- read_csv("Duncans-Grant-master/FP_ED/FP_ED_batch5_error.csv")

#Rename the participant numbers in the batches back to their original participant numbers.
FP_ED_batch5_error$subj[FP_ED_batch5_error$subj == 19] <-"98"
FP_ED_batch5_error$subj[FP_ED_batch5_error$subj == 18] <-"97"
FP_ED_batch5_error$subj[FP_ED_batch5_error$subj == 17] <-"96"
FP_ED_batch5_error$subj[FP_ED_batch5_error$subj == 16] <-"94"
FP_ED_batch5_error$subj[FP_ED_batch5_error$subj == 15] <-"93"
FP_ED_batch5_error$subj[FP_ED_batch5_error$subj == 14] <-"90"
FP_ED_batch5_error$subj[FP_ED_batch5_error$subj == 13] <-"88"
FP_ED_batch5_error$subj[FP_ED_batch5_error$subj == 12] <-"67"
FP_ED_batch5_error$subj[FP_ED_batch5_error$subj == 11] <-"65"
FP_ED_batch5_error$subj[FP_ED_batch5_error$subj == 10] <-"63"
FP_ED_batch5_error$subj[FP_ED_batch5_error$subj == 9] <-"61"
FP_ED_batch5_error$subj[FP_ED_batch5_error$subj == 8] <-"59"
FP_ED_batch5_error$subj[FP_ED_batch5_error$subj == 7] <-"57"
FP_ED_batch5_error$subj[FP_ED_batch5_error$subj == 6] <-"55"
FP_ED_batch5_error$subj[FP_ED_batch5_error$subj == 5] <-"53"
FP_ED_batch5_error$subj[FP_ED_batch5_error$subj == 4] <-"51"
FP_ED_batch5_error$subj[FP_ED_batch5_error$subj == 3] <-"49"
FP_ED_batch5_error$subj[FP_ED_batch5_error$subj == 2] <-"47"
FP_ED_batch5_error$subj[FP_ED_batch5_error$subj == 1] <-"45"

#Double check it all looks right now
#view(FP_ED_batch1_corr)
#view(FP_ED_batch2B_corr)
#view(FP_ED_batch3_corr)
#view(FP_ED_batch4_error)
#view(FP_ED_batch5_error)

#Let's combine the data
all_data <- rbind(FP_ED_batch1_corr, FP_ED_batch2B_corr, FP_ED_batch3_corr, FP_ED_batch4_error, 
                  FP_ED_batch5_error)

view(all_data)
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
view(ID_Measures_removed)

#remove Participant who we do not have ID data for.
all_data_removed <- all_data %>% filter(subj != 16)
all_data_removed <- all_data_removed %>% filter(subj != 34)
all_data_removed <- all_data_removed %>% filter(subj != 39)
all_data_removed <- all_data_removed %>% filter(subj != 42)
all_data_removed <- all_data_removed %>% filter(subj != 43)
all_data_removed <- all_data_removed %>% filter(subj != 44)
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

view(all_data_removed)

#Let's order the data by subject THIS DOES NOT WORK FOR SOME REAOSN NOT SURE WHY?
#all_data_removed$subj <- as.factor(all_data_removed$subj)
#all_data_removed_sort <- all_data_removed[order(subj),]

# Add the ID's to the data frame
all_data_join <- inner_join(all_data_removed, ID_Measures_removed, by = "subj")
view(all_data_join)

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
  group_by(cond) %>%
  summarise(mean(R4), sd(R4))

maximal <- lmer(R4 ~ cond + SRS2_total_score_t + EQ + WRMT_total_reading_score + WRMT_WI_raw +
                                   (1 + cond | subj) + (1 + cond | item) + (0 + SRS2_total_score_t|cond) + 
                                   (0 + EQ|cond) + (0 + WRMT_total_reading_score|cond) + 
                                   (0 + WRMT_WI_raw|cond), data = all_data_join)

check_model(maximal)

m_gamma <- buildmer(maximal,
                    data = all_data_join,
                    direction = 'order',
                    family = Gamma(link = "identity"),
                    control = glmerControl(optimizer = 'bobyqa'))

maximal_gamma <- formula(m_gamma@model)
maximal_gamma
model1a <- glmer(maximal_gamma, 
                 family = Gamma(link = "identity"), 
                 data = data1)

# Model assuming normality of residuals - singular fit error with more complex models
model.null <- lmer(R4 ~ (1 | subj) + (1 + cond | item), all_data_join) 
model <- lmer(R4 ~ cond + (1 | subj) + (1 + cond | item), all_data_join) 
summary(model)

anova(model, model.null)

qqnorm(residuals(model))
qqline(residuals(model))

descdist(all_data_join$R4)

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
  group_by(cond) %>%
  summarise(mean(R5), sd(R5))

# Model assuming normality of residuals - singular fit error with more complex models
model.null <- lmer(R5 ~ (1 | subj) + (1 + cond | item), all_data_join) 
model <- lmer(R5 ~ cond + (1 | subj) + (1 + cond | item), all_data_join) 
summary(model)

anova(model, model.null)

qqnorm(residuals(model))
qqline(residuals(model))

descdist(all_data_join$R5)

#Let's include some covariates! Region 4

#Step 1: Scale the ID measures...
all_data_join$SRS2_total_score_t <- scale(all_data_join$SRS2_total_score_t)
all_data_join$EQ <- scale(all_data_join$EQ)
all_data_join$WRMT_total_reading_score <- scale(all_data_join$WRMT_total_reading_score)
all_data_join$WRMT_WI_raw <- scale(all_data_join$WRMT_WI_raw)

# SINGULAR FIT 
model_alldata <- lmer(R4 ~ cond + SRS2_total_score_t + EQ + WRMT_total_reading_score + WRMT_WI_raw +
                        (1 |subj) + (0 + SRS2_total_score_t|cond) + 
                        (0 + EQ|cond) + (0 + WRMT_total_reading_score|cond) + 
                        (0 + WRMT_WI_raw|cond) , all_data_join, REML = TRUE)

#Try again
#Simplified it loads and cannot get it to converge!
