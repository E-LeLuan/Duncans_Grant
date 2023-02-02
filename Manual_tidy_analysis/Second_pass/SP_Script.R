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

# Analysis of First Pass data

#import the data set batch 1
SP_ED_batch1_corr <- read_csv("Duncans-Grant-master/SP_ED/SP_ED_batch1_corr.csv")

#Rename the participant numbers in the batches back to their original participant numbers.
SP_ED_batch1_corr$subj[SP_ED_batch1_corr$subj == 20] <-"38"
SP_ED_batch1_corr$subj[SP_ED_batch1_corr$subj == 19] <-"36"
SP_ED_batch1_corr$subj[SP_ED_batch1_corr$subj == 18] <-"35"
SP_ED_batch1_corr$subj[SP_ED_batch1_corr$subj == 17] <-"34"
SP_ED_batch1_corr$subj[SP_ED_batch1_corr$subj == 16] <-"32"
SP_ED_batch1_corr$subj[SP_ED_batch1_corr$subj == 15] <-"30"
SP_ED_batch1_corr$subj[SP_ED_batch1_corr$subj == 14] <-"28"
SP_ED_batch1_corr$subj[SP_ED_batch1_corr$subj == 13] <-"26"
SP_ED_batch1_corr$subj[SP_ED_batch1_corr$subj == 12] <-"24"
SP_ED_batch1_corr$subj[SP_ED_batch1_corr$subj == 11] <-"22"
SP_ED_batch1_corr$subj[SP_ED_batch1_corr$subj == 10] <-"20"
SP_ED_batch1_corr$subj[SP_ED_batch1_corr$subj == 9] <-"18"
SP_ED_batch1_corr$subj[SP_ED_batch1_corr$subj == 8] <-"16"
SP_ED_batch1_corr$subj[SP_ED_batch1_corr$subj == 7] <-"14"
SP_ED_batch1_corr$subj[SP_ED_batch1_corr$subj == 6] <-"12"
SP_ED_batch1_corr$subj[SP_ED_batch1_corr$subj == 5] <-"10"
SP_ED_batch1_corr$subj[SP_ED_batch1_corr$subj == 4] <-"8"
SP_ED_batch1_corr$subj[SP_ED_batch1_corr$subj == 3] <-"6"
SP_ED_batch1_corr$subj[SP_ED_batch1_corr$subj == 2] <-"4"
SP_ED_batch1_corr$subj[SP_ED_batch1_corr$subj == 1] <-"2"

#import the data set batch 2B
SP_ED_batch2B_corr <- read_csv("Duncans-Grant-master/SP_ED/SP_ED_batch2B_corr.csv")

#Rename the participant numbers in the batches back to their original participant numbers.
SP_ED_batch2B_corr$subj[SP_ED_batch2B_corr$subj == 19] <-"71"
SP_ED_batch2B_corr$subj[SP_ED_batch2B_corr$subj == 18] <-"70"
SP_ED_batch2B_corr$subj[SP_ED_batch2B_corr$subj == 17] <-"69"
SP_ED_batch2B_corr$subj[SP_ED_batch2B_corr$subj == 16] <-"68"
SP_ED_batch2B_corr$subj[SP_ED_batch2B_corr$subj == 15] <-"66"
SP_ED_batch2B_corr$subj[SP_ED_batch2B_corr$subj == 14] <-"64"
SP_ED_batch2B_corr$subj[SP_ED_batch2B_corr$subj == 13] <-"62"
SP_ED_batch2B_corr$subj[SP_ED_batch2B_corr$subj == 12] <-"60"
SP_ED_batch2B_corr$subj[SP_ED_batch2B_corr$subj == 11] <-"58"
SP_ED_batch2B_corr$subj[SP_ED_batch2B_corr$subj == 10] <-"56"
SP_ED_batch2B_corr$subj[SP_ED_batch2B_corr$subj == 9] <-"54"
SP_ED_batch2B_corr$subj[SP_ED_batch2B_corr$subj == 8] <-"50"
SP_ED_batch2B_corr$subj[SP_ED_batch2B_corr$subj == 7] <-"48"
SP_ED_batch2B_corr$subj[SP_ED_batch2B_corr$subj == 6] <-"46"
SP_ED_batch2B_corr$subj[SP_ED_batch2B_corr$subj == 5] <-"44"
SP_ED_batch2B_corr$subj[SP_ED_batch2B_corr$subj == 4] <-"43"
SP_ED_batch2B_corr$subj[SP_ED_batch2B_corr$subj == 3] <-"42"
SP_ED_batch2B_corr$subj[SP_ED_batch2B_corr$subj == 2] <-"40"
SP_ED_batch2B_corr$subj[SP_ED_batch2B_corr$subj == 1] <-"39"

#import the data set batch 3
SP_ED_batch3_corr <- read_csv("Duncans-Grant-master/SP_ED/SP_ED_batch3_corr.csv")

#Rename the participant numbers in the batches back to their original participant numbers.
SP_ED_batch3_corr$subj[SP_ED_batch3_corr$subj == 21] <-"99"
SP_ED_batch3_corr$subj[SP_ED_batch3_corr$subj == 20] <-"95"
SP_ED_batch3_corr$subj[SP_ED_batch3_corr$subj == 19] <-"92"
SP_ED_batch3_corr$subj[SP_ED_batch3_corr$subj == 18] <-"91"
SP_ED_batch3_corr$subj[SP_ED_batch3_corr$subj == 17] <-"89"
SP_ED_batch3_corr$subj[SP_ED_batch3_corr$subj == 16] <-"87"
SP_ED_batch3_corr$subj[SP_ED_batch3_corr$subj == 15] <-"86"
SP_ED_batch3_corr$subj[SP_ED_batch3_corr$subj == 14] <-"85"
SP_ED_batch3_corr$subj[SP_ED_batch3_corr$subj == 13] <-"84"
SP_ED_batch3_corr$subj[SP_ED_batch3_corr$subj == 12] <-"83"
SP_ED_batch3_corr$subj[SP_ED_batch3_corr$subj == 11] <-"82"
SP_ED_batch3_corr$subj[SP_ED_batch3_corr$subj == 10] <-"81"
SP_ED_batch3_corr$subj[SP_ED_batch3_corr$subj == 9] <-"80"
SP_ED_batch3_corr$subj[SP_ED_batch3_corr$subj == 8] <-"79"
SP_ED_batch3_corr$subj[SP_ED_batch3_corr$subj == 7] <-"78"
SP_ED_batch3_corr$subj[SP_ED_batch3_corr$subj == 6] <-"77"
SP_ED_batch3_corr$subj[SP_ED_batch3_corr$subj == 5] <-"76"
SP_ED_batch3_corr$subj[SP_ED_batch3_corr$subj == 4] <-"75"
SP_ED_batch3_corr$subj[SP_ED_batch3_corr$subj == 3] <-"74"
SP_ED_batch3_corr$subj[SP_ED_batch3_corr$subj == 2] <-"73"
SP_ED_batch3_corr$subj[SP_ED_batch3_corr$subj == 1] <-"72"

#import the data set batch 4
SP_ED_batch4_error <- read_csv("Duncans-Grant-master/SP_ED/SP_ED_batch4_error.csv")

#Rename the participant numbers in the batches back to their original participant numbers.
SP_ED_batch4_error$subj[SP_ED_batch4_error$subj == 20] <-"41"
SP_ED_batch4_error$subj[SP_ED_batch4_error$subj == 19] <-"37"
SP_ED_batch4_error$subj[SP_ED_batch4_error$subj == 18] <-"35"
SP_ED_batch4_error$subj[SP_ED_batch4_error$subj == 17] <-"33"
SP_ED_batch4_error$subj[SP_ED_batch4_error$subj == 16] <-"31"
SP_ED_batch4_error$subj[SP_ED_batch4_error$subj == 15] <-"29"
SP_ED_batch4_error$subj[SP_ED_batch4_error$subj == 14] <-"27"
SP_ED_batch4_error$subj[SP_ED_batch4_error$subj == 13] <-"25"
SP_ED_batch4_error$subj[SP_ED_batch4_error$subj == 12] <-"23"
SP_ED_batch4_error$subj[SP_ED_batch4_error$subj == 11] <-"21"
SP_ED_batch4_error$subj[SP_ED_batch4_error$subj == 10] <-"19"
SP_ED_batch4_error$subj[SP_ED_batch4_error$subj == 9] <-"17"
SP_ED_batch4_error$subj[SP_ED_batch4_error$subj == 8] <-"15"
SP_ED_batch4_error$subj[SP_ED_batch4_error$subj == 7] <-"13"
SP_ED_batch4_error$subj[SP_ED_batch4_error$subj == 6] <-"11"
SP_ED_batch4_error$subj[SP_ED_batch4_error$subj == 5] <-"9"
SP_ED_batch4_error$subj[SP_ED_batch4_error$subj == 4] <-"7"
SP_ED_batch4_error$subj[SP_ED_batch4_error$subj == 3] <-"5"
SP_ED_batch4_error$subj[SP_ED_batch4_error$subj == 2] <-"3"
SP_ED_batch4_error$subj[SP_ED_batch4_error$subj == 1] <-"1"

#import the data set batch 5
SP_ED_batch5_error <- read_csv("Duncans-Grant-master/SP_ED/SP_ED_batch5_error.csv")

#Rename the participant numbers in the batches back to their original participant numbers.
SP_ED_batch5_error$subj[SP_ED_batch5_error$subj == 19] <-"98"
SP_ED_batch5_error$subj[SP_ED_batch5_error$subj == 18] <-"97"
SP_ED_batch5_error$subj[SP_ED_batch5_error$subj == 17] <-"96"
SP_ED_batch5_error$subj[SP_ED_batch5_error$subj == 16] <-"94"
SP_ED_batch5_error$subj[SP_ED_batch5_error$subj == 15] <-"93"
SP_ED_batch5_error$subj[SP_ED_batch5_error$subj == 14] <-"90"
SP_ED_batch5_error$subj[SP_ED_batch5_error$subj == 13] <-"88"
SP_ED_batch5_error$subj[SP_ED_batch5_error$subj == 12] <-"67"
SP_ED_batch5_error$subj[SP_ED_batch5_error$subj == 11] <-"65"
SP_ED_batch5_error$subj[SP_ED_batch5_error$subj == 10] <-"63"
SP_ED_batch5_error$subj[SP_ED_batch5_error$subj == 9] <-"61"
SP_ED_batch5_error$subj[SP_ED_batch5_error$subj == 8] <-"59"
SP_ED_batch5_error$subj[SP_ED_batch5_error$subj == 7] <-"57"
SP_ED_batch5_error$subj[SP_ED_batch5_error$subj == 6] <-"55"
SP_ED_batch5_error$subj[SP_ED_batch5_error$subj == 5] <-"53"
SP_ED_batch5_error$subj[SP_ED_batch5_error$subj == 4] <-"51"
SP_ED_batch5_error$subj[SP_ED_batch5_error$subj == 3] <-"49"
SP_ED_batch5_error$subj[SP_ED_batch5_error$subj == 2] <-"47"
SP_ED_batch5_error$subj[SP_ED_batch5_error$subj == 1] <-"45"

#Double check it all looks right now
#view(SP_ED_batch1_corr)
#view(SP_ED_batch2B_corr)
#view(SP_ED_batch3_corr)
#view(SP_ED_batch4_error)
#view(SP_ED_batch5_error)

#Let's combine the data
all_data <- rbind(SP_ED_batch1_corr, SP_ED_batch2B_corr, SP_ED_batch3_corr, SP_ED_batch4_error, 
                  SP_ED_batch5_error)

all_data$subj <- as.factor(all_data$subj)


#Import Individual difference measures
#Import Individual difference measures
ID_Measures <- read_csv("Other_data_and_information/All_IDs.csv")
#View(All_IDs)
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
  group_by(cond) %>%
  summarise(mean(R4), sd(R4))

# Model assuming normality of residuals - singular fit error with more complex models
model.null <- lmer(R4 ~ (1 | subj) + (1 | item), all_data_join) 
# NO MODEL WILL CONVERGE FOR R4 Second Pass reading times
#anova(model, model.null)
#check_model(model)
#qqnorm(residuals(model))
#qqline(residuals(model))
#descdist(all_data_join$R4)

#so lets try removing outliers
library(ggstatsplot)
#You can cite this package as:
#  Patil, I. (2021). Visualizations with statistical details: The 'ggstatsplot' approach.
#Journal of Open Source Software, 6(61), 3167, doi:10.21105/joss.03167

#ggbetweenstats(all_data_join, condition_number, RT2ms, outlier.tagging = TRUE)
Q <- quantile(all_data_join$R4, probs=c(.25, .75), na.rm = FALSE)
#view(Q)
iqr <- IQR(all_data_join$R4)
up <-  Q[2]+2.0*iqr # Upper Range  
low<- Q[1]-2.0*iqr # Lo
eliminated<- subset(all_data_join, all_data_join$R4 > (Q[1] - 2.0*iqr) & all_data_join$R4 < (Q[2]+2.0*iqr))
ggbetweenstats(eliminated, cond, R4, outlier.tagging = TRUE) 

eliminated %>% 
  group_by(cond) %>%
  summarise(mean(R4), sd(R4))

model.null <- lmer(R4 ~ (1 | subj) + (1 | item), eliminated) 
model <- lmer(R4 ~ cond + (1 | subj) + (1 | item), eliminated) 
anova(model, model.null)
summary(model)

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
model.null <- lmer(R5 ~ (1 + cond | subj) + (1 | item), all_data_join) 
model <- lmer(R5 ~ cond + (1 + cond | subj) + (1 | item), all_data_join) 
summary(model)

check_model(model)

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
model_alldata_simpler_ranef <- lmer(R4 ~ cond + SRS2_total_score_t + EQ + WRMT_total_reading_score + WRMT_WI_raw +
                                      (1 + cond | subj) +  (1 + cond | item) , data = all_data_join, REML = TRUE)

# Try again with a simpler model
model_alldata_simpler_ranef <- lmer(R4 ~ cond + SRS2_total_score_t + EQ + WRMT_total_reading_score + WRMT_WI_raw +
                                      (1 | subj) +  (1 | item) , data = all_data_join, REML = TRUE)

check_model(model_alldata_simpler_ranef)
summary(model_alldata_simpler_ranef)

#Singular fit
model_alldata_simpler_null <- lmer(R4 ~ SRS2_total_score_t + EQ + WRMT_total_reading_score + WRMT_WI_raw +
                                     (1 + cond | subj) +  (1 + cond | item) , data = all_data_join, REML = TRUE)

# Try again with a simpler model
model_alldata_simpler_null <- lmer(R4 ~ SRS2_total_score_t + EQ + WRMT_total_reading_score + WRMT_WI_raw +
                                     (1 | subj) +  (1 | item) , data = all_data_join, REML = TRUE)

anova(model_alldata_simpler_null, model_alldata_simpler_ranef)


