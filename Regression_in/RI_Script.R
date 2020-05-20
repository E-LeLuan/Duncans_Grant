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

# Analysis of First Pass data

#import the data set batch 1
RI_ED_batch1_corr <- read_csv("Duncans-Grant-master/RI_ED/RI_ED_batch1_corr.csv")

#Rename the participant numbers in the batches back to their original participant numbers.
RI_ED_batch1_corr$subj[RI_ED_batch1_corr$subj == 20] <-"38"
RI_ED_batch1_corr$subj[RI_ED_batch1_corr$subj == 19] <-"36"
RI_ED_batch1_corr$subj[RI_ED_batch1_corr$subj == 18] <-"35"
RI_ED_batch1_corr$subj[RI_ED_batch1_corr$subj == 17] <-"34"
RI_ED_batch1_corr$subj[RI_ED_batch1_corr$subj == 16] <-"32"
RI_ED_batch1_corr$subj[RI_ED_batch1_corr$subj == 15] <-"30"
RI_ED_batch1_corr$subj[RI_ED_batch1_corr$subj == 14] <-"28"
RI_ED_batch1_corr$subj[RI_ED_batch1_corr$subj == 13] <-"26"
RI_ED_batch1_corr$subj[RI_ED_batch1_corr$subj == 12] <-"24"
RI_ED_batch1_corr$subj[RI_ED_batch1_corr$subj == 11] <-"22"
RI_ED_batch1_corr$subj[RI_ED_batch1_corr$subj == 10] <-"20"
RI_ED_batch1_corr$subj[RI_ED_batch1_corr$subj == 9] <-"18"
RI_ED_batch1_corr$subj[RI_ED_batch1_corr$subj == 8] <-"16"
RI_ED_batch1_corr$subj[RI_ED_batch1_corr$subj == 7] <-"14"
RI_ED_batch1_corr$subj[RI_ED_batch1_corr$subj == 6] <-"12"
RI_ED_batch1_corr$subj[RI_ED_batch1_corr$subj == 5] <-"10"
RI_ED_batch1_corr$subj[RI_ED_batch1_corr$subj == 4] <-"8"
RI_ED_batch1_corr$subj[RI_ED_batch1_corr$subj == 3] <-"6"
RI_ED_batch1_corr$subj[RI_ED_batch1_corr$subj == 2] <-"4"
RI_ED_batch1_corr$subj[RI_ED_batch1_corr$subj == 1] <-"2"

#import the data set batch 2B
RI_ED_batch2B_corr <- read_csv("Duncans-Grant-master/RI_ED/RI_ED_batch2B_corr.csv")

#Rename the participant numbers in the batches back to their original participant numbers.
RI_ED_batch2B_corr$subj[RI_ED_batch2B_corr$subj == 19] <-"71"
RI_ED_batch2B_corr$subj[RI_ED_batch2B_corr$subj == 18] <-"70"
RI_ED_batch2B_corr$subj[RI_ED_batch2B_corr$subj == 17] <-"69"
RI_ED_batch2B_corr$subj[RI_ED_batch2B_corr$subj == 16] <-"68"
RI_ED_batch2B_corr$subj[RI_ED_batch2B_corr$subj == 15] <-"66"
RI_ED_batch2B_corr$subj[RI_ED_batch2B_corr$subj == 14] <-"64"
RI_ED_batch2B_corr$subj[RI_ED_batch2B_corr$subj == 13] <-"62"
RI_ED_batch2B_corr$subj[RI_ED_batch2B_corr$subj == 12] <-"60"
RI_ED_batch2B_corr$subj[RI_ED_batch2B_corr$subj == 11] <-"58"
RI_ED_batch2B_corr$subj[RI_ED_batch2B_corr$subj == 10] <-"56"
RI_ED_batch2B_corr$subj[RI_ED_batch2B_corr$subj == 9] <-"54"
RI_ED_batch2B_corr$subj[RI_ED_batch2B_corr$subj == 8] <-"50"
RI_ED_batch2B_corr$subj[RI_ED_batch2B_corr$subj == 7] <-"48"
RI_ED_batch2B_corr$subj[RI_ED_batch2B_corr$subj == 6] <-"46"
RI_ED_batch2B_corr$subj[RI_ED_batch2B_corr$subj == 5] <-"44"
RI_ED_batch2B_corr$subj[RI_ED_batch2B_corr$subj == 4] <-"43"
RI_ED_batch2B_corr$subj[RI_ED_batch2B_corr$subj == 3] <-"42"
RI_ED_batch2B_corr$subj[RI_ED_batch2B_corr$subj == 2] <-"40"
RI_ED_batch2B_corr$subj[RI_ED_batch2B_corr$subj == 1] <-"39"

#import the data set batch 3
RI_ED_batch3_corr <- read_csv("Duncans-Grant-master/RI_ED/RI_ED_batch3_corr.csv")

#Rename the participant numbers in the batches back to their original participant numbers.
RI_ED_batch3_corr$subj[RI_ED_batch3_corr$subj == 21] <-"99"
RI_ED_batch3_corr$subj[RI_ED_batch3_corr$subj == 20] <-"95"
RI_ED_batch3_corr$subj[RI_ED_batch3_corr$subj == 19] <-"92"
RI_ED_batch3_corr$subj[RI_ED_batch3_corr$subj == 18] <-"91"
RI_ED_batch3_corr$subj[RI_ED_batch3_corr$subj == 17] <-"89"
RI_ED_batch3_corr$subj[RI_ED_batch3_corr$subj == 16] <-"87"
RI_ED_batch3_corr$subj[RI_ED_batch3_corr$subj == 15] <-"86"
RI_ED_batch3_corr$subj[RI_ED_batch3_corr$subj == 14] <-"85"
RI_ED_batch3_corr$subj[RI_ED_batch3_corr$subj == 13] <-"84"
RI_ED_batch3_corr$subj[RI_ED_batch3_corr$subj == 12] <-"83"
RI_ED_batch3_corr$subj[RI_ED_batch3_corr$subj == 11] <-"82"
RI_ED_batch3_corr$subj[RI_ED_batch3_corr$subj == 10] <-"81"
RI_ED_batch3_corr$subj[RI_ED_batch3_corr$subj == 9] <-"80"
RI_ED_batch3_corr$subj[RI_ED_batch3_corr$subj == 8] <-"79"
RI_ED_batch3_corr$subj[RI_ED_batch3_corr$subj == 7] <-"78"
RI_ED_batch3_corr$subj[RI_ED_batch3_corr$subj == 6] <-"77"
RI_ED_batch3_corr$subj[RI_ED_batch3_corr$subj == 5] <-"76"
RI_ED_batch3_corr$subj[RI_ED_batch3_corr$subj == 4] <-"75"
RI_ED_batch3_corr$subj[RI_ED_batch3_corr$subj == 3] <-"74"
RI_ED_batch3_corr$subj[RI_ED_batch3_corr$subj == 2] <-"73"
RI_ED_batch3_corr$subj[RI_ED_batch3_corr$subj == 1] <-"72"

#import the data set batch 4
RI_ED_batch4_error <- read_csv("Duncans-Grant-master/RI_ED/RI_ED_batch4_error.csv")

#Rename the participant numbers in the batches back to their original participant numbers.
RI_ED_batch4_error$subj[RI_ED_batch4_error$subj == 20] <-"41"
RI_ED_batch4_error$subj[RI_ED_batch4_error$subj == 19] <-"37"
RI_ED_batch4_error$subj[RI_ED_batch4_error$subj == 18] <-"35"
RI_ED_batch4_error$subj[RI_ED_batch4_error$subj == 17] <-"33"
RI_ED_batch4_error$subj[RI_ED_batch4_error$subj == 16] <-"31"
RI_ED_batch4_error$subj[RI_ED_batch4_error$subj == 15] <-"29"
RI_ED_batch4_error$subj[RI_ED_batch4_error$subj == 14] <-"27"
RI_ED_batch4_error$subj[RI_ED_batch4_error$subj == 13] <-"25"
RI_ED_batch4_error$subj[RI_ED_batch4_error$subj == 12] <-"23"
RI_ED_batch4_error$subj[RI_ED_batch4_error$subj == 11] <-"21"
RI_ED_batch4_error$subj[RI_ED_batch4_error$subj == 10] <-"19"
RI_ED_batch4_error$subj[RI_ED_batch4_error$subj == 9] <-"17"
RI_ED_batch4_error$subj[RI_ED_batch4_error$subj == 8] <-"15"
RI_ED_batch4_error$subj[RI_ED_batch4_error$subj == 7] <-"13"
RI_ED_batch4_error$subj[RI_ED_batch4_error$subj == 6] <-"11"
RI_ED_batch4_error$subj[RI_ED_batch4_error$subj == 5] <-"9"
RI_ED_batch4_error$subj[RI_ED_batch4_error$subj == 4] <-"7"
RI_ED_batch4_error$subj[RI_ED_batch4_error$subj == 3] <-"5"
RI_ED_batch4_error$subj[RI_ED_batch4_error$subj == 2] <-"3"
RI_ED_batch4_error$subj[RI_ED_batch4_error$subj == 1] <-"1"

#import the data set batch 5
RI_ED_batch5_error <- read_csv("Duncans-Grant-master/RI_ED/RI_ED_batch5_error.csv")

#Rename the participant numbers in the batches back to their original participant numbers.
RI_ED_batch5_error$subj[RI_ED_batch5_error$subj == 19] <-"98"
RI_ED_batch5_error$subj[RI_ED_batch5_error$subj == 18] <-"97"
RI_ED_batch5_error$subj[RI_ED_batch5_error$subj == 17] <-"96"
RI_ED_batch5_error$subj[RI_ED_batch5_error$subj == 16] <-"94"
RI_ED_batch5_error$subj[RI_ED_batch5_error$subj == 15] <-"93"
RI_ED_batch5_error$subj[RI_ED_batch5_error$subj == 14] <-"90"
RI_ED_batch5_error$subj[RI_ED_batch5_error$subj == 13] <-"88"
RI_ED_batch5_error$subj[RI_ED_batch5_error$subj == 12] <-"67"
RI_ED_batch5_error$subj[RI_ED_batch5_error$subj == 11] <-"65"
RI_ED_batch5_error$subj[RI_ED_batch5_error$subj == 10] <-"63"
RI_ED_batch5_error$subj[RI_ED_batch5_error$subj == 9] <-"61"
RI_ED_batch5_error$subj[RI_ED_batch5_error$subj == 8] <-"59"
RI_ED_batch5_error$subj[RI_ED_batch5_error$subj == 7] <-"57"
RI_ED_batch5_error$subj[RI_ED_batch5_error$subj == 6] <-"55"
RI_ED_batch5_error$subj[RI_ED_batch5_error$subj == 5] <-"53"
RI_ED_batch5_error$subj[RI_ED_batch5_error$subj == 4] <-"51"
RI_ED_batch5_error$subj[RI_ED_batch5_error$subj == 3] <-"49"
RI_ED_batch5_error$subj[RI_ED_batch5_error$subj == 2] <-"47"
RI_ED_batch5_error$subj[RI_ED_batch5_error$subj == 1] <-"45"

#Double check it all looks right now
#view(RI_ED_batch1_corr)
#view(RI_ED_batch2B_corr)
#view(RI_ED_batch3_corr)
#view(RI_ED_batch4_error)
#view(RI_ED_batch5_error)

#Let's combine the data
all_data <- rbind(RI_ED_batch1_corr, RI_ED_batch2B_corr, RI_ED_batch3_corr, RI_ED_batch4_error, 
                  RI_ED_batch5_error)

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
#REGION 4 model using glmer

all_data_join$R4[all_data_join$R4 == 1] <-"100"

all_data_join %>% 
  ggplot(aes(x = cond, y = R4, colour = cond)) + ggtitle("Regression In for Critical Region: Question") +
  labs(y = "Regression", x = "Prediction") +
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(colour = FALSE)
  
  #labs(title= "Petal and Sepal Length \nof Iris",
                          #     y="Petal Length (cm)", x = "Sepal Length (cm)"))

all_data_join %>% 
  group_by(cond) %>%
  summarise(mean(R4), sd(R4))

library(psych)
describeBy(all_data_join$R4, group=all_data_join$cond)

# Just eye tracking
modelR4 <- glmer(R4 ~ cond + (1 + cond | subj) + (1 + cond | item), all_data_join, family = "binomial") 
summary(modelR4)

modelR4.null <- glmer(R4 ~ (1 + cond | subj) + (1 + cond | item), all_data_join, family = "binomial") 

anova(modelR4, modelR4.null)
check_model(modelR4)

#Let's include some covariates! Region 4

#Step 1: Scale the ID measures...
all_data_join$SRS2_total_score_t <- scale(all_data_join$SRS2_total_score_t)
all_data_join$EQ <- scale(all_data_join$EQ)
all_data_join$WRMT_total_reading_score <- scale(all_data_join$WRMT_total_reading_score)
all_data_join$WRMT_WI_raw <- scale(all_data_join$WRMT_WI_raw)


model_alldata_ranefR4 <- glmer(R4 ~ cond + SRS2_total_score_t + EQ + WRMT_total_reading_score + WRMT_WI_raw +
                                       (1 | subj) +  (1 | item) , data = all_data_join, family = "binomial")

model_alldataR4_null <- glmer(R4 ~ SRS2_total_score_t + EQ + WRMT_total_reading_score + WRMT_WI_raw +
                                      (1 | subj) +  (1 + cond | item) , data = all_data_join, family = "binomial")

summary(model_alldata_ranefR4)
anova(model_alldata_ranefR4, model_alldataR4_null)
check_model(model_alldata_ranefR4)
anova(modelR4, model_alldata_ranefR4)

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

#Most complex model with the ability to converge 
model_alldata_ranefR5 <- glmer(R5 ~ cond + SRS2_total_score_t + EQ + WRMT_total_reading_score + WRMT_WI_raw +
                                       (1 | subj) +  (1 | item) , data = all_data_join, family = "binomial")

model_alldataR5_null <- glmer(R5 ~ SRS2_total_score_t + EQ + WRMT_total_reading_score + WRMT_WI_raw +
                                      (1 | subj) +  (1 + cond | item) , data = all_data_join, family = "binomial")

summary(model_alldata_ranefR5)
anova(model_alldata_ranefR5, model_alldataR5_null)
anova(modelR5, model_alldata_ranefR5)
check_model(model_alldata_ranefR5)

#Measuring Correlations
EQscore <- all_data_join %>% group_by(subj) %>% summarise(mean = mean(EQ)) %>% pull(mean)
SRS2 <- all_data_join %>% group_by(subj) %>% summarise(mean = mean(SRS2_total_score_t)) %>% pull(mean)
rcorr(EQscore, SRS2)


#rcorr(all_data_join$SRS2_total_score_t, all_data_join$EQ)

