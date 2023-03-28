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
library(sjPlot)

#Set seed for random number generation
set.seed(42)
knitr::opts_chunk$set(cache.extra = knitr::rand_seed)
library(readr)
RP_ED_batch_corr <- read_csv("Manual_tidy_analysis/Regression_Path/RP_ED/RP_ED_batch_corr.csv")
RP_ED_batch_error <- read_csv("Manual_tidy_analysis/Regression_Path/RP_ED/RP_ED_batch_error.csv")

#Rename the participant numbers in the batches back to their original participant numbers.
RP_ED_batch_corr$subj[RP_ED_batch_corr$subj == 54] <-"84"
RP_ED_batch_corr$subj[RP_ED_batch_corr$subj == 53] <-"83"
RP_ED_batch_corr$subj[RP_ED_batch_corr$subj == 52] <-"82"
RP_ED_batch_corr$subj[RP_ED_batch_corr$subj == 51] <-"81"
RP_ED_batch_corr$subj[RP_ED_batch_corr$subj == 50] <-"80"
RP_ED_batch_corr$subj[RP_ED_batch_corr$subj == 49] <-"79"
RP_ED_batch_corr$subj[RP_ED_batch_corr$subj == 48] <-"78"
RP_ED_batch_corr$subj[RP_ED_batch_corr$subj == 47] <-"77"
RP_ED_batch_corr$subj[RP_ED_batch_corr$subj == 46] <-"76"
RP_ED_batch_corr$subj[RP_ED_batch_corr$subj == 45] <-"75"
RP_ED_batch_corr$subj[RP_ED_batch_corr$subj == 44] <-"74"
RP_ED_batch_corr$subj[RP_ED_batch_corr$subj == 43] <-"73"
RP_ED_batch_corr$subj[RP_ED_batch_corr$subj == 42] <-"72"
RP_ED_batch_corr$subj[RP_ED_batch_corr$subj == 41] <-"71"
RP_ED_batch_corr$subj[RP_ED_batch_corr$subj == 40] <-"70"
RP_ED_batch_corr$subj[RP_ED_batch_corr$subj == 39] <-"69"
RP_ED_batch_corr$subj[RP_ED_batch_corr$subj == 38] <-"68"
RP_ED_batch_corr$subj[RP_ED_batch_corr$subj == 37] <-"67"
RP_ED_batch_corr$subj[RP_ED_batch_corr$subj == 36] <-"66"
RP_ED_batch_corr$subj[RP_ED_batch_corr$subj == 35] <-"64"
RP_ED_batch_corr$subj[RP_ED_batch_corr$subj == 34] <-"62"
RP_ED_batch_corr$subj[RP_ED_batch_corr$subj == 33] <-"60"
RP_ED_batch_corr$subj[RP_ED_batch_corr$subj == 32] <-"58"
RP_ED_batch_corr$subj[RP_ED_batch_corr$subj == 31] <-"56"
RP_ED_batch_corr$subj[RP_ED_batch_corr$subj == 30] <-"54"
RP_ED_batch_corr$subj[RP_ED_batch_corr$subj == 29] <-"52"
RP_ED_batch_corr$subj[RP_ED_batch_corr$subj == 28] <-"50"
RP_ED_batch_corr$subj[RP_ED_batch_corr$subj == 27] <-"48"
RP_ED_batch_corr$subj[RP_ED_batch_corr$subj == 26] <-"46"
RP_ED_batch_corr$subj[RP_ED_batch_corr$subj == 25] <-"44"
RP_ED_batch_corr$subj[RP_ED_batch_corr$subj == 24] <-"43"
RP_ED_batch_corr$subj[RP_ED_batch_corr$subj == 23] <-"42"
RP_ED_batch_corr$subj[RP_ED_batch_corr$subj == 22] <-"40"
RP_ED_batch_corr$subj[RP_ED_batch_corr$subj == 21] <-"39"
RP_ED_batch_corr$subj[RP_ED_batch_corr$subj == 20] <-"38"
RP_ED_batch_corr$subj[RP_ED_batch_corr$subj == 19] <-"36"
RP_ED_batch_corr$subj[RP_ED_batch_corr$subj == 18] <-"35"
RP_ED_batch_corr$subj[RP_ED_batch_corr$subj == 17] <-"34"
RP_ED_batch_corr$subj[RP_ED_batch_corr$subj == 16] <-"32"
RP_ED_batch_corr$subj[RP_ED_batch_corr$subj == 15] <-"30"
RP_ED_batch_corr$subj[RP_ED_batch_corr$subj == 14] <-"28"
RP_ED_batch_corr$subj[RP_ED_batch_corr$subj == 13] <-"26"
RP_ED_batch_corr$subj[RP_ED_batch_corr$subj == 12] <-"24"
RP_ED_batch_corr$subj[RP_ED_batch_corr$subj == 11] <-"22"
RP_ED_batch_corr$subj[RP_ED_batch_corr$subj == 10] <-"20"
RP_ED_batch_corr$subj[RP_ED_batch_corr$subj == 9] <-"18"
RP_ED_batch_corr$subj[RP_ED_batch_corr$subj == 8] <-"16"
RP_ED_batch_corr$subj[RP_ED_batch_corr$subj == 7] <-"14"
RP_ED_batch_corr$subj[RP_ED_batch_corr$subj == 6] <-"12"
RP_ED_batch_corr$subj[RP_ED_batch_corr$subj == 5] <-"10"
RP_ED_batch_corr$subj[RP_ED_batch_corr$subj == 4] <-"8"
RP_ED_batch_corr$subj[RP_ED_batch_corr$subj == 3] <-"6"
RP_ED_batch_corr$subj[RP_ED_batch_corr$subj == 2] <-"4"
RP_ED_batch_corr$subj[RP_ED_batch_corr$subj == 1] <-"2"
RP_ED_batch_error$subj[RP_ED_batch_error$subj == 31] <-"65"
RP_ED_batch_error$subj[RP_ED_batch_error$subj == 30] <-"63"
RP_ED_batch_error$subj[RP_ED_batch_error$subj == 29] <-"61"
RP_ED_batch_error$subj[RP_ED_batch_error$subj == 28] <-"59"
RP_ED_batch_error$subj[RP_ED_batch_error$subj == 27] <-"57"
RP_ED_batch_error$subj[RP_ED_batch_error$subj == 26] <-"55"
RP_ED_batch_error$subj[RP_ED_batch_error$subj == 25] <-"53"
RP_ED_batch_error$subj[RP_ED_batch_error$subj == 24] <-"51"
RP_ED_batch_error$subj[RP_ED_batch_error$subj == 23] <-"49"
RP_ED_batch_error$subj[RP_ED_batch_error$subj == 22] <-"47"
RP_ED_batch_error$subj[RP_ED_batch_error$subj == 21] <-"45"
RP_ED_batch_error$subj[RP_ED_batch_error$subj == 20] <-"41"
RP_ED_batch_error$subj[RP_ED_batch_error$subj == 19] <-"37"
RP_ED_batch_error$subj[RP_ED_batch_error$subj == 18] <-"35"
RP_ED_batch_error$subj[RP_ED_batch_error$subj == 17] <-"33"
RP_ED_batch_error$subj[RP_ED_batch_error$subj == 16] <-"31"
RP_ED_batch_error$subj[RP_ED_batch_error$subj == 15] <-"29"
RP_ED_batch_error$subj[RP_ED_batch_error$subj == 14] <-"27"
RP_ED_batch_error$subj[RP_ED_batch_error$subj == 13] <-"25"
RP_ED_batch_error$subj[RP_ED_batch_error$subj == 12] <-"23"
RP_ED_batch_error$subj[RP_ED_batch_error$subj == 11] <-"21"
RP_ED_batch_error$subj[RP_ED_batch_error$subj == 10] <-"19"
RP_ED_batch_error$subj[RP_ED_batch_error$subj == 9] <-"17"
RP_ED_batch_error$subj[RP_ED_batch_error$subj == 8] <-"15"
RP_ED_batch_error$subj[RP_ED_batch_error$subj == 7] <-"13"
RP_ED_batch_error$subj[RP_ED_batch_error$subj == 6] <-"11"
RP_ED_batch_error$subj[RP_ED_batch_error$subj == 5] <-"9"
RP_ED_batch_error$subj[RP_ED_batch_error$subj == 4] <-"7"
RP_ED_batch_error$subj[RP_ED_batch_error$subj == 3] <-"5"
RP_ED_batch_error$subj[RP_ED_batch_error$subj == 2] <-"3"
# Analysis of Regression Path data

#import the data set batch 1
#Check the subject numbers have been redefined correctly
#View(RP_ED_batch_corr)
#View(RP_ED_batch_error)

#Let's combine the data
all_data <- rbind(RP_ED_batch_corr, RP_ED_batch_error)

view(all_data)

#make subj a factor
all_data$subj <- as.factor(all_data$subj)

#Import Individual difference measures
All_IDs <- read_csv("Other_data_and_information/All_IDs.csv")
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

#Visualisation
all_data_join %>% 
  ggplot(aes(x = cond, y = R4, colour = cond)) + ggtitle("Regression Path for Critical Region: Question") +
  labs(y = "Reading time in ms.", x = "Prediction") +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(colour = FALSE)

#boxplot
all_data_join %>% 
  ggplot(aes(x = cond, y = R4, colour = cond)) + ggtitle("Regression Path for Critical Region: Question") +
  labs(y = "Reading time in ms.", x = "Prediction") +
  geom_boxplot()+  
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(colour = FALSE)


#Descriptives
all_data_join %>% 
  group_by(cond) %>%
  summarise(mean(R4), sd(R4))

# Model assuming normality of residuals maximal structure
#model.nullR4 <- lmer(R4 ~ (1 + cond | subj) + (1 + cond | item), all_data_join) 
modelR4 <- lmer(R4 ~ cond + (1 | subj) + (1 + cond | item), all_data_join) 
summary(modelR4)
model.nullR4 <- lmer(R4 ~ (1 | subj) + (1 + cond | item), all_data_join) 
anova(modelR4, model.nullR4)

#All the data for this model looks pretty normal.
check_model(modelR4)
#qqnorm(residuals(modelR4))
#qqline(residuals(modelR4))
descdist(all_data_join$R4)
#Let's include some co-variates! Region 4

#Step 1: Scale the ID measures...
all_data_join$SRS_total_score_t <- scale(all_data_join$SRS_total_score_t)
all_data_join$EQ <- scale(all_data_join$EQ)
all_data_join$Total_reading_cluster <- scale(all_data_join$Total_reading_cluster)
all_data_join$Total_RAN <- scale(all_data_join$Total_RAN)
all_data_join$"WI _RPI" <- scale(all_data_join$"WI _RPI")

# Model including covariates
model_alldatacov_R4 <- lmer(R4 ~ SRS_total_score_t + EQ + Total_reading_cluster + Total_RAN + cond + (1 | subj) +  (1 + cond | item) , data = all_data_join, REML = TRUE)
summary(model_alldatacov_R4)

SE1 = emmeans(model_alldatacov_R4, specs = 'cond')
summary(SE1)

# Getting our Summary of Mixed Models as a table using the sjPlot package.
#Nakagawa S, Johnson P, Schielzeth H (2017) 
#The coefficient of determination R2 and intra-class correlation coefficient from generalized linear mixed-effects models revisted and expanded. 
#J. R. Soc. Interface 14. doi: 10.1098/rsif.2017.0213
#tab_model(model_alldatacov_R4, p.val = "kr", show.df = TRUE)


#model_alldatacov_R4_null <- lmer(R4 ~ SRS_total_score_t + EQ + Total_reading_cluster + #Total_RAN +
#                               (1 + cond | subj) +  (1 + cond | item) , data = #all_data_join, REML = TRUE)


#anova(model_alldatacov_R4_null, model_alldatacov_R4)
check_model(model_alldatacov_R4)


#idividual intercepts and slopes 
ranef(model_alldatacov_R4)

# summary of Results for region 4, the question.
# No sig. diff. b/w conditions on the measure regression path. RAN "approaching" sig.

library(Hmisc)
#Measuring Correlations
#EQscore <- all_data_join %>% group_by(subj) %>% summarise(mean = mean(EQ)) %>% pull(mean)
#SRS2 <- all_data_join %>% group_by(subj) %>% summarise(mean = mean(SRS_total_score_t)) %>% pull(mean)
#WRMT <- all_data_join %>% group_by(subj) %>% summarise(mean = mean(Total_reading_cluster)) %>% pull(mean)
#RAN <- all_data_join %>% group_by(subj) %>% summarise(mean = mean(Total_RAN)) %>% pull(mean)
#rcorr(EQscore, SRS2)
#rcorr(EQscore, WRMT)
#rcorr(EQscore, RAN)
#rcorr(SRS2, WRMT)
#rcorr(SRS2, RAN)
#rcorr(WRMT, RAN)


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
modelR5 <- lmer(R5 ~ cond + (1 + cond | subj) + (1 + cond | item), all_data_join) 
summary(modelR5)
model.nullR5 <- lmer(R5 ~ (1 + cond | subj) + (1 + cond | item), all_data_join) 
anova(modelR5, model.nullR5)

#All the data for this model looks pretty normal.
check_model(modelR5)
#qqnorm(residuals(modelR5))
#qqline(residuals(modelR5))
descdist(all_data_join$R5)
#Let's include some co-variates! region 5

#Step 1: Scale the ID measures...
#all_data_join$SRS_total_score_t <- scale(all_data_join$SRS_total_score_t)
#all_data_join$EQ <- scale(all_data_join$EQ)
#all_data_join$Total_reading_cluster <- scale(all_data_join$Total_reading_cluster)
#all_data_join$Total_RAN <- scale(all_data_join$Total_RAN)
#all_data_join$"WI _RPI" <- scale(all_data_join$"WI _RPI")
# Model including covariates
model_alldatacov_R5 <- lmer(R5 ~ SRS_total_score_t + EQ + Total_reading_cluster + Total_RAN + cond + (1 | subj) +  (1 + cond | item) , data = all_data_join, REML = TRUE)
summary(model_alldatacov_R5)

SE1 = emmeans(model_alldatacov_R5, specs = 'cond')
summary(SE1)

#anova(model_alldatacov_R5_null, model_alldatacov_R5)
check_model(model_alldatacov_R5)

ranef(model_alldatacov_R5)

myplot3 <- ggboxplot(
  all_data_join, x = "cond", y = "R5",
  fill = "cond", palette = "jco", legend = "none",
  ggtheme = theme_pubr(border = TRUE)) + 
  labs(title = "Post-Critical Reply Region: Regression Path", y = "Reading time in Milliseconds", x = "Prediction")
myplot3
#Raincloud plot
all_data_join %>% 
  ggplot(aes(x = cond, y = R5, colour = cond)) + ggtitle("Post-Critical Reply Region: Regression Path") +
  #add violins from ggdist package
  stat_halfeye(adjust = .5, width = .5, .width = 0, justification = -.3, point_colour = NA) + 
  geom_boxplot(width = 0.35, outlier.color = "NA", justification = -0.35) +
  scale_fill_fivethirtyeight() + 
  labs(y = "Reading time in Milliseconds", x = "Prediction") + 
  coord_flip()


#Let's have a look at region 3

#set condition as a factor
all_data_join$cond <- as.factor(all_data_join$cond)
# Throw away zeroes
all_data_join <- all_data_join %>% filter(R3 != 0)

#Visualisation
all_data_join %>% 
  ggplot(aes(x = cond, y = R3, colour = cond)) + ggtitle("Regression Path for Prediction") +
  labs(y = "Reading time in ms.", x = "Prediction") +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(colour = FALSE)

#Descriptives
all_data_join %>% 
  group_by(cond) %>%
  summarise(mean(R3), sd(R3))

# Model assuming normality of residuals maximal structure
modelR3 <- lmer(R3 ~ cond + (1 + cond | subj) + (1 + cond | item), all_data_join) 
summary(modelR3)
#Null model fails to converge
#model.nullR3 <- lmer(R3 ~ (1 + cond | subj) + (1 + cond | item), all_data_join) 
#anova(modelR3, model.nullR3)

#All the data for this model looks pretty normal.
check_model(modelR3)
#qqnorm(residuals(modelR3))
#qqline(residuals(modelR3))
descdist(all_data_join$R3)
#Let's include some co-variates! region 3

#Step 1: Scale the ID measures...
#all_data_join$SRS_total_score_t <- scale(all_data_join$SRS_total_score_t)
#all_data_join$EQ <- scale(all_data_join$EQ)
#all_data_join$Total_reading_cluster <- scale(all_data_join$Total_reading_cluster)
#all_data_join$Total_RAN <- scale(all_data_join$Total_RAN)
#all_data_join$"WI _RPI" <- scale(all_data_join$"WI _RPI")
# Model including covariates
model_alldatacov_R3 <- lmer(R3 ~ SRS_total_score_t + EQ + Total_reading_cluster + Total_RAN + cond + (1 | subj) +  (1 + cond | item) , data = all_data_join, REML = TRUE)
summary(model_alldatacov_R3)

SE1 = emmeans(model_alldatacov_R3, specs = 'cond')
summary(SE1)

# No sig. diff. b/w conditions on the measure regression path. RAN "approaching" sig.

#anova(model_alldatacov_R3_null, model_alldatacov_R3)
check_model(model_alldatacov_R3)

ranef(model_alldatacov_R3)
