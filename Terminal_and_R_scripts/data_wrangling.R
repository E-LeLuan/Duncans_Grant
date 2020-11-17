library(tidyverse)

EDB1 <- read_csv('FP_ED_batch1_corr')

write_csv(EDB1, path = 'FP_ED_batch1_corr.csv')

EDB1 <- read_csv('FP_ED_batch1_corr.csv')
FAB1 <- read_csv('FP_FA_batch1_corr.csv')

EDP1 <- EDB1 %>% filter(subj == 1)
FAP1 <- FAB1 %>% filter(subj == 1)

plot1 <- EDP1 %>% 
  ggplot(aes(x = subj,
                    y = R1)) +
  geom_jitter(width = 1) +
  ylim(0,3500)

plot2 <- FAP1 %>% 
  ggplot(aes(x = subj,
                    y = R1)) +
  geom_jitter(width = 1) +
  ylim(0,3500)
 

plot1 + plot2

