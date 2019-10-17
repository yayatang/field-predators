# Importing and comparing litterbag masses

library(tidyverse)
library(ggpubr)
library(here)
source(here::here('src/yaya_fxns.r'))

cage_treatments <-  read_csv(here::here('results/g1_updated_cages_wk9.csv')) %>% 
    select(-predatorID, -block, -position)

bag_prep0 <- read_csv(here::here('data/litterbag_DATA_2019.07.11.csv'))
colnames(bag_prep0) <- c('cage', 'dir', 'start_lit', 'start_lit_bag')
bag_prep0 <- bag_prep0 %>% 
    filter(cage!=3 & cage!=26) %>% 
    filter(!(cage==29 & dir=="E"))

bag_samp1 <- read_csv(here::here('data/litterbag_sampling_DATA.csv'))
colnames(bag_samp1) <- c('cage', 'dir', 'samp1_wet', 'samp1_dry')

bag_all <- left_join(bag_prep0, bag_samp1, by=c('cage', 'dir')) %>% 
    left_join(cage_treatments, by= 'cage') %>% 
    mutate(samp1_wet_diff = samp1_wet - start_lit_bag) %>% 
    select(cage, replicate, treatment, dir, everything())


#== check the data
sample_n(bag_all, 10)

ggboxplot(bag_all, x = 'treatment', y = 'samp1_wet_diff',
          color = 'treatment', 
          order = c('control', 'ghop', 'mantid', 'spider'),
          ylab = 'litter bag mass difference from start', xlab = 'treatment')

ctrl_bags <- bag_all %>% 
    filter(treatment=='control')

# =====stats=====
samp1_summ <- group_by(bag_all, treatment)
summarise(samp1_summ, mean = mean(samp1_wet_diff), se = se(samp1_wet_diff))

# ANOVA
samp1_aov <- aov(samp1_wet_diff ~ treatment, data = bag_all)
summary(samp1_aov) # see whether treatments differ

# assumptions tests
plot(samp1_aov) #check plots for normally distributed values

# levene's test for homogeneity of variances
library(car)
samp1_levenes <- leveneTest(samp1_wet_diff ~ treatment, data = bag_all)
samp1_levenes
