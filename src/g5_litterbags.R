# This script is for importing and comparing litterbag masses

library(tidyverse)
library(ggpubr)
library(here)
source(here::here('src/yaya_fxns.r'))

# this fxn imports the dry litter data and generates the average moisture proportion
get_dry_lit <- function() {
    dry_lit <- read_csv(here::here('data/litter_dry.mass_data.csv')) %>% 
        mutate(mass_lit_wet = mass_tin_wet - mass_tin,
               mass_lit_dry = mass_tin_dry - mass_tin,
               prop_moist= (mass_lit_wet - mass_lit_dry) / mass_lit_wet)
    
    avg_moist <- mean(dry_lit$prop_moist)
}

#call sthe function to get original moisture %
litter_moist <- get_dry_lit()

# import cage properties and initial bag masses
cage_treatments <- read_csv(here::here('results/g1_updated_cages_wk9.csv')) %>% 
    select(-predatorID, -block, -position)

bag_prep0 <- read_csv(here::here('data/litterbag_DATA_2019.07.11.csv'))
colnames(bag_prep0) <- c('cage', 'dir', 'start_lit_wet', 'start_lit_bag')
bag_prep1 <- bag_prep0 %>% 
    filter(cage!=3 & cage!=26) %>% 
    filter(!(cage==29 & dir=="E")) %>% 
    mutate(start_lit_dry = start_lit_wet - (start_lit_wet*litter_moist),
           mass_bag = start_lit_bag - start_lit_wet)

bag_sampA <- read_csv(here::here('data/litterbag_sampling_DATA.csv'))
colnames(bag_sampA) <- c('cage', 'dir', 'samp1_wet', 'samp1_dry_1m', 'samp1_dry_4m')

bag_all <- left_join(bag_prep1, bag_sampA, by=c('cage', 'dir')) %>% 
    left_join(cage_treatments, by= 'cage') %>% 
    mutate(samp1_wet_diff = samp1_wet - start_lit_bag,
           samp1_dry_diff = samp1_dry_1m - start_lit_dry - mass_bag) %>% 
    select(cage, replicate, treatment, dir, everything())


#== look at the data
sample_n(bag_all, 10)

# boxplots for wet masses of all treatments and directions
ggboxplot(bag_all, x = 'treatment', y = 'samp1_wet_diff',
          color = 'treatment', 
          order = c('control', 'ghop', 'mantid', 'spider'),
          title = 'treatments litter bag WET mass difference from start', 
          xlab = 'treatment',
          ylab = 'wet mass diff') +
    theme(legend.position='none')

ggboxplot(bag_all, x = 'dir', y = 'samp1_wet_diff',
          color = 'dir', 
          order = c('N', 'E', 'S', 'W'),
          title = 'direction litter bag WET mass difference from start', 
          xlab = 'direction',
          ylab = 'wet mass diff')+
    theme(legend.position='none')

# boxplots for DRY masses of all treatments in the NORTH direction
bag_north <- filter(bag_all, dir == 'N')
ggboxplot(bag_north, x = 'treatment', y = 'samp1_dry_diff',
          color = 'treatment', 
          order = c('control', 'ghop', 'mantid', 'spider'),
          title = 'treatments litter bag DRY mass difference from start', 
          xlab = 'treatment',
          ylab = 'dry mass diff')+
    theme(legend.position='none')

# =====summary stats=====
# wet litterbag means by treatment
samp1_summ <- group_by(bag_all, treatment)
summarise(samp1_summ, mean = mean(samp1_wet_diff), se = se(samp1_wet_diff))

# means by direction 
samp1_dir <- group_by(bag_all, dir)
summarise(samp1_dir, mean = mean(samp1_wet_diff), se = se(samp1_wet_diff))

samp1_dry <- group_by(bag_north, treatment)
summarise(samp1_dry, mean = mean(samp1_dry_diff), se = se(samp1_dry_diff))

# =====ANOVAs=====
# ANOVA between wet bags
samp1_aov <- aov(samp1_wet_diff ~ treatment, data = bag_all)
summary(samp1_aov)

# ANOVA between bag direction
samp1_aov_dir <- aov(samp1_wet_diff ~ dir, data = bag_all)
summary(samp1_aov_dir)

# ANOVA between dry bag treatments
samp1_aov_dry <- aov(samp1_dry_diff ~ treatment, data = bag_north)
summary(samp1_aov_dry)


#===assumptions tests===
# plot(samp1_aov) #check plots for normally distributed values
# plot(samp1_aov_dir) #check plots for normally distributed values
plot(samp1_aov_dry)

# levene's test for homogeneity of variances
library(car)
samp1_levenes <- leveneTest(samp1_dry_diff ~ treatment, data = bag_north)
samp1_levenes
