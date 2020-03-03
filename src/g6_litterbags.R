source(here::here('src/g6.1_litterbag_import.R'))
source(here::here('src/g6.2_litterbag_viz.R'))
source(here::here('src/g6.3_litterbag_stats.R'))



# This script is for importing and comparing litterbag masses

library(tidyverse)
library(ggpubr)
library(here)
source(here::here('src/yaya_fxns.r'))

#===== set up reference data from experimental setup
# this fxn imports the dry litter data and generates the average moisture proportion
get_dry_lit <- function() {
    # this is the dry mass data from 
    dry_lit <- read_csv(here::here('data/litter_dry.mass_data.csv')) %>% 
        mutate(mass_lit_wet = mass_tin_wet - mass_tin,
               mass_lit_dry = mass_tin_dry - mass_tin,
               prop_moist= (mass_lit_wet - mass_lit_dry) / mass_lit_wet)
    
    avg_moist <- mean(dry_lit$prop_moist)
    se_moist <- se(dry_lit$prop_moist)
    return(list(avg_moist, se_moist))
}

#call function to get original moisture %
lit0_moist <- get_dry_lit() #[[1]] is the mean, [[2]] is the se

#==== create tibble for all litterbag sampling data
lit_moist <- tibble(sampling = integer(),
                    samp_desc = character(),
                    moist_mean = double(),
                    moist_se = double())
# assign values for pre-launch
lit_moist[1, 1] <- 0
lit_moist[1, 2] <- "pre-launch"
lit_moist[1, 3] <- lit0_moist[[1]]*100
lit_moist[1, 4] <- lit0_moist[[2]]*100


# import cage properties and initial bag masses from July 2019
cage_treatments <- read_csv(here::here('results/g1_updated_cages_wk9.csv')) %>% 
    select(-predatorID, -block, -position)

bag_prep0 <- read_csv(here::here('data/litterbag_DATA_2019.07.11.csv'))
colnames(bag_prep0) <- c('cage', 'dir', 'start_lit_wet', 'start_lit_bag')
bag_prep1 <- bag_prep0 %>% 
    filter(cage!=3 & cage!=26) %>% 
    filter(!(cage==29 & dir=="E")) %>%
    mutate(start_lit_dry = start_lit_wet - (start_lit_wet*lit0_moist[[1]]),
           m_bag = start_lit_bag - start_lit_wet)

#==== import sampling 1 data
bag_samp1 <- read_csv(here::here('data/litterbag_sampling1_N_DATA.csv'))
colnames(bag_samp1) <- c('cage', 'dir', 'samp1_wet', 'samp1_dry_1m', 'samp1_dry_4m')

# merge sampling1 data with pre-launch data
bag_1 <- left_join(bag_prep1, bag_samp1, by=c('cage', 'dir')) %>% 
    left_join(cage_treatments, by= 'cage') %>% 
    mutate(samp1_wet_m = samp1_wet - m_bag, 
           samp1_wet_diff = samp1_wet - start_lit_bag,
           samp1_dry_lit = samp1_dry_1m - m_bag,
           samp1_dry_diff = samp1_dry_1m - start_lit_dry - m_bag,
           samp1_moist = samp1_wet - samp1_dry_1m,
           samp1_moist_percent = samp1_moist/samp1_wet*100) %>% 
    select(cage, replicate, treatment, dir, everything())

# calculate the average moisture and infer dry masses
lit_moist[2,] <- data.frame(1,'north',mean(na.omit(bag_1$samp1_moist_percent)),
                   se(bag_1$samp1_moist_percent))
bag_1 <- bag_1 %>% 
    mutate(samp1_dry_infer = samp1_wet_m - (lit_moist[2,]$moist_mean/100*samp1_wet_diff))


#== look at the data
sample_n(bag_1, 10)

# boxplots for wet masses of all treatments and directions
ggboxplot(bag_1, x = 'treatment', y = 'samp1_wet_diff',
          color = 'treatment', 
          order = c('control', 'ghop', 'mantid', 'spider'),
          title = 'treatments litter bag WET mass difference from start', 
          xlab = 'treatment',
          ylab = 'wet mass diff') +
    theme(legend.position='none')

ggboxplot(bag_1, x = 'dir', y = 'samp1_wet_diff',
          color = 'dir', 
          order = c('N', 'E', 'S', 'W'),
          title = 'direction litter bag WET mass difference from start', 
          xlab = 'direction',
          ylab = 'wet mass diff')+
    theme(legend.position='none')

# by inferred dry mass, by treatment
ggboxplot(bag_1, x = 'treatment', y = 'samp1_dry_infer',
          color = 'treatment',
          order = c('control', 'ghop', 'mantid', 'spider'),
          title = '1: treatments litter bag DRY mass',
          xlab = 'treatment',
          ylab = 'dry mass') +
    theme(legend.position='none')

# by inferred dry mass, by direction
ggboxplot(bag_1, x = 'dir', y = 'samp1_dry_infer',
          color = 'dir', 
          order = c('N', 'E', 'S', 'W'),
          title = '1: direction litter bag DRY mass', 
          xlab = 'direction',
          ylab = 'dry mass')+
    theme(legend.position='none')

ggbarplot(lit_moist, x = 'sampling', y = 'moist_mean',
          color = 'sampling', 
          title = 'average %moisture of litterbags',
          xlab = 'sampling',
          ylab = '%moisture',
)

# boxplots for DRY masses of all treatments in the NORTH direction
bag_north <- filter(bag_1, dir == 'N')
ggboxplot(bag_north, x = 'treatment', y = 'samp1_dry_diff',
          color = 'treatment', 
          order = c('control', 'ghop', 'mantid', 'spider'),
          title = 'Dry litter bags by treatments, difference from start', 
          xlab = 'Treatment',
          ylab = 'Dry mass difference')+
    theme(legend.position='none')

# =====summary stats=====
# wet litterbag means by treatment
samp1_summ <- group_by(bag_1, treatment)
summarise(samp1_summ, mean = mean(samp1_wet_diff), se = se(samp1_wet_diff))

# means by direction 
samp1_dir <- group_by(bag_1, dir)
summarise(samp1_dir, mean = mean(samp1_wet_diff), se = se(samp1_wet_diff))

samp1_dry <- group_by(bag_north, treatment)
summarise(samp1_dry, mean = mean(samp1_dry_diff), se = se(samp1_dry_diff))

# =====ANOVAs=====
# ANOVA between wet bags
samp1_aov <- aov(samp1_wet_diff ~ treatment, data = bag_1)
summary(samp1_aov)

# ANOVA between bag direction
samp1_aov_dir <- aov(samp1_wet_diff ~ dir, data = bag_1)
summary(samp1_aov_dir)

# ANOVA between dry bag treatments
samp1_aov_dry <- aov(samp1_dry_diff ~ treatment, data = bag_north)
summary(samp1_aov_dry)

samp1_ttest_start <- t.test(bag_north$samp1_dry_lit, bag_north$start_lit_dry, paired=TRUE)
samp1_ttest_start

# #===assumptions tests===
# # plot(samp1_aov) #check plots for normally distributed values
# # plot(samp1_aov_dir) #check plots for normally distributed values
# plot(samp1_aov_dry)
# 
# # levene's test for homogeneity of variances
# library(car)
# samp1_levenes <- leveneTest(samp1_dry_diff ~ treatment, data = bag_north)
# samp1_levenes

#==============
####
# copied code for sampling 2

#==== import sampling 2 data
bag_samp2 <- read_csv(here::here('data/litterbag_sampling2_E_DATA.csv'))
colnames(bag_samp2) <- c('cage', 'dir', 'samp2_wet', 'samp2_dry')

# merge sampling1 data with pre-launch data
bag_2 <- left_join(bag_1, bag_samp2) %>% 
    filter(dir != 'N') %>% 
    mutate(samp2_wet_m = samp2_wet - m_bag, 
           samp2_wet_diff = samp2_wet - start_lit_bag,
           samp2_dry_lit = samp2_dry - m_bag,
           samp2_dry_diff = samp2_dry - start_lit_dry - m_bag,
           samp2_moist = samp2_wet - samp2_dry,
           samp2_moist_percent = samp2_moist/samp2_wet*100) %>% 
    select(cage, replicate, treatment, dir, everything())

# calculate the average moisture and infer dry masses
lit_moist[3,] <- data.frame(2,'east',mean(na.omit(bag_2$samp2_moist_percent)),
                            se(bag_2$samp2_moist_percent))
bag_2 <- bag_2 %>% 
    mutate(samp2_dry_infer = samp2_wet_m - (lit_moist[3,]$moist_mean/100*samp2_wet_diff))

#== look at the data
sample_n(bag_2, 10)

# boxplots for wet masses of all treatments and directions
# ggboxplot(bag_2, x = 'treatment', y = 'samp2_wet_diff',
#           color = 'treatment', 
#           order = c('control', 'ghop', 'mantid', 'spider'),
#           title = '2: treatments litter bag WET mass difference from start', 
#           xlab = 'treatment',
#           ylab = 'wet mass diff') +
#     theme(legend.position='none')
# 
# ggboxplot(bag_2, x = 'dir', y = 'samp2_wet_diff',
#           color = 'dir', 
#           order = c('N', 'E', 'S', 'W'),
#           title = '2: direction litter bag WET mass difference from start', 
#           xlab = 'direction',
#           ylab = 'wet mass diff')+
#     theme(legend.position='none')

ggboxplot(bag_2, x = 'treatment', y = 'samp2_dry_infer',
          color = 'treatment',
          order = c('control', 'ghop', 'mantid', 'spider'),
          title = '2: treatments litter bag DRY mass',
          xlab = 'treatment',
          ylab = 'dry mass') +
    theme(legend.position='none')

ggboxplot(bag_2, x = 'dir', y = 'samp2_dry_infer',
          color = 'dir', 
          order = c('N', 'E', 'S', 'W'),
          title = '2: direction litter bag DRY mass', 
          xlab = 'direction',
          ylab = 'dry mass')+
    theme(legend.position='none')

ggbarplot(lit_moist, x = 'sampling', y = 'moist_mean',
          color = 'sampling', 
          title = '2: average %moisture of litterbags',
          xlab = 'sampling',
          ylab = '%moisture',
)

# boxplots for DRY masses of all treatments in the EAST direction
bag_east <- filter(bag_2, dir == 'E')
ggboxplot(bag_east, x = 'treatment', y = 'samp2_dry_diff',
          color = 'treatment', 
          order = c('control', 'ghop', 'mantid', 'spider'),
          title = '2: Dry litter bags by treatments, difference from start', 
          xlab = 'Treatment',
          ylab = 'Dry mass difference')+
    theme(legend.position='none')

# =====summary stats=====
# wet litterbag means by treatment
samp2_summ <- group_by(bag_2, treatment)
summarise(samp2_summ, mean = mean(samp2_wet_diff), se = se(samp2_wet_diff))

# means by direction 
samp2_dir <- group_by(bag_2, dir)
summarise(samp2_dir, mean = mean(samp2_wet_diff), se = se(samp2_wet_diff))

samp2_dry <- group_by(bag_east, treatment)
summarise(samp2_dry, mean = mean(samp2_dry_diff), se = se(samp2_dry_diff))

# =====ANOVAs=====
# ANOVA between wet bags
samp2_aov <- aov(samp2_wet_diff ~ treatment, data = bag_2)
summary(samp2_aov)

# ANOVA between bag direction
samp2_aov_dir <- aov(samp2_wet_diff ~ dir, data = bag_2)
summary(samp2_aov_dir)

# ANOVA between dry bag treatments
samp2_aov_dry <- aov(samp2_dry_diff ~ treatment, data = bag_east)
summary(samp2_aov_dry)

samp2_ttest_start <- t.test(bag_east$samp2_dry_lit, bag_east$start_lit_dry, paired=TRUE)
samp2_ttest_start

# #===assumptions tests===
# # plot(samp2_aov) #check plots for normally distributed values
# # plot(samp2_aov_dir) #check plots for normally distributed values
# plot(samp2_aov_dry)
# 
# # levene's test for homogeneity of variances
# library(car)
# samp2_levenes <- leveneTest(samp2_dry_diff ~ treatment, data = bag_east)
# samp2_levenes