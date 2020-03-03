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

# call the above function to get original moisture %
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

# assign values for first sampling
lit_moist[2,] <- data.frame(1,'north',mean(na.omit(bag_1$samp1_moist_percent)),
                            se(bag_1$samp1_moist_percent))

# calculate the average moisture and infer dry masses
bag_1 <- bag_1 %>% 
    mutate(samp1_dry_infer = samp1_wet_m - (lit_moist[2,]$moist_mean/100*samp1_wet_diff))


#== look at the data
sample_n(bag_1, 10)


#===SAMPLE 2
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
