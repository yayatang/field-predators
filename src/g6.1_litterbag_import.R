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

lit_samps <- read_csv(here::here('data/lit_samplings.csv')) %>% 
    rename(samp_num = sampling)


#==== create tibble for all litterbag sampling data
# lit_moist <- tibble(sampling = integer(),
#                     samp_desc = character())
# 
# # lit_moist <- tibble(moist_mean = double(),
# #                     moist_se = double())
# # assign values for pre-launch
# lit_moist[1, 1] <- 0
# lit_moist[1, 2] <- "pre-launch"



# import cage properties and initial bag masses from July 2019
cage_treatments <- read_csv(here::here('results/g1_updated_cages_wk9.csv')) %>% 
    select(-predatorID, -block, -position)

bag_prep_raw <- read_csv(here::here('data/litterbag_DATA_2019.07.11.csv'))
colnames(bag_prep_raw) <- c('cage', 'dir', 'start_lit_wet', 'start_lit_bag')
bag_prep <- bag_prep_raw %>% 
    filter(cage!=3 & cage!=26) %>% 
    filter(!(cage==29 & dir=="E")) %>%
    mutate(start_lit_dry = start_lit_wet - (start_lit_wet*lit0_moist[[1]]),
           m_bag = start_lit_bag - start_lit_wet) %>% 
    left_join(cage_treatments, by= 'cage') %>% 
    select(cage, dir, everything())

#==== create sampling 0 data
bag_samp0 <- bag_prep %>% 
    mutate(bag_wet = m_bag + start_lit_wet,
           bag_dry = m_bag + start_lit_dry,
           samp_num = 0) %>% 
    select(cage, dir, bag_wet, bag_dry, samp_num)

#==== import sampling 1 data
bag_samp1 <- read_csv(here::here('data/litterbag_sampling1_N_DATA.csv'))
colnames(bag_samp1) <- c('cage', 'dir', 'bag_wet', 'bag_dry', 'bag_dry_4m')
bag_samp1 <- select(bag_samp1, -bag_dry_4m) %>% 
    mutate(samp_num = 1)

#==== import sampling 2 data
bag_samp2 <- read_csv(here::here('data/litterbag_sampling2_E_DATA.csv'))
colnames(bag_samp2) <- c('cage', 'dir', 'bag_wet', 'bag_dry')
bag_samp2 <- mutate(bag_samp2, samp_num = 2)

# merge sampling1 data with pre-launch data
bag_data <- bind_rows(bag_samp0, bag_samp1, bag_samp2) %>% 
    left_join(bag_prep, by=c('cage', 'dir')) %>% 
    group_by(samp_num) %>% 
    mutate(lit_wet_m = bag_wet - m_bag, # for the wet mass of litter
           wet_diff = bag_wet - start_lit_bag, # for wet mass diff from the tstart
           dry_lit = bag_dry - m_bag, # dry mass of litter at this sampling
           dry_diff = start_lit_dry - bag_dry - m_bag, # difference in dry litter mass from start
           moist = bag_wet - bag_dry,
           moist_percent = moist/lit_wet_m*100, # what is this a percentage of?
           bag_sampID = paste0(cage, dir, samp_num)) %>% 
    select(bag_sampID, cage, replicate, treatment, dir, everything())

# calculate mean moisture percentage and add to each 

mean_moists <- bag_data %>% 
    group_by(samp_num) %>% 
    summarize(samp_moist.perc_mean = mean(moist_percent, na.rm=TRUE),
              samp_moist.perc_se = se(moist_percent))

lit_moist <- lit_samps %>% 
    left_join(mean_moists)
lit_moist[1,]$samp_moist.perc_mean <- lit0_moist[[1]] * 100
lit_moist[1,]$samp_moist.perc_se <- lit0_moist[[2]] * 100


# calculate the average moisture and infer dry masses
bag_data2 <- bag_data %>% 
    left_join(lit_moist, by="samp_num") %>% 
    rename(moist_mean.perc = samp_moist.perc_mean,
           moist_se.perc = samp_moist.perc_se)  %>% 
    mutate(
        dry_infer = lit_wet_m - (moist_mean.perc/100*lit_wet_m),
        bag_sampID = paste0(as.character(cage), dir, as.character(samp_num))) %>% 
    select(bag_sampID, everything())

bag_data3 <- bag_data2 %>% 
    group_by(treatment, samp_num) %>% 
    summarize(mean.infer_trt = mean(dry_infer),
              se.infer_trt = se(dry_infer)) %>% 
    na.omit()

bag_data_real <- bag_data2 %>% 
    na.omit() %>% 
    group_by(treatment, samp_num) %>% 
    summarize(mean.true_trt = mean(dry_lit),
              se.true_trt = se(dry_lit))
