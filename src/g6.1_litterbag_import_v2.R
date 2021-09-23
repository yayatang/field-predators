# This script is for importing and comparing litterbag masses

library(tidyverse)
library(ggpubr)
library(viridis)
library(here)
source(here::here('src/yaya_fxns.r'))
source('C:/Users/yaya/Dropbox/1 Ecologist/2 EXPERIMENTS/yaya_r_themes.R')

#===== set up reference data from experimental setup ======

# this fxn imports the dry litter data and generates the average moisture proportion
get_dry_lit <- function() {
    # this is the dry mass data from weighed tins during prep
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

# import cage properties and initial bag masses from July 2019
cage_treatments <- read_csv(here::here('results/g1_updated_cages_wk9.csv')) %>% 
    select(-predatorID, -position)


#====== start importing litterbag weights ========

bag_prep_raw <- read_csv(here::here('data/litterbag_DATA_2019.07.11.csv'))
colnames(bag_prep_raw) <- c('cage', 'dir', 'm_lit_wet', 'start_m_lit_bag')

# clean up imported initial litterbag data
# filter out irrelevant cages + outliers
bag_prep <- bag_prep_raw %>% 
    filter(cage!=3 & cage!=26) %>% 
    mutate(start_m_lit_dry = m_lit_wet - (m_lit_wet*lit0_moist[[1]]),
           m_bag = start_m_lit_bag - start_m_lit_dry) %>% 
    left_join(cage_treatments, by= 'cage') %>% 
    # filter(!(cage==29 & dir=="E")) %>% # strange mass-gaining outlier in samp 1
    # above line commented out since we're doing ash free analysis
    select(cage, dir, treatment, replicate, m_bag, start_m_lit_dry, everything())

# REMOVE???
# this is where the logic of making a "spread" table doesnt seem to make sense....

# #==== create sampling 0 data
# # m_lit_bag_wet/ bag_dry are variables for all samplings to use
# bag_samp0 <- bag_prep %>% 
#     mutate(m_lit_bag_wet = m_bag + start_lit_wet,
#            bag_dry = m_bag + start_lit_dry,
#            samp_num = 0) %>% 
#     select(cage, dir, samp_num, m_lit_bag_wet, bag_dry, )

#==== import sampling 1 data =====
samp1_raw <- read_csv(here::here('data/litterbag_sampling1_N_DATA.csv'))
colnames(samp1_raw) <- c('cage', 'dir', 'm_lit_bag_wet', 'm_lit_bag_dry', 'bag_dry_4m')
bag_samp1 <- samp1_raw %>% 
    mutate(samp_num = 1) %>% 
    select(-bag_dry_4m)

#==== import sampling 2 data
samp2_raw <- read_csv(here::here('data/litterbag_sampling2_E_DATA.csv'))
colnames(samp2_raw) <- c('cage', 'dir', 'm_lit_bag_wet', 'm_lit_bag_dry')
bag_samp2 <- mutate(samp2_raw, samp_num = 2)

#==== import sampling 3 data
samp3_raw <- read_csv(here::here('data/litterbag_sampling3_W_S_DATA.csv'))
colnames(samp3_raw) <- c('cage', 'dir', 'm_lit_bag_wet', 'm_lit_bag_dry')
bag_samp3 <- mutate(samp3_raw, samp_num = 3)
n <- dim(bag_samp3)[1]
bag_samp3 <- bag_samp3[1:(n-5),]

# merge sampling1 data with pre-launch data
bag_data <- bind_rows(bag_samp1, bag_samp2, bag_samp3) %>% 
    left_join(bag_prep) %>% 
    group_by(samp_num) %>% 
    mutate(m_lit_wet = m_lit_bag_wet - m_bag, # for the wet mass of litter
           m_wet_diff = m_lit_bag_wet - start_m_lit_bag, # for wet mass diff from the tstart
           m_lit_dry = m_lit_bag_dry - m_bag, # dry mass of litter at this sampling
           # difference in dry litter mass from start
           dry_diff_from_start = m_lit_bag_dry - start_m_lit_dry - m_bag, 
           moisture_m = m_lit_bag_wet - m_lit_bag_dry,
           moisture_percent = moisture_m/m_lit_wet*100, # what is this a percentage of?
           bag_sampID = paste0(cage, dir, samp_num)) %>% 
    select(bag_sampID, cage, replicate, treatment, dir, everything())

# calculate mean moisture percentage and add to each 

mean_moists <- bag_data %>% 
    group_by(samp_num) %>%  #, dir) %>% 
    summarize(samp_moist.perc_mean = mean(moisture_percent, na.rm=TRUE),
              samp_moist.perc_se = se(moisture_percent))

lit_moist <- lit_samps %>% 
    left_join(mean_moists)
lit_moist[1,]$samp_moist.perc_mean <- lit0_moist[[1]] * 100
lit_moist[1,]$samp_moist.perc_se <- lit0_moist[[2]] * 100

#====== create the MASTER tibble for all litterbag data

# calculate the average moisture and infer dry masses
bag_data_all <- bag_data %>% 
    left_join(lit_moist, by="samp_num") %>% 
    rename(moist_mean.perc = samp_moist.perc_mean,
           moist_se.perc = samp_moist.perc_se)  %>% 
    mutate(dry_infer = m_lit_wet - (moist_mean.perc/100*m_lit_wet),
           bag_sampID = paste0(as.character(cage), dir),
           samp_num = as_factor(samp_num)) %>%
    select(bag_sampID, everything())

## last written, 2021.09.11
# write_rds(bag_data_all, here::here('results/litterbag_data_all_samplings.rds'))

# all following subsets are of the above tibble

# inferred dry litterbag masses SUMMARIZED by treatment
bag_data_infer_summ <- bag_data_all %>% 
    group_by(treatment, samp_num) %>% 
    summarize(mean.infer_trt = mean(dry_infer),
              se.infer_trt = se(dry_infer)) %>% 
    na.omit()

#this is summarized by TREATMENT and TIME
bag_data_real_summ <- bag_data_all %>% 
    na.omit() %>% # to get rid of inferred data
    group_by(treatment, samp_num) %>% 
    summarize(mean.real_trt = mean(m_lit_dry),
              se.real_trt = se(m_lit_dry))
