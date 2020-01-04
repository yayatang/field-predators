# library(tidyverse)
library(readr)
library(readxl)
library(tidyverse)
library(here)
library(ggpubr)

substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
}

#==== 
# this is data on the properties of the ghops and predators (e.g. mass, identity)
a_ghops0 <- read_csv(here::here('data/feeding4_assay_ghops.csv'))
a_preds0 <- read_csv(here::here('data/feeding4_assay_predators.csv')) 

colnames(a_ghops0) <- c('ghopID', 'ghop_num', 'm_tube', 'm_tube_ghop', 'calc_m_ghop', 
                       'ghop_fate', 'predatorID', 'comments', 'm_tube_poop')

colnames(a_preds0) <- c('predator_type', 'predatorID', 
                      'm_lid','m_lid_pred_start', 'm_lid_pred_24', 
                      'm_tube_feces0', 'm_tube_feces_wet', 
                      'm_tube_remains0', 'm_tube_remains_wet',
                      'm_tube_feces_dry', 'm_tube_remains_dry',
                      'feeding_observations','pred_fieldID', 'eggsac_notes')
# removing predator MR08 bc it didn't eat
a_preds0 <- filter(a_preds0, predatorID != 'MR08')

# remove all columns that are ONLY NAs
a_ghops0 <- a_ghops0[rowSums(is.na(a_ghops0)) != ncol(a_ghops0),]
a_preds0 <- a_preds0[rowSums(is.na(a_preds0)) != ncol(a_preds0),]

#====
# turn raw, imported data into useful data
# trims ghop data
a_ghops1 <- a_ghops0 %>% 
    mutate(m_ghop_fed = calc_m_ghop - m_tube_poop + m_tube) %>% 
    select(ghopID, m_ghop_fed, ghop_fate, predatorID, comments)

# clean predator data
a_preds1 <- a_preds0 %>% 
    mutate(m_pred_start = m_lid_pred_start - m_lid,
           m_pred_24 = m_lid_pred_24 - m_lid,
           m_feces_wet = m_tube_feces_wet - m_tube_feces0,
           m_remains_wet = m_tube_remains_wet - m_tube_remains0,
           m_feces_dry = m_tube_feces_dry - m_tube_feces0,
           m_remains_dry = m_tube_remains_dry - m_tube_remains0) %>% 
    select(predatorID, predator_type, 
           m_pred_start, m_pred_24, 
           m_feces_wet, m_remains_wet, m_feces_dry, m_remains_dry,
           eggsac_notes, pred_fieldID, feeding_observations)

# melts feces + remains data into one long table
a_preds2 <- a_preds1 %>% 
    pivot_longer(
        cols = m_feces_wet:m_remains_dry,
        names_to = c('sample_type', 'moisture_status'),
        names_pattern = 'm_(.*)_(.*)',
        values_to = 'mass')
a_preds2$sample_type[a_preds2$sample_type == 'remains'] <- 'prey remains'
levels(a_preds2$sample_type) <- c('feces', 'prey remains', 'silk')

a_preds3 <- a_preds2 %>% 
    pivot_wider(
        names_from = moisture_status, 
        values_from = 'mass') %>% 
    rename(mass_wet = wet, 
           mass_dry = dry) %>% 
    mutate(pred_gain_24 = m_pred_24 - m_pred_start) %>% 
    select(predatorID, predator_type, sample_type, mass_wet, mass_dry, pred_gain_24, everything())
a_preds3$sample_type <- as.factor(a_preds3$sample_type)

#====
# this is data from the elemental analysis of the assay
# both the calibration ghops and the predator products
e_plate1 <- read_excel(here::here('data/siel data/YT-F4.1_CN_Rep, SIEL SO#1768.xls'), skip = 2)
e_plate2 <- read_excel(here::here('data/siel data/YT-F4.2-PPg_CN_Rep, SIEL SO#1768.xls'), skip = 2)

colnames(e_plate1) <- c('sampleID', 'mass_mg', 'total_N', 'total_C', 'C_N_ratio')
colnames(e_plate2) <- c('sampleID', 'mass_mg', 'total_N', 'total_C', 'C_N_ratio')

e_0all <- full_join(e_plate1, e_plate2) %>% 
    filter(sampleID != 'spinach') %>% 
    mutate(expID = substr(sampleID, 1,1))

# remove MR08-R because it didn't eat in feeding 4
e_0all <- filter(e_0all, sampleID != 'MR08-R')


# feeding 4 assay is to parameterize the products predators produce in preparation for poopy plants
# poopy plants samples are directly from running the experiment

#======
# split dataset into subsets according to the groups above
e_preds <- e_0all %>% 
    filter(expID == 'M' | expID == 'S') %>% 
    mutate(prod_type = 'pred prods',
           exp_stage = 'assay prep',
           predatorID = sub("\\-.*", "", sampleID),
           sample_type = substrRight(sampleID, 1))
e_preds$sample_type[e_preds$sample_type == 'F'] <- 'feces'
e_preds$sample_type[e_preds$sample_type == 'R'] <- 'prey remains'
e_preds$sample_type[e_preds$sample_type == 'S'] <- 'silk'
e_preds$sample_type <- as.factor(e_preds$sample_type)

e_ghops <- e_0all %>% 
    filter(expID == 'F') %>% 
    mutate(prod_type = 'calib ghops',
           exp_stage = 'assay prep',
           ghopID = substrRight(sampleID, 3),
           sample_type = 'ghop')

#=====
# these are samples that came directly from the experiment, and have no meta data above
e_field <- e_0all %>% 
    filter(expID == 'G') %>% 
    mutate(prod_type = 'calib ghops',
           exp_name = 'exp samples',
           ghop_num = substr(sampleID, 1, 3),
           week_num = substr(sampleID, 5, nchar(sampleID)))

#=====
# calculate elemental composition of feeding 4 calibration ghops

# 1. merge elemental data with earlier data
ghops_all <- left_join(e_ghops, a_ghops1)

# 2. summarize data
summary(ghops_all[c('mass_mg', 'C_N_ratio')])

#====
# calculate elemental composition of feed4 assay predator products
# 1. melt lab data to have prey remains + feces data separately
# a_preds0 <- pivot_longer()

# 2. merge elemental data with lab data
preds_all <- left_join(e_preds, a_preds3)

fact_cols <- c('sample_type', 'prod_type', 'predator_type', 'expID')
preds_all %>% 
    mutate_at(fact_cols, list(~ factor(.))) %>% 
    group_by(predator_type, sample_type)

preds_mantid <- filter(preds_all, predator_type == 'mantid')
preds_spider <- filter(preds_all, predator_type == 'spider')

ggboxplot(preds_mantid, x = 'sample_type', y = 'C_N_ratio')


# 3. summarize + viz
# preds_summ <- summarize(preds_all[c('C_N_ratio')])

