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

# import cage properties and initial bag masses from July 2019
cage_treatments <- read_csv(here::here('results/g1_updated_cages_wk9.csv')) %>% 
    select(-predatorID, -position)

bag_prep_raw <- read_csv(here::here('data/litterbag_DATA_2019.07.11.csv'))
colnames(bag_prep_raw) <- c('cage', 'dir', 'start_lit_wet', 'start_lit_bag')

# clean up imported initial litterbag data
# filter out irrelevant cages + outliers
bag_prep <- bag_prep_raw %>% 
    filter(cage!=3 & cage!=26) %>% 
    filter(!(cage==29 & dir=="E")) %>% # strange mass-gaining outlier in samp 1
    mutate(start_lit_dry = start_lit_wet - (start_lit_wet*lit0_moist[[1]]),
           m_bag = start_lit_bag - start_lit_wet) %>% 
    left_join(cage_treatments, by= 'cage') %>% 
    select(cage, dir, everything())

#==== create sampling 0 data
# bag_wet/ bag_dry are variables for all samplings to use
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

#==== import sampling 3 data
bag_samp3 <- read_csv(here::here('data/litterbag_sampling3_W_S_DATA.csv'))
colnames(bag_samp3) <- c('cage', 'dir', 'bag_wet', 'bag_dry')
bag_samp3 <- mutate(bag_samp3, samp_num = 3)


# merge sampling1 data with pre-launch data
bag_data <- bind_rows(bag_samp0, bag_samp1, bag_samp2, bag_samp3) %>% 
    left_join(bag_prep, by=c('cage', 'dir')) %>% 
    group_by(samp_num) %>% 
    mutate(lit_wet_m = bag_wet - m_bag, # for the wet mass of litter
           wet_diff = bag_wet - start_lit_bag, # for wet mass diff from the tstart
           dry_lit = bag_dry - m_bag, # dry mass of litter at this sampling
           # difference in dry litter mass from start
           dry_diff_from_start = bag_dry - start_lit_dry - m_bag, 
           moist_m = bag_wet - bag_dry,
           moist_percent = moist_m/lit_wet_m*100) %>% # what is this a percentage of?
    # bag_sampID = paste0(cage, dir, samp_num)) %>% # use for NMDS?
    select(cage, replicate, treatment, dir, everything())

# calculate mean moisture percentage for each sampling and join

mean_moists <- bag_data %>% 
    group_by(samp_num) %>%  #, dir) %>% 
    summarize(samp_moist.perc_mean = mean(moist_percent, na.rm=TRUE),
              samp_moist.perc_se = se(moist_percent))

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
    mutate(dry_infer = lit_wet_m - (moist_mean.perc/100*lit_wet_m),
           bag_sampID = paste0(as.character(cage), dir)) %>% 
    select(bag_sampID, everything())
# making specific data edits to get rid of weird artifacts of processing
# remove 29E litterbag outliers
bag_data_all <- bag_data_all[-which(bag_data_all$bag_sampID == '29E'),]

# to see differences in the moisture content of litterbags by direction + sampling
ggplot(bag_data_all,
       aes(x = dry_lit,
           y = moist_percent, 
           group = treatment)) + 
    geom_jitter(aes(color = samp_num, shape = dir), width = 0.2) +
    labs(title = 'dry litter vs moisture by sampling num + direction')

# #=======================
# make a simplified tibble for GLMs
bag_data_simple <- bag_data_all %>%
    select(bag_sampID, cage, treatment, block, dir, samp_num, start_lit_dry,
           dry_lit, dry_diff_from_start)
# 
# ##### checking for normality, QQ plots ####
# # check qqplots of real data, REAL dry mass data + unsummarized
# ggqqplot(bag_data_simple, x= "dry_lit", 
#          title = "qqplot: dry litter")
# shapiro.test(bag_data_simple$dry_lit)
# 
# # check qqplots of dry litter bass loss
# # this include all the ZEROS in dry diff from start
# ggqqplot(bag_data_simple, x= "dry_diff_from_start", 
#          title = "qqplot: dry mass loss from start")
# shapiro.test(bag_data_simple$dry_diff_from_start)
# 
# # trying a glm...
# simple_glm <- glm(formula = dry_lit ~ treatment + block + dir,
#                   data = bag_data_simple)
#                   # family = binomial)
# summary(simple_glm)$coefficients

##### data summarized by treatment ####

# inferred dry litterbag masses SUMMARIZED by treatment
bag_data_infer_summ <- bag_data_all %>% 
    group_by(treatment, samp_num) %>% 
    summarize(mean.infer_trt = mean(dry_infer),
              se.infer_trt = se(dry_infer)) %>% 
    na.omit()

#this is summarized by TREATMENT and TIME
# dry mass loss, as Dror recommended
bag_data_real_summ <- bag_data_all %>% 
    na.omit() %>% # to get rid of inferred data
    group_by(treatment, samp_num) %>% 
    summarize(mean.real_trt = mean(dry_diff_from_start),
              se.real_trt = se(dry_diff_from_start))
