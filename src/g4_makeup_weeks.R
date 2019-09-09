library(here)
library(tidyverse)
library(zoo)

#==== set seed
# week 8 mantid redistribution randomization
wk8_mantids <- c('M02', 'M03', 'M05', 'M07')
wk8_rand_m <- 20190827

randID_m <- as.integer(wk8_rand_m) # date run for randomization (maybe include in csV)
set.seed(randID_m)

#========
cages_feed_m <- tibble(cageID = c(9, 28, 31, 36))

cages_feed_m$wk8_assign <- runif(4)
cages_feed_m <- cages_feed_m %>% 
    arrange(wk8_assign) %>% 
    mutate(wk8_new_m = wk8_mantids) %>% 
    arrange(cageID)

#===== import ghop mass data
ghops_wk8 <- read_csv(here::here('data/ghop_data_wk8.csv'))
# weighed_g <- read_csv(here::here(paste0('data/ghop_data_wk',week,'.csv')))
ghops_wk8 <- ghops_wk8[,1:3]
colnames(ghops_wk8) <- c('ghop_tube', 'tube_mass', 'tube_ghop_mass')

# import the relevant cages w/ info for the week
receiving_ghops <- nrow(cages_feed_m)
ghops_2weigh <- receiving_ghops + 3


#===== calculate ghop masses
calc_g <- ghops_wk8 %>% 
    mutate(ghop_mass = tube_ghop_mass - tube_mass,
           ghopID = paste0('W',week,".",ghop_tube)) %>% 
    arrange(ghop_mass)

#==========
num_ghops <- ghops_2weigh
g_masses <- calc_g

g_masses <- g_masses %>% 
    mutate(rolled_mean = rollapply(ghop_mass, num_ghops, mean, fill = NA, align='center'),
           rolled_median = rollapply(g_masses$ghop_mass, num_ghops, median, fill = NA, align='center'),
           rolled_diff = rolled_mean - rolled_median)
min_index <- which.min(abs(g_masses$rolled_diff)) - floor(num_ghops/2)
max_index <- min_index + num_ghops - 1

g_masses$include <- FALSE
g_masses[min_index:max_index,]$include <- TRUE
g_masses$rando <- runif(nrow(g_masses))

#===== start allocating calibration ghops
g_masses$treatment <- NA
g_masses[min_index:max_index,]$treatment <- 'mantid'
g_masses[min_index,]$treatment <- 'calib'
g_masses[max_index,]$treatment <- 'calib'
med_g <- floor(ghops_2weigh/2 + min_index)
g_masses[med_g,]$treatment <- 'calib'

#===========
g_write <- g_masses %>% 
    filter(include==TRUE) %>% 
    select(ghopID, ghop_mass, rando, treatment)

write_csv(g_write, here::here(paste0('results/g2_ghops_include_wk',week,'.csv')))

all50 <- left_join(calc_g, g_write) %>% 
    arrange(ghopID)
write_csv(all50, here::here(paste0('results/g2_ghops_all_weighed_wk',week,'.csv')))

#===== allocate ghops
ghop2mant <- g_masses %>% 
    filter(treatment=='mantid')

ghop2freeze <- g_masses %>% 
    filter(treatment=='calib')

ghop_alloc <- ghop2mant %>% 
    select(ghopID, treatment) %>% 
    mutate(ghop_rand = runif(nrow(ghop2mant))) %>% 
    arrange(ghop_rand)

cages_feed_m$ghopID <- ghop_alloc$ghopID
cages_feed_m %>% 
    select(-wk8_assign)

write_csv(cages_feed_m, here::here(paste0('results/g3_allocated_ghops_wk',week,'.csv')))

# cages_feed_m <- cages_feed_m %>% 
#     mutate(randomiz_g = runif(4)) %>% 
#     arrange(randomiz_g) %>% 
#     mutate(ghopID = ghop2mant)


# #================================
# # GHOP ALLOCATION
# #================================
# g_sorted <- read_csv(here::here(paste0('results/g2_ghops_include_wk',week,'.csv')))
# cage_alloc <- cages_feed_m %>% 
#     mutate(treatment='mantid')
# trt_alloc <- cage_alloc %>% 
#     filter(treatment != 'control') %>% 
#     select(treatment)
# 
# # df of all cage treatments and their corresponding replicate number w/in
# pred_trts <- unique(cage_alloc[,c('treatment', 'replicate')]) %>% 
#     filter(treatment!= 'control') %>% 
#     arrange(treatment, replicate)
# 
# # allocate to treatment
# g_trt <- g_sorted %>% 
#     filter(treatment != 'calib') %>% 
#     arrange(ghop_mass) %>% 
#     mutate(treatment = trt_alloc$treatment)
# 
# # allocate to cage/replicaet
# g_incl <- g_trt %>% 
#     arrange(treatment, rando) %>% 
#     mutate(replicate = pred_trts$replicate)
# 
# # try to come up with a more elegant solution that doesn't subset the calib ghops
# g_calib <- filter(g_sorted, treatment == 'calib')
# ghops_all <- bind_rows(g_calib, g_incl)
# 
# datasheet_cages <- left_join(ghops_all, cage_alloc) %>% 
#     arrange(cage) %>% 
#     select(block, cage, position, treatment, predatorID, ghopID)
# datasheet_cages[which(datasheet_cages$treatment=='ghop'),]$treatment <- 'ghop carcass'
# 
# #====
# write_csv(datasheet_cages, here::here(paste0('results/g3_allocated_ghops_wk_',week,'.csv')))
# 
# ghop_to_freeze <- datasheet_cages %>% 
#     filter(treatment == 'calib' | treatment == 'ghop carcass') %>% 
#     select(ghopID) %>% 
#     arrange(ghopID)
# write_csv(ghop_to_freeze,  here::here(paste0('results/g3_allocated_ghops_wk_',week,'_TO.FREEZE.csv')))
