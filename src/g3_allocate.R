#==============================================
# 3. Generate allocation tables to be copied to sheets
#==============================================
library(tidyverse)

# week <- 2
g_sorted <- read_csv(here::here(paste0('results/g2_ghops_include_wk',week,'.csv')))
cage_alloc <- read_csv(here::here('results/g1_updated_cages.csv'))
trt_alloc <- cage_alloc %>% 
    filter(treatment != 'control') %>% 
    select(treatment)

# df of all cage treatments and their corresponding replicate number w/in
pred_trts <- unique(cage_alloc[,c('treatment', 'replicate')]) %>% 
    filter(treatment!= 'control') %>% 
    arrange(treatment, replicate)

# allocate to treatment
g_trt <- g_sorted %>% 
    filter(treatment != 'calib') %>% 
    arrange(ghop_mass) %>% 
    mutate(treatment = trt_alloc$treatment)

# allocate to cage/replicaet
g_incl <- g_trt %>% 
    arrange(treatment, rando) %>% 
    mutate(replicate = pred_trts$replicate)

# try to come up with a more elegant solution that doesn't subset the calib ghops
g_calib <- filter(g_sorted, treatment == 'calib')
ghops_all <- bind_rows(g_calib, g_incl)

datasheet_cages <- left_join(ghops_all, cage_alloc) %>% 
    arrange(cage) %>% 
    select(block, cage, position, treatment, predatorID, ghopID)
datasheet_cages[which(datasheet_cages$treatment=='ghop'),]$treatment <- 'ghop carcass'

# if(week == 5){
#     datasheet_cages[which(predator)]
#     #*** write a function that shifts all spider values up one***
# }

write_csv(datasheet_cages, here::here(paste0('results/g3_allocated_ghops_wk_',week,'.csv')))

ghop_to_freeze <- datasheet_cages %>% 
    filter(treatment == 'calib' | treatment == 'ghop carcass') %>% 
    select(ghopID) %>% 
    arrange(ghopID)
write_csv(ghop_to_freeze,  here::here(paste0('results/g3_allocated_ghops_wk_',week,'_TO.FREEZE.csv')))
