#==============================================
# 2. Choose the set of ghops to use
#==============================================
library(tidyverse)

# week <- 2
g_sorted <- read_csv(here::here(paste0('results/g2_ghops_include_wk',week,'.csv')))
cage_alloc <- read_csv(here::here('results/1_updated_cages.csv'))
standin_alloc <- read_csv(here::here('data/wk4_temp_alloc.csv'))

standin_alloc <- standin_alloc %>% 
    filter(treatment != 'control')

pred_trts <- unique(cage_alloc[,c('treatment', 'replicate')]) %>% 
    filter(treatment!= 'control') %>% 
    arrange(treatment, replicate)

# allocate to treatment
g_trt <- g_sorted %>% 
    filter(treatment != 'calib') %>% 
    arrange(ghop_mass) %>% 
    mutate(treatment = standin_alloc$treatment)

# allocate to cage/replicate
g_incl <- g_trt %>% 
    arrange(treatment, rando) %>% 
    mutate(replicate = pred_trts$replicate)

# try to come up with a more elegant solution that doesn't subset the calib ghops
g_calib <- filter(g_sorted, treatment == 'calib')
ghops_all <- bind_rows(g_calib, g_incl)

datasheet_cages <- left_join(ghops_all, cage_alloc) %>% 
    arrange(cage) %>% 
    select(block, cage, position, treatment, predatorID, ghopID)

write_csv(datasheet_cages, here::here(paste0('results/g3_allocated_ghops_wk_',week,'.csv')))
