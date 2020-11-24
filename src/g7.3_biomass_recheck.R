# g7 biomass reweigh
library(tidyverse)

reweigh <- read_csv(here::here('data/biomass_reweigh.csv'))
colnames(reweigh) <- c('cage', 'fxl_group', 'total_mass', 'bag_mass', 'comments')
reweigh$bag_mass <- ifelse(reweigh$bag_mass == '-', reweigh$total_mass, reweigh$bag_mass)


reweighed <- reweigh %>% 
    select(-comments) %>% 
    mutate(bag_mass = as.numeric(bag_mass),
           group_mass_yaya = total_mass - bag_mass) %>% 
    select(-total_mass, -bag_mass)

biomass <- read_rds(here::here('results/plant_productivity_data.rds'))
colnames(biomass)
biomass_long <- biomass %>% 
    select(-block, -position) %>% 
    pivot_longer(-c(cage, treatment, replicate, cage_npp),
                 names_to = 'fxl_group', 
                 values_to = 'fxl_group_mass') %>% 
    rename(group_mass_evya = fxl_group_mass)

check_weights <- left_join(reweighed, biomass_long, by = c('cage', 'fxl_group')) %>% 
    mutate(weight_diff = group_mass_yaya - group_mass_evya)
View(check_weights)
