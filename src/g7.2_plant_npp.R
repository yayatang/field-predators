# for graphing plant only data

library(tidyverse)

npp_raw <- read_csv(here::here('data/biomass_DATApartial.csv'))
colnames(npp_raw) <- c('cage', 'fxnl_group', 'bag_plant_m', 'bag_m', 'comments')

# making the table with SPREAD functional group masses
plant_npp0 <- npp_raw %>% 
    select(-comments) %>% 
    na.omit() %>% 
    mutate(fxnl_group_m = bag_plant_m - bag_m) %>% 
    select(cage, fxnl_group, fxnl_group_m) %>%  
    # comment out from here onward to keep it GATHERED
    spread(fxnl_group, fxnl_group_m) %>% 
    rename(C.arg = 'C. argentatus') %>% 
    mutate(cage_npp = C.arg + cereals + legumes + other)

# # uncomment line bundle to keep the summarizing from GATHERED
#=========
# # calculate the toatl NPP per cage
# plant_cage_m <- plant_npp0 %>% 
#     group_by(cage) %>% 
#     summarize(cage_npp = sum(fxnl.group_m))
# 
# merge all cage plant data into one table
plant_npp1 <- plant_npp0 %>%
    left_join(plant_spp0)
    # mutate(try_spp.npp_index = cage_npp / num_spp)

# check qqplot 
qqnorm(plant_npp1$cage_npp)


#============ GRAPHING
# plot cages against number of species
graph_spp <- ggplot(plant_spp0,
                   aes(x = cage, y = num_spp, group = treatment)) +
    geom_jitter(aes(color = treatment), width = 0.2)
ggplotly(graph_spp)

plant_aov <- aov(num_spp ~ treatment + block, data = plant_spp0)
summary(plant_aov)

#=========
graph_npp1 <- ggplot(plant_npp1,
                     aes(x = cage, y = cage_npp, group = treatment)) +
    geom_point(aes(color = treatment))
# geom_jitter(aes(color = treatment), width = 0.2)
ggplotly(graph_npp1)

npp_aov  <- aov(cage_npp ~ treatment + block, data = plant_npp1)
summary(npp_aov)

# JUST MESSING AROUND BELOW
#================
# NMDS
#
library(vegan)

plant_4nmds <- plant_npp1 %>% 
    mutate(cageID = paste0(cage, substring(treatment, 1,1))) %>% 
    select(-position, -treatment, -replicate, -predatorID)
plant_4nmds <- as.matrix(plant_4nmds)

set.seed(20200419)
plantNPP.nmds.bray <- metaMDS(plant_4nmds, distance="bray")
