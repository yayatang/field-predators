##### for graphing plant only data #####

library(tidyverse)
library(ggpubr)
library(plotly)
source(here::here('src/yaya_fxns.R'))

cages0 <- read_csv(here::here('data/0_cage_allocation.csv')) %>% 
    filter(cage != 3) %>% 
    select(-predatorID)
cages0$block <- as_factor(cages0$block)
cages0

npp_raw <- read_csv(here::here('data/biomass_DATA.csv'))
colnames(npp_raw) <- c('cage', 'fxnl_group', 'bag_plant_m', 'bag_m', 'comments')

# making the table with SPREAD functional group masses
npp_calc <- npp_raw %>% 
    select(-comments) %>% 
    na.omit() %>% 
    group_by(cage, fxnl_group) %>% 
    mutate(plant_m = bag_plant_m - bag_m)

# add all functional group masses by cage
plant_npp0 <- npp_calc %>% 
    summarize(fxnl_group_m = sum(plant_m)) %>% 
    select(cage, fxnl_group, fxnl_group_m) %>%  
    # comment out from here onward to keep it GATHERED
    spread(fxnl_group, fxnl_group_m) %>% 
    rename(C.arg = 'C. argentatus') %>%
    mutate(cage_npp = C.arg + cereals + legumes + other) # %>% 
# filter(cage != 40,
#        cage != 26) #the cage where the mantid died early

# add cage meta data
plant_npp1 <- plant_npp0 %>% 
    left_join(cages0) %>% 
    mutate_if(is.numeric, round, 4) %>%
    select(cage, block, position, treatment, replicate, everything())
summary(plant_npp1)

# save to csv for sharing
# note: sometimes when writing the file the numbers get rounded strangely
# ending in 999999999 or 00000000001, but only when opening with notepad
write_rds(plant_npp1, here::here('results/plant_productivity_data.rds'))
write_csv(plant_npp1, here::here('results/plant_productivity_data.csv'))

#============ GRAPHING CAGE VS NPP ====
graph_npp1 <- ggplot(plant_npp1,
                     aes(x = block, 
                         y = cage_npp, 
                         group = treatment,
                         text = paste("cage:", cage))) +
    geom_jitter(aes(color = treatment),
                size = 1,width = 0.2) +
    labs(x = "Cage block",
         y = "Productivity, g",
         title = "Cage productivity by treatment") + 
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5))

ggplotly(graph_npp1)

ggsave(here::here('results/plant_productivity_by.block.png'),
       width = 7, height = 5, dpi = 600)

#### npp vs block, by treatment ####
# make a scatter plot comparing npp against block

# plant_npp_summ_block <- plant_npp1 %>% 
#     group_by(block, treatment) %>% 
#     summarize(npp_by_block_mean = mean(cage_npp))
# plant_npp_summ_block
# 
# graph_npp2 <- ggplot(plant_npp_summ_block,
#                      aes(x = block, y = npp_by_block_mean, 
#                          group = treatment)) +
#     geom_jitter(aes(color = treatment),
#                 size = 1,width = 0.2) +
#     labs(title = "Mean productivity by block + treatment")
# ggplotly(graph_npp2)
# ggsave(here::here('results/plant_productivity_by.block.trt.png'),
#        width = 7, height = 5, dpi = 600)

plant_npp_summ_trt <- plant_npp1 %>%
    group_by(treatment) %>% 
    summarize(npp_by_trt_mean = mean(cage_npp),
              npp_by_trt_se = se(cage_npp))
plant_npp_summ_trt

graph_npp_summ <- ggboxplot(plant_npp1,
                            x = "treatment",
                            y = "cage_npp")
graph_npp_summ <- ggplot(plant_npp1,
                         aes(x = treatment, 
                             y = cage_npp, 
                             group = treatment,
                             text = paste("cage:", cage))) +
    geom_boxplot(aes(color = treatment)) +
    labs(x = "Treatment",
         y = "Productivity, g",
         title = "Cage productivity by treatment") + 
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5))

ggplotly(graph_npp_summ)

# ggsave(here::here('results/plant_productivity_trt_boxplot.png'),
#        width = 7, height = 5, dpi = 600)

#### run ANOVA for all cages against NPP ####
# one way anova with blocks as random factor
# this is actually a linear mixed model
library(lme4)
library(lmerTest)
npp_mod <- lmer(cage_npp ~ treatment + (1|block), data = plant_npp1, REML = TRUE)
anova(npp_mod)
rand(npp_mod)

# ANOVAS without blocks as a RANDOM factor, likely meaningless
# # regular ANOVAS
# npp_aov  <- aov(cage_npp ~ treatment + block, data = plant_npp1)
# summary(npp_aov)
# # TukeyHSD(npp_aov)
# 
# fxl_cer  <- aov(cereals ~ treatment + block, data = plant_npp1)
# summary(fxl_cer)
# # TukeyHSD(fxl_cer)
# 
# fxl_leg  <- aov(legumes ~ treatment + block, data = plant_npp1)
# summary(fxl_leg)
# # TukeyHSD(fxl_leg)
# 
# fxl_C.arg  <- aov(C.arg ~ treatment + block, data = plant_npp1)
# summary(fxl_C.arg)
# # TukeyHSD(fxl_C.arg)

##### graphing data by functional group productivity GATHER ####
plant_npp1_long <- plant_npp1 %>% 
    gather(fxl_group, fxl_mass, C.arg:other) %>% 
    mutate(percent_NPP = fxl_mass / cage_npp) %>% 
    arrange(desc(fxl_mass))

# write_csv(plant_npp1_long, here::here('results/plant_biomass_long.csv'))

ggplot(plant_npp1_long, aes(fxl_group, fill = treatment)) + 
    geom_histogram() + 
    facet_grid(cols = vars(fxl_group))


# # running an ANOVA with the functional group masses
# fxl_aov  <- aov(cage_npp ~ treatment + block, data = plant_npp1_long)
# summary(fxl_aov)

# # MANOVA with cage npp and functional group mass?
# fxl_manov <- manova(cbind(cage_npp, fxl_mass) ~ treatment + block + fxl_group, data = plant_npp1_long)
# summary(fxl_manov)

# scatterplot by fxnl group
gr_fxnl <- ggplot(plant_npp1_long,
                  aes(fxl_group, 
                      fxl_mass,
                      color = treatment))+
    geom_jitter(width = 0.2) +
    labs(x = "Functional group",
         y = "Productivity, g",
         title = "Plant productivity by functional group") + 
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5))

ggplotly(gr_fxnl)
ggsave(here::here('results/plant_productivity_fxl_groups_scatter.png'),
       width = 7, height = 5, dpi = 600)


gr_fxl_all <- ggplot(plant_npp1_long,
                     # gr_fxl <- ggplot(filter(plant_npp1_long, fxl_group == 'legumes'),
                     aes(cage, 
                         fxl_mass,
                         fill = fxl_group))+
    # geom_jitter(width = 0.2, 
    #             size = 1.5,
    #             aes(color = treatment, 
    #                 shape = fxl_group)) +
    geom_bar(position = 'dodge', stat = 'identity') + 
    labs(x = "Cage",
         y = "Productivity, g",
         title = "Plant productivity by functional group",
         fill = "Functional group") + 
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5))

ggplotly(gr_fxl_all)

ggsave(here::here('results/plant_productivity_fxl_groups_all.png'),
       width = 7, height = 5, dpi = 600)



## graphing only one of the functional groups
group_2_gr <- 'cereals'

gr_fxl1 <- ggplot(filter(plant_npp1_long, fxl_group == group_2_gr),
                  aes(block, 
                      fxl_mass,
                      group = fxl_group)) +
    geom_jitter(width = 0.2, size = 1.5,
                aes(color = treatment)) +
    labs(x = paste("Functional group,", group_2_gr),
         y = "Productivity, g",
         title = paste("Plant productivity by functional group: ", group_2_gr)) + 
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5))

ggplotly(gr_fxl1)
ggsave(paste0(here::here('results'),'/plant_productivity_fxl_group_', group_2_gr,'.png'),
       width = 7, height = 5, dpi = 600)

#### stacked barchart of NPP per fxl group by treatment ####
library(viridis)
# plant_npp1_long$cage <- as_factor(plant_npp1_long$cage)
# gr_fxl_stack <- ggplot(plant_npp1_long,
#                       aes(cage, 
#                           percent_NPP,
#                           group = treatment,
#                           fill = treatment))+
#     geom_bar(stat = 'identity') + 
#     labs(x = "Cage",
#          y = "Percent NPP by fxnl group",
#          title = "Plant productivity by functional group, per cage") + 
#     scale_fill_viridis(discrete = T) +
#     theme_bw() + 
#     theme(plot.title = element_text(hjust = 0.5))
# 
# ggplotly(gr_fxl_stack)

#### summarizing by treatment #####

plant_npp_gath_summ <- plant_npp1_long %>%
    group_by(treatment, fxl_group) %>% 
    summarize(npp_fxl_mean = mean(fxl_mass),
              npp_fxl_se = se(fxl_mass))
plant_npp_gath_summ

gr_fxl_summ <- ggplot(plant_npp_gath_summ,
                      aes(treatment, 
                          npp_fxl_mean,
                          group = fxl_group,
                          fill = fxl_group))+
    geom_bar(position = 'dodge', stat = 'identity') + 
    geom_errorbar(aes(ymin = npp_fxl_mean - npp_fxl_se, 
                      ymax = npp_fxl_mean + npp_fxl_se), 
                  width=.2,
                  position=position_dodge(.9)) +
    labs(x = "Treatment",
         y = "Productivity, g",
         title = "Plant productivity by functional group") + 
    scale_fill_viridis(discrete = T) +
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5))

ggplotly(gr_fxl_summ)

ggsave(here::here('results/plant_productivity_fxl_groups_summ_dodge.png'),
       width = 5, height = 4, dpi = 600)


##### LMM models on summarized data? is this valid? ####
npp_aov_summ <- aov(npp_fxl_mean ~ treatment + fxl_group, data = plant_npp_gath_summ)
summary(npp_aov_summ)

npp_aov_summ2 <- aov(npp_fxl_mean ~ treatment + fxl_group + treatment*fxl_group, data = plant_npp_gath_summ)
summary(npp_aov_summ2)


npp_aov_summ3 <- lm(npp_fxl_mean ~ treatment + fxl_group + treatment:fxl_group, data = plant_npp_gath_summ)
summary(npp_aov_summ3)


## do a simple chi square, proportion of each functional group against 


# #======== GRAPHING NPP VS SPP NUM, BUT NO DATA MERGE YET####
# npp_v_rich <- ggplot(plant_npp1,
#                      aes(x = num_spp,
#                          y = cage_npp,
#                          group = treatment,
#                          text = cage)) +
#     geom_point(aes(color = treatment))
# # geom_jitter(aes(color = treatment), width = 0.2)
# ggplotly(npp_v_rich)


#### JUST MESSING AROUND BELOW, NMDS ####
# # NMDS
# #
# library(vegan)
# 
# plant_4nmds <- plant_npp1 %>% 
#     mutate(cageID = paste0(cage, substring(treatment, 1,1))) %>% 
#     select(-position, -treatment, -replicate, -predatorID)
# plant_4nmds <- as.matrix(plant_4nmds)
# 
# set.seed(20200419)
# plantNPP.nmds.bray <- metaMDS(plant_4nmds, distance="bray")
