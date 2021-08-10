# for importing plant data
# output: two RDS files 
# 1) 

library(tidyverse)
library(plotly)
library(ggpubr)
library(janitor)
library(viridis)
library(lme4)
library(lmerTest)
source(here::here('src/yaya_fxns.R'))
source("C:/Users/yaya/Dropbox/1 Ecologist/2 EXPERIMENTS/yaya_r_themes.R")

set.seed(20200422)

cages0 <- read_csv(here::here('data/0_cage_allocation.csv')) %>% 
    filter(cage != 3,
           cage != 26,
           cage != 40) %>% 
    select(-predatorID)
cages0$block <- as_factor(cages0$block)
cages0

plant_raw <- read_csv(here::here('data/plant comm/plant species DATA - FINAL.csv')) %>% 
    clean_names(case = "sentence", sep_out = "_") %>% 
    rename(cage = Cage)
colnames(plant_raw)

# make the first table that includes all necessary info
plant_spp0 <- plant_raw %>% 
    mutate(num_spp = rowSums(.)) %>% 
    right_join(cages0) %>% 
    select(-replicate)

#======
# transforming the data to suit NMDS
# this means including all the cage metadata, treatments etc
plant_spp1 <- plant_spp0 %>% 
    group_by(block) %>% 
    mutate(cageID = paste0(cage, substr(treatment, 1,1)),
           rank_trt = order(order(num_spp, decreasing = TRUE))) %>% 
    select(cageID, cage, treatment, block, position, rank_trt, num_spp, everything())
plant_spp1$rank_trt <- factor(plant_spp1$rank_trt)
plant_spp1$treatment <- fct_relevel(plant_spp1$treatment, c('ghop', 'mantid', 'spider', 'control'))
# write_csv(plant_spp1[,2:7], here::here('results/plant_spp_data.csv'))

(plant_spp_ranks <- plant_spp1 %>% 
    select(treatment, block, rank_trt) %>% 
    spread(rank_trt, treatment)
)

write_csv(plant_spp_ranks, here::here('results/plant_spp_ranks.csv'))



##----Figure: species count, summarized by treatment ----
plant_spp1 %>% 
    group_by(treatment) %>% 
    mutate(treatment = fct_relevel(treatment, c('ghop', 'mantid', 'spider', 'control'))) %>%
    summarize(mean_num_spp = mean(num_spp),
              se_num_spp = se(num_spp)) %>% 
    ggplot(aes(treatment, mean_num_spp, fill = treatment)) + 
    geom_col(position = 'dodge',
             color = 'black') +
    geom_errorbar(aes( ymin = mean_num_spp - se_num_spp, 
                       ymax = mean_num_spp + se_num_spp),
                  width = 0.15) +
    scale_fill_viridis(name = '', discrete = TRUE) +
    labs(title = 'Plant community composition: species count',
         x = 'Treatment',
         y = 'Number of plant species',
         color = 'Treatment') +
    scale_x_discrete(labels=c('Grasshopper', 'Mantid', 'Spider', 'Control')) + 
    theme_yaya() + 
    theme(legend.position = 'none') +
    theme(plot.title = element_blank())
    
    
my_ggsave(here::here('results/plant_species_count_by_treatment.png'))

#--- Data analysis: species number and block -----
lmer_num_spp <- lmer(num_spp ~ treatment + (1| block), plant_spp1)
summary(lmer_num_spp)
ggqqplot(resid(lmer_num_spp))


# same data, to see the overall pattern across all cages without summarizing          
plant_spp1 %>% 
    group_by(block) %>% 
    ggplot(aes(cage, num_spp, fill = treatment)) + 
    geom_col(position = 'dodge',
             color = 'black') +
    scale_fill_viridis(name = 'Treatment', discrete = TRUE) +
    labs(title = 'Plant community composition: species count',
         x = 'Cage',
         y = 'Number of plant species',
         color = 'Treatment') +
    # scale_x_discrete(labels=c('Grasshopper \ncarcass', 'Mantid', 'Spider', 'Control')) + 
    theme(axis.text.x = element_text(vjust = 1, hjust =0.5),
          panel.background = element_rect(fill = "#f9f9f9",
                                          colour = "#f9f9f9"),
          panel.border = element_rect(color = "black", fill = NA))

# phtest(plant_spp1$treatment, plant_spp1$block)


## OLD spp_mod <- lm(num_spp ~ treatment + (1 + block), 
#               data = plant_spp1)
# spp_lm <- anova(spp_mod)
# spp_lm

plant_spp1$treatment <- fct_relevel(plant_spp1$treatment, c('control', 'ghop', 'mantid', 'spider'))



#############................#################
#############................#################
#############................#################
#############................#################
#############................#################

#### run ANOVA for all cages against spp ####
spp_aov  <- aov(num_spp ~ treatment + block, data = plant_spp1)
summary(spp_aov)

# trying to run fixed and random effects in ANOVA?
library(lme4)
library(lmerTest)

spp_mod <- lmer(num_spp ~ treatment + (1|block), data = plant_spp1, REML = TRUE)
spp_lm <- anova(spp_mod)
spp_lm

####  check guides on doing NMDS ####
# # 
# # ## bray curtis dissimilarity?
# # simper(comm, group, permutations = 0, trace = FALSE, 
# #        parallel = getOption("mc.cores"), ...)
# # ## S3 method for class 'simper'
# # summary(object, ordered = TRUE,
# #         digits = max(3,getOption("digits") - 3), ...)
# 
# #============NMDS testing
# library(vegan)
# plant_4nmds <- as.matrix(plant_spp_trans[,c(6:97)])
# 
# plant_rich.nmds <- metaMDS(plant_4nmds, distance="bray", try = 100)
# plant_rich.nmds
# 
# #plotted, but doesn't say a whole lot...
# plot(plant_rich.nmds)
# 
# #======== convert in a form that ggplot can handle
# #extract NMDS scores (x and y coordinates)
# richness.scores = as_tibble(scores(plant_rich.nmds))
# richness.scores
# 
# #add columns to data frame 
# plant_rich.nmds$cageID <- plant_spp_trans$cageID
# plant_rich.nmds$treatment <- plant_spp_trans$treatment
# plant_rich.nmds$block <- plant_spp_trans$block
# 
# g_richness_nmds <- ggplot(plant_rich.nmds, 
#                           aes(x = NMDS1, y = NMDS2)) + 
#     geom_point(size = 9, aes(shape = cage, colour = treatment)) + 
#     # theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"), 
#     #       axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
#     #       legend.text = element_text(size = 12, face ="bold", colour ="black"), 
#     #       legend.position = "right", axis.title.y = element_text(face = "bold", size = 14), 
#     #       axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
#     #       legend.title = element_text(size = 14, colour = "black", face = "bold"), 
#     #       panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
#     #       legend.key=element_blank()) + 
#     labs(x = "NMDS1", colour = "treatment", y = "NMDS2")
# # scale_colour_manual(values = c("#009E73", "#E69F00")) 
# 
# g_richness_nmds
# 
# ggsave("NMDS.svg")