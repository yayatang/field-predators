# for importing plant data
# output: two RDS files 
# 1) 

library(tidyverse)
library(plotly)
library(ggpubr)
library(janitor)
source(here::here('src/yaya_fxns.R'))

set.seed(20200422)

cages0 <- read_csv(here::here('data/0_cage_allocation.csv')) %>% 
    filter(cage != 3,
           cage !=26) %>% 
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
plant_spp1 <- plant_spp0 %>% 
    group_by(block) %>% 
    mutate(cageID = paste0(cage, substr(treatment, 1,1)),
           rank_trt = order(order(num_spp, decreasing = TRUE))) %>% 
    select(cageID, cage, treatment, block, position, rank_trt, num_spp, everything())
plant_spp1$rank_trt <- factor(plant_spp1$rank_trt)
write_csv(plant_spp1[,2:7], here::here('results/plant_spp_data.csv'))

plant_spp_ranks <- plant_spp1 %>% 
    select(treatment, block, rank_trt) %>% 
    spread(rank_trt, treatment)
write_csv(plant_spp_ranks, here::here('results/plant_spp_ranks.csv'))

# divide each species column into proportion of species present in 
plant_spp_trans <- plant_spp1 %>% 
    mutate_at(vars(8:ncol(plant_spp1)), ~.x/num_spp)

#==============
# # check qqplot, normality measures
# qqnorm(plant_spp1$num_spp)
# shapiro.test(plant_spp1$num_spp)
# ggdensity(plant_spp1$num_spp,
#           main = "Density plot of spp num",
#           xlab = "Species per cage")
# ggqqplot(plant_spp1$num_spp)
# summary(plant_spp1$num_spp)

##### plot blocks against number of species ####
plot_spp <- ggplot(plant_spp1,
                   aes(x = block, y = num_spp, group = treatment,
                       text = paste("cage:", cage))) +
    geom_jitter(aes(color = treatment), width = 0.2, size = 1.5) + 
    theme_bw() +
    labs(title = "number of spp per cage")
ggplotly(plot_spp)

ggsave(here::here('results/plant_richness_by.block.png'),
       width = 7, height = 5, dpi = 600)

# plant_aov <- aov(num_spp ~ treatment + block, data = plant_spp1)
# summary(plant_aov)

##### plant spp summary ####
plant_spp_summ_trt <- plant_spp1 %>%
    group_by(treatment) %>% 
    summarize(spp_by_trt_mean = mean(num_spp),
              spp_by_trt_se = se(num_spp))
plant_spp_summ_trt

graph_spp_summ <- ggboxplot(plant_spp1,
                            x = "treatment",
                            y = "num_spp")
graph_spp_summ <- ggplot(plant_spp1,
                         aes(x = treatment, 
                             y = num_spp, 
                             group = treatment,
                             text = paste("cage:", cage))) +
    geom_boxplot(aes(color = treatment)) +
    labs(x = "Treatment",
         y = "Number of species",
         title = "Species count by treatment") + 
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5))

ggplotly(graph_spp_summ)

ggsave(here::here('results/plant_species_trt_boxplot.png'),
       width = 7, height = 5, dpi = 600)


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