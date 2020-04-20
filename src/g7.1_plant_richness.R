# for importing plant data
# output: two RDS files 
# 1) 

library(tidyverse)
library(plotly)

cages0 <- read_csv(here::here('data/0_cage_allocation.csv')) %>% 
    filter(cage != 3)
cages0

plant_raw <- read_csv(here::here('data/plant comm/plant species DATA.csv')) %>% 
    rename(cage = Cage)
    
plant_spp0 <- plant_raw %>% 
    mutate(num_spp = rowSums(.)) %>% 
    select(cage, num_spp) %>% 
    right_join(cages0)

# check qqplot 
qqnorm(plant_spp0$num_spp)

# plot cages against number of species
plot_spp <- ggplot(plant_spp0,
       aes(x = cage, y = num_spp, group = treatment)) +
        geom_jitter(aes(color = treatment), width = 0.2)
ggplotly(plot_spp)

plant_aov <- aov(num_spp ~ treatment + block, data = plant_spp0)
summary(plant_aov)


#============NMDS testing
library(vegan)
plant_4nmds <- as.matrix(plant_raw)

set.seed(20200419)
plant_rich.nmds <- metaMDS(plant_4nmds, distance="bray", try = 100)
plant_rich.nmds

#plotted, but doesn't say a whole lot...
plot(plant_rich.nmds)

#======== convert in a form that ggplot can handle
#extract NMDS scores (x and y coordinates)
richness.scores = as.data.frame(scores(plant_rich.nmds))

#add columns to data frame 
plant_rich.nmds$cage <- plant_spp0$cage
plant_rich.nmds$treatment <- plant_spp0$treatment
plant_rich.nmds$block <- plant_spp0$block

g_richness_nmds <- ggplot(plant_rich.nmds, 
                          aes(x = NMDS1, y = NMDS2)) + 
    geom_point(size = 9, aes(shape = cage, colour = treatment)
    theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"), 
          axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
          legend.text = element_text(size = 12, face ="bold", colour ="black"), 
          legend.position = "right", axis.title.y = element_text(face = "bold", size = 14), 
          axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
          legend.title = element_text(size = 14, colour = "black", face = "bold"), 
          panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
          legend.key=element_blank()) + 
    labs(x = "NMDS1", colour = "treatment", y = "NMDS2", shape = "cage")
    # scale_colour_manual(values = c("#009E73", "#E69F00")) 


g_richness_nmds

ggsave("NMDS.svg")