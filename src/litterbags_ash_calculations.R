library(tidyverse)
library(plotly)
library(janitor)
library(ggpubr)
library(viridis)
library(here)
source(here::here('src/yaya_fxns.r'))

ash_free_raw <- read_csv(here::here('data/litterbag_ash-free_DATA.csv')) %>% 
    clean_names() %>% 
    mutate(cage = as.numeric(substr(sample_id, 0, 2)),
           dir = factor(substr(sample_id, 3, 3))) %>% 
    filter(!is.na(cage),
           cage != 40)

cage_treatments <- read_csv(here::here('results/g1_updated_cages_wk9.csv')) %>% 
    select(-predatorID, -position)

ash_free_data <- left_join(ash_free_raw, cage_treatments) %>% 
    mutate(treatment = fct_relevel(treatment, c('control', 'ghop', 'mantid', 'spider')),
           litter_sample = pre_oven_dry_mass - crucible_weight,
           litter_total = litter_sample + remaining_litterbag_litter,
           litter_ash = post_oven_ash_mass - crucible_weight,
           ash_proportion = litter_ash / litter_sample,
           litter_corrected = litter_sample * (1 - ash_proportion))

## plot of the number of samples per treatment
ash_free_data %>% 
    ggplot(aes(x = treatment)) + 
    geom_bar()

#####################################

# Data visualisation ----

theme_ash <- function(){  # Creating a function
    theme_classic() +  # Using pre-defined theme as base
        theme(axis.text.x = element_text(size = 12, face = "bold"),  # Customizing axes text      
              axis.text.y = element_text(size = 12, face = "bold"),
              axis.title = element_text(size = 14, face = "bold"),  # Customizing axis title
              panel.grid = element_blank(),  # Taking off the default grid
              plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
              legend.text = element_text(size = 12, face = "italic"),  # Customizing legend text
              legend.title = element_text(size = 12, face = "bold"),  # Customizing legend title
              legend.position = "right",  # Customizing legend position
              plot.caption = element_text(size = 12))  # Customizing plot caption
}                                  

## Creating a plot showing the two directions of litterbag correct ash-free mass
ash_free_data %>% 
    ggplot(aes(x = treatment,
               y = litter_corrected, 
               color = dir)) +
    geom_jitter(width = 0.2) + 
    # scale_color_viridis(name = 'Treatment', labels = c('Control', 'Carcass', 'Mantid', 'Spider'), discrete = TRUE) +
    labs(title = "Litterbag corrected litter mass at end of experiment",
         x = "Treatment",
         y = "Percent ash") +
    expand_limits(y=0)


# Creating a boxplot with ggplot for looking at corrected litter amounts

litter_boxplot_dir <- ggplot(ash_free_data, 
                             aes(x = treatment, 
                                 y = litter_corrected,
                                 fill = dir)) +
    geom_boxplot() +
    geom_jitter() + 
    # scale_fill_manual(values = c("#97F7C5", "#4ED973", "#08873D")) +  
    labs(x = "\nTreatment", y = "Corrected litter mass",  
         caption = "\n Fig.2 Mass of litter in litterbags at the end of the 
                        experiment.") +
    # caption = "\nFig.2") +  # Caption for figure in panel
    expand_limits(y = 0) +
    theme_ash() +  
    theme(legend.position = "none")  # Over-writing our theme() to get rid of legend

ggplotly(litter_boxplot_dir)


# looking at the ash percentages
# should be an inverted plot of the previous one
litter_boxplot_ash <- ggplot(ash_free_data, 
                             aes(x = treatment, 
                                 y = ash_proportion,
                                 fill = dir)) +
    geom_boxplot() +
    geom_jitter() + 
    labs(x = "\nTreatment", y = "Ash proportion",  
         caption = "\n Proportion of ash in each litter sample.") +
    
    expand_limits(y = 0) +
    theme_ash()

ggplotly(litter_boxplot_ash)


### Checking corrected data against original data ####
source(here::here('src/g6.1_litterbag_import_v2.R'))

litter_samp3 <- bag_data_all %>% 
    filter(samp_num == 3) %>% 
    select(cage, treatment, dir, m_lit_dry)

compare_lit <- ash_free_data %>% 
    select(cage, dir, litter_corrected, ash_proportion) %>% 
    left_join(litter_samp3, by= c('cage', 'dir'))

compare_lit %>% 
    ggplot(aes(x = litter_corrected,
               y = m_lit_dry,
               color = treatment)) + 
    geom_point() + 
    labs(x = "\n Corrected ash-free litter mass", 
         y = "Weighed litter in sampling 3") +
    expand_limits(x = 0, y = 0) +
    theme_ash()


compare_lit %>% 
    ggplot(aes(x = ash_proportion,
               y = m_lit_dry,
               color = treatment)) + 
    geom_point() + 
    labs(x = "\n Ash proportion", 
         y = "Weighed litter in sampling 3") +
    expand_limits(x = 0, y = 0) +
    theme_ash()


###########
### ONE-WAY ANOVA (WITH BLOCKING) -----

litter_anova <- aov(litter_corrected ~ treatment + dir + (1|block), data = ash_free_data)
summary(litter_anova)
par(mfrow = c(2,2))
plot(litter_anova)

litter_lm <- lm(litter_corrected ~ treatment + dir +  (1|block), data = ash_free_data)
summary(litter_lm)
plot(litter_lm) ## same model as above
