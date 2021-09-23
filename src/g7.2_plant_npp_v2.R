##### for graphing plant only data #####

library(ggpubr)
library(plotly)
library(viridis)
library(lmerTest)
library(emmeans)
library(multcomp)
library(lme4)
library(tidyverse)

source(here::here('src/yaya_fxns.R'))
source("C:/Users/yaya/Dropbox/1 Ecologist/2 EXPERIMENTS/yaya_r_themes.R")

plot_levels <- c('ghop', 'mantid', 'spider', 'control')
model_levels <- c('control', 'ghop', 'mantid', 'spider')

theme_npp <- function() {
    theme_bw() + 
        theme(axis.text.x = element_text(vjust = 1, hjust =0.5),
              panel.background = element_rect(fill = "#f9f9f9",
                                              colour = "#f9f9f9"),
              panel.border = element_rect(color = "black", fill = NA)) +
        theme(plot.title = element_text(hjust = 0.5))
    
}

## import cage meta data
cages0 <- read_csv(here::here('results/g1_updated_cages_wk9.csv')) %>% 
    filter(cage != 3) %>% 
    select(-predatorID)
cages0$block <- as_factor(cages0$block)
cages0

# data import updated 2021 feb, more simple/streamlined
npp_raw <- read_csv(here::here('data/biomass_DATA_master_2021.02.csv'))
colnames(npp_raw) <- c('cage', 'fxl_group', 'm_group')
npp_raw[which(npp_raw$fxl_group == 'C. argentatus'),]$fxl_group <- 'C.arg'

# making the table with SPREAD functional group masses

## CREATING THE MASTER DATA FRAME ####
# add all functional group masses by cage
plant_npp <- npp_raw %>% 
    na.omit() %>% 
    group_by(cage, fxl_group) %>% 
    summarize(cage = first(cage), 
              fxl_group = first(fxl_group), 
              m_group = sum(m_group)) %>% 
    ungroup() %>% 
    left_join(cages0) %>% 
    select(cage, block, position, treatment, replicate, everything()) %>%
    filter(cage != 40, # the cage with MASSIVE amounts of legumes
           cage != 26) %>%  #the cage where the mantid died very early
    mutate_if(is.numeric, round, 4) %>%
    # pivot_longer(names_to = 'fxl_group', values_to = 'm_group', C.arg:other) %>% 
    mutate(treatment = as_factor(treatment),
           treatment = fct_relevel(treatment, c('ghop', 'mantid', 'spider', 'control'))) %>%
    arrange(cage, fxl_group)
summary(plant_npp)
# View(plant_npp)

plant_npp_cage <- plant_npp %>% 
    group_by(cage) %>% 
    summarize(cage = first(cage),
              block = first(block),
              treatment = first(treatment),
              cage_npp = sum(m_group))

plant_npp_percent <- plant_npp %>% 
    left_join(plant_npp_cage) %>% 
    mutate(percent_cage_npp = m_group / cage_npp) %>% 
    select(-cage_npp)


# the commented out lines is OLDER code for saving the data, not in long format 
# save to rds for sharing
# note: sometimes when writing the file the numbers get rounded strangely
# ending in 999999999 or 00000000001, but only when opening with notepad
write_rds(plant_npp, here::here('results/data_plant_biomass.rds'))

# Pre-check: visualizing cage vs npp ####
(npp_cage <- plant_npp_cage %>% 
     ggplot(aes(x = block, 
                y = cage_npp, 
                group = cage,
                fill = treatment,
                text = paste("cage:", cage))) +
     geom_col(position = position_dodge(),
              color = 'black') +
     scale_fill_viridis(name = 'Treatment', 
                        # labels = c('Grasshopper', 'Mantid', 'Spider', 'Litter-only\ncontrol'), 
                        discrete = TRUE) +
     labs(title = 'Total plant biomass by cage',
          x = 'Cage block',
          y = 'Total biomass (g)',
          color = 'Treatment') +
     theme_npp()
)

# ggplotly(npp_cage)
# ggsave(here::here('results/plant_productivity_by.block.png'),width = 7, height = 5, dpi = 600)


## ..Figure: overall plant productivity -----
(plant_npp_cage %>%
     group_by(treatment) %>% 
     mutate(treatment = fct_relevel(treatment, plot_levels)) %>% 
     summarize(mean_cage_npp = mean(cage_npp),
               se_cage_npp = se(cage_npp)) %>% 
     ggplot(aes(x = treatment, 
                y = mean_cage_npp, 
                fill = treatment)) +
     geom_col(color = 'black') +
     geom_errorbar(aes( ymin = mean_cage_npp - se_cage_npp, 
                        ymax = mean_cage_npp + se_cage_npp),
                   width = 0.2) +
     scale_fill_viridis(name = '', discrete = T) +
     labs(title = 'Cage plant productivity by treatment',
          x = 'Treatment',
          y = 'Total plant biomass (g)',
          color = 'Treatment') +
     scale_x_discrete(labels=c('Grasshopper', 'Mantid', 'Spider', 'Control')) +
     theme_yaya() + 
     theme(legend.position = 'none',
           plot.title = element_blank())
)

my_ggsave(here::here('results/7.2_plant_productivity_trt.png'))

# ggsave(here::here('results/7.2_plant_productivity_trt.png'),
# width = 5.5, height = 5, dpi = 600)

## Data analysis: cage npp by treatment----

plant_npp_cage$treatment <- fct_relevel(plant_npp_cage$treatment, model_levels)

lmm_npp_cage <- lmer(cage_npp ~ treatment + (1|block), data = plant_npp_cage)
summary(lmm_npp_cage) ## non significant

plot(lmm_npp_cage)
# these lines are for model selection, not for looking at factors within the model
anova(lmm_npp_cage)
rand(lmm_npp_cage)

# running emmeans ("estimating marginal means") as an example, no significance found above
lmm_npp_cage.emm <- emmeans(lmm_npp_cage, "treatment")
pairs(lmm_npp_cage.emm)

##=======from the Coding Club tutorial for basic linear models
# #1
# Checking that the residuals are normally distributed
# Extracting the residuals
lmm_npp_cage.resid <- resid(lmm_npp_cage)
# Using the Shapiro-Wilk test
shapiro.test(lmm_npp_cage.resid) ## non significant

# The null hypothesis of normal distribution is accepted: there is no significant difference (p > 0.05) from a normal distribution

# 2
# Checking for homoscedasticity
bartlett.test(cage_npp ~ treatment, data = plant_npp_cage)  # Note that these two ways of writing the code give the same results
# The null hypothesis of homoscedasticity is accepted

#======= from the UIC info page
# 1
# data follows a line, linearity
lmm_npp_cage.linearity <- plot(resid(lmm_npp_cage), plant_npp_cage$cage_npp)

# 2
# homoskedascity
#for this portion of the analysis, we need to revisit about statistical significance - since the assumption is that the variance is not going to differ, we would hope to see NO STATISTICAL DIFFERENCES in the following procedure (i.e. p>0.05) to confirm that -

plant_npp_cage$lmm_npp_cage.Res<- residuals(lmm_npp_cage) #extracts the residuals and places them in a new column in our original data table
plant_npp_cage$Abs.lmm_npp_cage.Res <-abs(plant_npp_cage$lmm_npp_cage.Res) #creates a new column with the absolute value of the residuals
plant_npp_cage$lmm_npp_cage.Res2 <- plant_npp_cage$Abs.lmm_npp_cage.Res^2 #squares the absolute values of the residuals to provide the more robust estimate
Levene.lmm_npp_cage <- lm(lmm_npp_cage.Res2 ~ block, data=plant_npp_cage) #ANOVA of the squared residuals
anova(Levene.lmm_npp_cage) #displays the results
## p > 0.05. huzzah!

# 3
library(lattice)
qqmath(lmm_npp_cage, id=0.05)


## ..Figure:  plant mass by fxl group -----
# simple histogram for the distribution of masses by fxl group

hist(plant_npp$m_group)

plant_npp %>% 
    mutate(treatment = fct_relevel(treatment, plot_levels)) %>%
    group_by(treatment, fxl_group) %>% 
    summarize(mean_m_group = mean(m_group),
              se_m_group = se(m_group)) %>% 
    # View()
    ggplot(aes(treatment, 
               mean_m_group, 
               fill = fxl_group))+
    geom_col(position = position_dodge(),
             color = 'black') +
    geom_errorbar(aes( ymin = mean_m_group - se_m_group,
                       ymax = mean_m_group + se_m_group),
                  width = 0.2,
                  position = position_dodge(width = 0.9)) +
    # geom_jitter(data = plant_npp,
    #             aes(treatment, m_group, color = fxl_group),
    #             width = 0.2,
    #             position = position_jitterdodge()) +
    # geom_point(data = plant_npp, 
    #            aes(treatment, m_group, fill = fxl_group)) +
    scale_fill_viridis(name = 'Functional group', option = 'magma', discrete = TRUE) +
    labs(title = 'Plant productivity, by functional group',
         x = 'Treatment',
         y = 'Total plant biomass (g)',
         color = 'Treatment') +
    scale_x_discrete(labels=c('Grasshopper', 'Mantid', 'Spider', 'Control')) +
    theme_yaya() + 
    theme(legend.position = 'none',
          plot.title = element_blank())


my_ggsave(here::here('results/7.2_plant_productivity_trt_fxl_group.png'))

## Data analysis: functional group productivity by treatment----
plant_npp$treatment <- fct_relevel(plant_npp$treatment, model_levels)

## using / for nested random effects, or :
## removed the cage random factor because it doesn't make sense and there's singularity issues

lmm_fxl_npp0 <- lmer(m_group ~ treatment + fxl_group + (1|block), 
                     data = plant_npp)
summary(lmm_fxl_npp0) ## basically this result confirms the fact that each fxl group generate different biomasses. not relevant.

lmm_fxl_npp <- lmer(m_group ~ treatment * fxl_group + (1|block), data = plant_npp)
summary(lmm_fxl_npp)

#**** 
anova(lmm_fxl_npp0, lmm_fxl_npp)
#****
# what does this mean? is it even relevant?^^^^

anova(lmm_fxl_npp) # not necessary bc there is interaction significance
rand(lmm_fxl_npp)

### comparing between treatments in model without interactions
lmm_fxl_npp0.emm <- emmeans(lmm_fxl_npp0, "fxl_group")
pairs(lmm_fxl_npp0.emm)



## results say misleading due to interactions
lmm_fxl_npp.emm <- emmeans(lmm_fxl_npp, "fxl_group")
pairs(lmm_fxl_npp.emm)


### example for using emmeans
pigs.lm <- lm(log(conc) ~ source + factor(percent), data = pigs)
pigs.emm.s <- emmeans(pigs.lm, "source")
pairs(pigs.emm.s)


## ..Figure: C arg productivity by treatment----

(npp_c.arg <- plant_npp %>% 
     filter(fxl_group == 'C.arg') %>% 
     group_by(treatment) %>% 
     mutate(treatment = fct_relevel(treatment, c('ghop','mantid', 'spider', 'control'))) %>%      # View()
     summarize(mean_m_group = mean(m_group),
               se_m_group = se(m_group),
               treatment = first(treatment)) %>% 
     ggplot(aes(treatment, 
                mean_m_group,
                fill = treatment))+
     geom_bar(position = 'dodge', stat = 'identity') + 
     geom_errorbar(aes(ymin = mean_m_group - se_m_group, 
                       ymax = mean_m_group + se_m_group), 
                   width=.2,
                   position=position_dodge(.9)) +
     scale_fill_viridis(discrete = T, name = 'Functional group') +
     labs(title = "Plant productivity by functional group: C. argentatus",
          x = 'Treatment',
          y = 'Total plant biomass (g)',
          color = 'Treatment') +
     scale_x_discrete(labels=c('Grasshopper', 'Mantid', 'Spider', 'Control')) +
     theme_yaya() + 
     theme(legend.position = 'none',
           plot.title = element_blank())
)

my_ggsave(here::here('results/7.2_plant_productivity_C.arg.png'))

## Data analysis: linear model for C.arg npp ####
plant_npp$treatment <- fct_relevel(plant_npp$treatment, model_levels)

lmm_npp_c.arg <- lmer(m_group ~ treatment + (1|block),
                      filter(plant_npp, fxl_group == 'C.arg'))
summary(lmm_npp_c.arg)
emmeans(lmm_npp_c.arg, pairwise ~ treatment) #method 1


anova(lmm_npp_c.arg)
rand(lmm_npp_c.arg)
plot(lmm_npp_c.arg)

## ..Figure: cereals productivity by treatment----

(npp_cereals <- plant_npp %>% 
     group_by(treatment) %>% 
     filter(fxl_group == 'cereals') %>% 
     mutate(treatment = fct_relevel(treatment, c('ghop','mantid', 'spider', 'control'))) %>% 
     # View()
     summarize(mean_m_group = mean(m_group),
               se_m_group = se(m_group),
               treatment = first(treatment)) %>% 
     ggplot(aes(treatment, 
                mean_m_group,
                fill = treatment))+
     geom_bar(position = 'dodge', stat = 'identity') + 
     geom_errorbar(aes(ymin = mean_m_group - se_m_group, 
                       ymax = mean_m_group + se_m_group), 
                   width=.2,
                   position=position_dodge(.9)) +
     scale_fill_viridis(discrete = T, name = 'Functional group') +
     
     labs(x = "Treatment",
          y = "Total biomass, g",
          title = "Plant productivity by functional group: cereals") + 
     scale_x_discrete(labels=c('Grasshopper', 'Mantid', 'Spider', 'Litter-only\ncontrol')) +
     theme_npp()
)
# npp_cereals

## Data analysis: linear model for cereals npp ####
plant_npp$treatment <- fct_relevel(plant_npp$treatment, model_levels)

lmm_npp_cereals <- lme4::lmer(m_group ~ treatment + (1|block),
                              filter(plant_npp, fxl_group == 'cereals'))
summary(lmm_npp_cereals) ## ghop and mantid different from controls


lmm_npp_cereals2 <- lmerTest::lmer(m_group ~ treatment + (1|block),
                                   filter(plant_npp, fxl_group == 'cereals'))
summary(lmm_npp_cereals2) ## ghop and mantid different from 

## so check the difference between levels
# method 1: emmeans "estimating marginal means" aka least squares means
emmeans(lmm_npp_cereals, pairwise ~ treatment, lmer.df="satterthwaite") #method 1
emmeans(lmm_npp_cereals2, pairwise ~ treatment) #method 1


# running glht(), method 2
post.hoc <- glht(lmm_npp_cereals, linfct = mcp(treatment = 'Tukey'))
# displaying the result table with summary()
summary(post.hoc)

lmerTest::anova(lmm_npp_cereals)
rand(lmm_npp_cereals)
plot(lmm_npp_cereals)


## nevo method!
lmm_npp_cereals_null <- lme4::lmer(m_group ~ (1|block),
                                   filter(plant_npp, fxl_group == 'cereals'))
anova(lmm_npp_cereals)
anova(lmm_npp_cereals_null, lmm_npp_cereals)
emmeans(lmm_npp_cereals, pairwise ~ treatment)


## ..Figure: legumes productivity by treatment----
(npp_legumes <- plant_npp %>% 
     group_by(treatment) %>% 
     filter(fxl_group == 'legumes') %>% 
     mutate(treatment = fct_relevel(treatment, c('ghop','mantid', 'spider', 'control'))) %>% 
     # View()
     summarize(mean_m_group = mean(m_group),
               se_m_group = se(m_group),
               treatment = first(treatment)) %>% 
     ggplot(aes(treatment, 
                mean_m_group,
                fill = treatment))+
     geom_bar(position = 'dodge', stat = 'identity') + 
     geom_errorbar(aes(ymin = mean_m_group - se_m_group, 
                       ymax = mean_m_group + se_m_group), 
                   width=.2,
                   position=position_dodge(.9)) +
     scale_fill_viridis(discrete = T, name = 'Functional group') +
     
     labs(x = "Treatment",
          y = "Total biomass, g",
          title = "Plant productivity by functional group: legumes") + 
     # scale_x_discrete(labels=c('Grasshopper', 'Mantid', 'Spider', 'Litter-only\ncontrol')) +
     theme_npp()
)


## Data analysis: linear model for legumes npp -----
plant_npp$treatment <- fct_relevel(plant_npp$treatment, model_levels)

lmm_npp_legumes <- lmer(m_group ~ treatment + (1|block),
                        filter(plant_npp, fxl_group == 'legumes'))
summary(lmm_npp_legumes)
plot(lmm_npp_legumes)

emmeans(lmm_npp_legumes, pairwise ~ treatment, lmer.df="satterthwaite") #method 1




## ..Figure: other productivity by treatment----

(npp_other <- plant_npp %>% 
     group_by(treatment) %>% 
     filter(fxl_group == 'other') %>% 
     mutate(treatment = fct_relevel(treatment, c('ghop','mantid', 'spider', 'control'))) %>% 
     # View()
     summarize(mean_m_group = mean(m_group),
               se_m_group = se(m_group),
               treatment = first(treatment)) %>% 
     ggplot(aes(treatment, 
                mean_m_group,
                fill = treatment))+
     geom_bar(position = 'dodge', stat = 'identity') + 
     geom_errorbar(aes(ymin = mean_m_group - se_m_group, 
                       ymax = mean_m_group + se_m_group), 
                   width=.2,
                   position=position_dodge(.9)) +
     scale_fill_viridis(discrete = T, name = 'Functional group') +
     
     labs(x = "Treatment",
          y = "Total biomass, g",
          title = "Plant productivity by functional group: other") + 
     scale_x_discrete(labels=c('Grasshopper', 'Mantid', 'Spider', 'Litter-only\ncontrol')) +
     theme_npp()
)

## Data analysis: linear model for other npp ####
plant_npp$treatment <- fct_relevel(plant_npp$treatment, model_levels)

lmm_npp_other <- lmer(m_group ~ treatment + (1|block),
                      filter(plant_npp, fxl_group == 'other'))
summary(lmm_npp_other)
emmeans(lmm_npp_other, pairwise ~ treatment) #method 1
post.hoc <- glht(lmm_npp_other, linfct = mcp(treatment = 'Tukey')) # method 2
summary(post.hoc)


####..Figure: plant dominance (aka percent of total biomass) .. ####

### ..Figure: stacked barchart of fxl group  %NPP by treatment ####
plant_npp_percent %>% 
    # mutate(cage = as_factor(cage)) %>% 
    ggplot(aes(cage, 
               # percent_cage_npp,
               m_group,
               # group = treatment,
               fill = fxl_group))+
    geom_col(position = "fill") +
    scale_y_continuous(labels = scales::percent) + 
    scale_fill_viridis(discrete = T, name = 'Functional group') +
    labs(x = "Cage",
         y = "Percent NPP by fxnl group",
         title = "Plant functional group %NPP, per cage") + 
    theme_npp() +
    theme(axis.text.x=element_blank(),
          axis.ticks = element_blank()) + 
    facet_grid(. ~ treatment)

