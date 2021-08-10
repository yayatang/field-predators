## making my own theme
library(tidyverse)
library(viridis)
library(lme4)
library(emmeans)

## run the data importing and cleaning script before graphing
source(here::here('src/g8.1_isotope_soil_import_all.R'))


theme_angled <- theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1),
                      panel.background = element_rect(fill = "#f9f9f9",
                                                      colour = "#f9f9f9"),
                      panel.border = element_rect(color = "black", fill = NA))

theme_flat <- theme(axis.text.x = element_text(vjust = 1, hjust =0.5),
                    panel.background = element_rect(fill = "#f9f9f9",
                                                    colour = "#f9f9f9"),
                    panel.border = element_rect(color = "black", fill = NA),
                    legend.position = "none")


##### GRAPHS, exploratory -----
# data exploration
hist(isotopes_e_w$d15n)

isotopes_e_w %>% 
    filter(sample_dir == 'W') %>% 
    select(d15n) %>% 
    ggplot(aes(d15n))+
    geom_histogram(bins = 10)

isotopes_e_w %>% 
    filter(sample_dir == 'E') %>% 
    select(d15n) %>% 
    ggplot(aes(d15n))+
    geom_histogram(bins = 10)

dotchart(isotopes_e_w$d15n, 
         groups = isotopes_e_w$treatment_vec,
         ylab = 'treatment',
         xlab = 'd15n', main = 'cleveland dotplot: d15n',
         pch = isotopes_e_w$treatment_vec)
dotchart(isotopes_e_w$d13c, 
         groups = isotopes_e_w$treatment_vec,
         ylab = 'treatment',
         xlab = 'd13c', main = 'cleveland dotplot: d13c',
         pch = isotopes_e_w$treatment_vec)

ggpairs(select(isotopes_e_w, c('total_percent_n', 'd15n','atom_percent_15n', 'total_percent_c', 'd13c','atom_percent_13c')))

# lengthen dataset and summarize
isotopes_under_long <- isotopes_e_w %>% 
    ungroup() %>% 
    pivot_longer(c('total_percent_n', 'd15n', 'atom_percent_15n', 
                   'total_percent_c', 'd13c', 'atom_percent_13c'), 
                 names_to = "analysis", values_to = "a_value")

isotopes_under_long_summ <- isotopes_under_long %>% 
    group_by(treatment, analysis) %>% 
    summarize(iso_mean = mean(a_value),
              iso_se = se(a_value),
              iso_var = var(a_value),
              iso_n = n())

# bar plot of all the variables
isotopes_under_long_summ$treatment <- fct_relevel(isotopes_under_long_summ$treatment, c('ghop', 'mantid', 'spider','control')) 

isotopes_under_long_summ %>% 
    ggplot(aes(fill = treatment, x = analysis, y = iso_mean))+
    geom_bar(position = 'dodge', stat = 'identity')+
    geom_errorbar(aes(ymin=iso_mean-iso_se, ymax= iso_mean+iso_se), width=.2,
                  position=position_dodge(.9)) +
    scale_fill_viridis(name = 'Grasshopper fate', discrete = TRUE) +
    labs(title = 'Comparing all elemental analyses by treatment',
         x = 'Analysis',
         y = 'Mean +- se') +
    scale_x_discrete(labels=c('Atom percent 13C', 'Atom percent 15N',
                              'd13C', 'd15N',
                              'Total percent C', 'Total percent N')) + 
    theme_angled


# # boxplot with jitter for d15N
# isotopes_e_w %>% 
#     filter(sample_dir == 'W') %>% 
#     mutate(treatment = fct_relevel(treatment,  c('ghop', 'mantid', 'spider','control')))
# %>% 
#     ggplot(aes(x = treatment, y = d15n, fill = treatment))+
#     geom_boxplot() +
#     geom_jitter(width = 0.1)+
#     scale_fill_viridis(name = '', discrete = TRUE) +
#     labs(title = 'd15N of soil beneath litter bags at end of experiment',
#          x = '',
#          y = 'd15N') +
#     # scale_x_discrete(labels=c('Grasshopper', 'Mantid', 'Spider', 'Control')) + 
#     theme_flat

###### ...Figure for d15N=====

# rough look 
# plot(d15n ~ treatment, isotopes_e_w)

isotopes_e_w %>% 
    filter(sample_dir == 'W') %>%
    # mutate(treatment = fct_relevel(treatment, c('control', 'ghop', 'mantid', 'spider'))) %>% 
    group_by(treatment) %>% 
    summarize(mean_d15n = mean(d15n),
              se_d15n = se(d15n)) %>% 
    # View()
    ggplot(aes(x = treatment, y = mean_d15n, fill = treatment))+
    geom_col(position = 'dodge',
             color = 'black')+
    geom_errorbar(aes( ymin = mean_d15n - se_d15n, 
                       ymax = mean_d15n + se_d15n,
                       width = 0.15)) +
    # geom_hline(yintercept = 20664.72) + ### line for the Control treatments
    scale_fill_viridis(name = 'Grashopper fate', discrete = TRUE) +
    labs(title = 'd15N of soil beneath litter bags, at end of experiment',
         x = 'Treatment',
         y = 'd15N') +
    scale_x_discrete(labels=c('Grasshopper', 'Mantid', 'Spider', 'Control')) +
    theme_yaya() + 
    theme(legend.position = 'none',
          plot.title = element_blank())

my_ggsave(here::here('results/soil_d15n.png'))

## Data analysis: d15N by treatment ----
# switch factors back into an order that compares treatments against control
isotopes_e_w$treatment <- fct_relevel(isotopes_e_w$treatment, c('control', 'ghop', 'mantid', 'spider')) 

# isotopes_e_w <- mutate(isotopes_e_w,
#                        d15n_2 = scale(d15n, center = TRUE, scale = TRUE))

lmer_soil_d15n <- lmerTest::lmer(d15n ~ treatment + (1|block),  #filter()
                       isotopes_e_w) # , sample_dir == 'W'))

lm_soil_d15n <- lme4::lmer(d15n ~ treatment + (1|block), isotopes_e_w)
lm_soil_d15n_null <- lme4::lmer(d15n ~ (1|block), isotopes_e_w)

## Nevo's method of determining whether a variable matters and the post hoc
anova(lm_soil_d15n, lm_soil_d15n_null)
emmeans(lm_soil_d15n, pairwise ~ treatment)

summary(lmer_soil_d15n)
emmeans(lmer_soil_d15n, pairwise ~ treatment) #, lmer.df="satterthwaite") #method 1

par(mfrow = c(2,2))
plot(lmer_soil_d15n)


## spider is significant!  when compared to control 


###### ...Figure for total percent N====
isotopes_e_w %>% 
    filter(sample_dir == 'W') %>% 
    mutate(treatment = fct_relevel(treatment, c('ghop', 'mantid', 'spider','control'))) %>% 
    group_by(treatment) %>% 
    summarize(mean_total_percent_n = mean(total_percent_n),
              se_total_percent_n = se(total_percent_n)) %>% 
    ggplot(aes(x = treatment, y = mean_total_percent_n, fill = treatment))+
    geom_col(position = 'dodge',
             color = 'black')+
    geom_errorbar(aes( ymin = mean_total_percent_n - se_total_percent_n, 
                       ymax = mean_total_percent_n + se_total_percent_n,
                       width = 0.15)) +
    # geom_hline(yintercept = 20664.72) + ### line for the Control treatments
    scale_fill_viridis(name = 'Grashopper fate', discrete = TRUE) +
    labs(title = 'Total percent N of soil beneath litter bags, end of experiment',
         x = 'Treatment',
         y = 'Total % N') +
    scale_x_discrete(labels=c('Grasshopper', 'Mantid', 'Spider', 'Control')) +
    theme_yaya() + 
    theme(legend.position = 'none',
          plot.title = element_blank())

my_ggsave(here::here('results/soil_total_percent_n.png'))


## Data analysis: total percent N by treatment ----
# switch factors back into an order that compares treatments against control
isotopes_e_w$treatment <- fct_relevel(isotopes_e_w$treatment, c('control', 'ghop', 'mantid', 'spider')) 

lmer_soil_total_percent_n <- lmerTest::lmer(total_percent_n ~ treatment + (1|block), 
                                  filter(isotopes_e_w, sample_dir == 'W'))
summary(lmer_soil_total_percent_n)
emmeans(lmer_soil_total_percent_n, pairwise ~ treatment) #, lmer.df="satterthwaite") #method 1

ggqqplot(resid(lmer_soil_total_percent_n))


###### ...Figure for d13C====
isotopes_e_w %>% 
    filter(sample_dir == 'W') %>% 
    mutate(treatment = fct_relevel(treatment, c('ghop', 'mantid', 'spider','control'))) %>% 
    group_by(treatment) %>% 
    summarize(mean_d13c = mean(d13c),
              se_d13c = se(d13c)) %>% 
    ggplot(aes(x = treatment, y = mean_d13c, fill = treatment))+
    geom_col(position = 'dodge',
             color = 'black')+
    geom_errorbar(aes( ymin = mean_d13c - se_d13c, 
                       ymax = mean_d13c + se_d13c,
                       width = 0.15)) +
    # geom_hline(yintercept = 20664.72) + ### line for the Control treatments
    scale_fill_viridis(name = '', discrete = TRUE) +
    labs(title = 'd13C of soil beneath litter bags, at end of experiment',
         x = 'Treatment',
         y = 'd13C') +
    scale_x_discrete(labels=c('Grasshopper', 'Mantid', 'Spider', 'Control')) + 
    theme_yaya() + 
    theme(legend.position = 'none',
          plot.title = element_blank())

my_ggsave(here::here('results/soil_d13c.png'))


## Data analysis: d13c by treatment ----
# switch factors back into an order that compares treatments against control
isotopes_e_w$treatment <- fct_relevel(isotopes_e_w$treatment, c('control', 'ghop', 'mantid', 'spider')) 

lmer_soil_d13c <- lmer(d13c ~ treatment + (1|block), 
                       filter(isotopes_e_w, sample_dir == 'W'))
summary(lmer_soil_d13c)
emmeans(lmer_soil_d13c, pairwise ~ treatment)

ggqqplot(resid(lmer_soil_d13c))

###### ...Figure for total percent C====
isotopes_e_w %>% 
    filter(sample_dir == 'W') %>% 
    mutate(treatment = fct_relevel(treatment, c('ghop', 'mantid', 'spider','control'))) %>% 
    group_by(treatment) %>% 
    summarize(mean_total_percent_c = mean(total_percent_c),
              se_total_percent_c = se(total_percent_c)) %>% 
    ggplot(aes(x = treatment, y = mean_total_percent_c, fill = treatment))+
    geom_col(position = 'dodge',
             color = 'black')+
    geom_errorbar(aes( ymin = mean_total_percent_c - se_total_percent_c, 
                       ymax = mean_total_percent_c + se_total_percent_c,
                       width = 0.15)) +
    # geom_hline(yintercept = 20664.72) + ### line for the Control treatments
    scale_fill_viridis(name = '', discrete = TRUE) +
    labs(title = 'Total percent C of soil beneath litter bags, at end of experiment',
         x = 'Treatment',
         y = 'Total %C') +
    scale_x_discrete(labels=c('Grasshopper', 'Mantid', 'Spider', 'Control')) + 
    theme_yaya() + 
    theme(legend.position = 'none',
          plot.title = element_blank())

my_ggsave(here::here('results/soil_total_percent_c.png'))


## Data analysis: total_percent_c by treatment ----
# switch factors back into an order that compares treatments against control
isotopes_e_w$treatment <- fct_relevel(isotopes_e_w$treatment, c('control', 'ghop', 'mantid', 'spider')) 

lmer_soil_total_percent_c <- lmer(total_percent_c ~ treatment + (1|block), 
                                  filter(isotopes_e_w, sample_dir == 'W'))
summary(lmer_soil_total_percent_c)
emmeans(lmer_soil_total_percent_c, pairwise ~ treatment)

ggqqplot(resid(lm_soil_total_percent_c))


######## Box & whisker for C:N ratio----
# box + whisker plus dotplot for C:N ratio
## CHECK THE FACTORING FOR THIS ****

isotopes_e_w_cn <- isotopes_e_w %>% 
    mutate(cn_ratio = total_percent_c / total_percent_n,
           treatment = fct_relevel(treatment, c('ghop', 'mantid', 'spider','control')))

isoptopes_e_w_cn %>% 
    group_by(treatment) %>% 
    ggplot(aes(x = treatment, y = cn_ratio, fill = treatment))+
    geom_boxplot() +
    geom_jitter(width = 0.1)+
    scale_fill_viridis(name = '', discrete = TRUE) +
    labs(title = 'C:N ratio of soil beneath litter bags in E and W directions',
         x = '',
         y = 'C:N ratio') +
    scale_x_discrete(labels=c('Grasshopper', 'Mantid', 'Spider', 'Control')) + 
    theme_yaya() + 
    theme(legend.position = 'none',
          plot.title = element_blank())

my_ggsave(here::here('results/soil_c_to_n_boxplot.png'))


##### ...Figure for C:N, bar chart
isotopes_e_w_cn %>% 
    group_by(treatment) %>% 
    summarize(mean_cn_ratio = mean(cn_ratio),
              se_cn_ratio = se(cn_ratio)) %>% 
    ggplot(aes(x = treatment, y = mean_cn_ratio, fill = treatment))+
    geom_col(position = 'dodge',
             color = 'black')+
    geom_errorbar(aes( ymin = mean_cn_ratio - se_cn_ratio, 
                       ymax = mean_cn_ratio + se_cn_ratio,
                       width = 0.15)) +
    # geom_hline(yintercept = 20664.72) + ### line for the Control treatments
    scale_fill_viridis(name = 'Grashopper fate', discrete = TRUE) +
    labs(title = 'C:N ratio of soil beneath litter bags',
         x = 'Treatment',
         y = 'C:N') +
    scale_x_discrete(labels=c('Grasshopper', 'Mantid', 'Spider', 'Control')) + 
    theme_yaya() + 
    theme(legend.position = 'none',
          plot.title = element_blank())

my_ggsave(here::here('results/soil_c_to_n_bar_chart.png'))



## Data analysis: cn_ratio by treatment ----
# switch factors back into an order that compares treatments against control
isotopes_e_w_cn$treatment <- fct_relevel(isotopes_e_w_cn$treatment, c('control', 'ghop', 'mantid', 'spider')) 

lm_soil_cn_ratio <- lm(cn_ratio ~ treatment + (1 + block), 
                       isotopes_e_w_cn)
summary(lm_soil_cn_ratio) #no significance
par(mfrow = c(2,2))
plot(lm_soil_cn_ratio)



###............................................####
#### PLOT one variable at a time (change via name) -----
var_of_interest <- 'd15n'
isotopes_under_long %>% 
    group_by(treatment) %>% 
    filter(analysis == var_of_interest) %>%
    summarize(iso_mean = mean(a_value),
              iso_se = se(a_value)) %>% 
    ggplot(aes(x = var_of_interest, y = iso_mean, fill = treatment))+
    geom_bar(position = position_dodge(0.9), stat = 'identity')+
    # geom_point() + 
    geom_errorbar(aes(ymin=iso_mean-iso_se, ymax= iso_mean+iso_se), width=.2,
                  position=position_dodge(.9)) +
    expand_limits(y=0) +
    scale_fill_viridis(name = 'Treatment', discrete = TRUE) +
    labs(title = var_of_interest,
         x = 'Treatment',
         y = var_of_interest) +
    theme_pp

# summarize data
iso_summ <- isotopes_e_w %>%
    group_by(treatment) %>%
    summarize(mean_d15n = mean(d15n),
              se_d15n = se(d15n),
              mean_d13c = mean(d13c),
              se_d13c = se(d13c),
              mean_percent_n = mean(total_percent_n),
              se_percent_n = se(total_percent_n),
              mean_percent_c = mean(total_percent_c),
              se_percent_c = se(total_percent_c),
              n_size = n())


################## old summary stuff, probably delete=====
# iso_summ <- isotopes_under %>% 
#     group_by(treatment) %>% 
#     summarize(d15n = mean(d15n),
#               se_d15n = se(d15n),
#               d13c = mean(d13c),
#               se_d13c = se(d13c),
#               percent_n = mean(total_percent_n),
#               se_percent_n = se(total_percent_n),
#               percent_c = mean(total_percent_c),
#               se_percent_c = se(total_percent_c),
#               n_size = n())
# 
# ## dotplot and barchart of both "raw" core data and means
# ggplot(isotopes_under, aes(treatment, d15n, color = treatment)) +
#     geom_bar(stat = "identity", data = iso_summ, fill = NA) +
#     geom_jitter(position = position_jitter(0.2)) + 
#     geom_errorbar(
#         aes(ymin = d15n - se_d15n, ymax = d15n + se_d15n),
#         data = iso_summ, width = 0.2) 
################# end old summary stuff -----


#### scatterplot d15N and d13C with two error bars ####
# # should be using 95% conf int
# alpha = 0.05
# degrees.freedom = sample.n - 1
# t.score = qt(p=alpha/2, df=degrees.freedom,lower.tail=F)
# margin.error <- t.score * sample.se

iso_1 <- ggplot(iso_summ, aes(x = mean_d13c, y = mean_d15n, color = treatment))+
    geom_point(size = 2)+
    geom_errorbar(width = 0.04, aes(ymin = mean_d15n - se_d15n, ymax = mean_d15n + se_d15n))+
    geom_errorbarh(height = 0.5, aes(xmin = mean_d13c - se_d13c, xmax = mean_d13c + se_d13c))+
    scale_fill_viridis(name = 'Treatment', discrete = TRUE) +
    labs(title = "Trophic position plot (?)")

iso_1

# iso_under %>% 
#     pivot_wider(id_cols = c('mean_d13c', 'mean_d15n', ))
#     ggplot(aes(x = mean_d13c, y = mean_d15n, color = treatment))+
#     # geom_point(size = 2)+
#     geom_boxplot() +
#     geom_dotplot(binaxis = "y", stackdir = "center") +
#     geom_errorbar(width = 0.04, aes(ymin = mean_d15n - se_d15n, ymax = mean_d15n + se_d15n))+
#     geom_errorbarh(height = 0.5, aes(xmin = mean_d13c - se_d13c, xmax = mean_d13c + se_d13c))+
#     labs(title = "Trophic position plot (?)")



bp1 <-  ggplot(iso_summ, aes(x = treatment, y = mean_d15n, fill = treatment))+
    # geom_bar(stat = 'identity', width=0.5) +
    geom_boxplot() +
    geom_dotplot(binaxis = "y", stackdir = "center") +
    geom_errorbar(aes(ymin = mean_d15n - se_d15n, ymax = mean_d15n + se_d15n, width=0.1)) + 
    labs(title = "d15N in soil under labeled litterbags")

bp2 <-  ggplot(iso_summ, aes(x = treatment, y = mean_d13c, fill = treatment))+
    geom_bar(stat = 'identity', width=0.5) +
    # geom_boxplot() +
    geom_dotplot(binaxis = "y", stackdir = "center") +
    geom_errorbar(aes(ymin = mean_d13c - se_d13c, ymax = mean_d13c + se_d13c, width=0.1))+ 
    labs(title = "d13C in soil under labeled litterbags")

bp3 <- ggplot(iso_summ, aes(x = treatment, y = mean_percent_n, fill = treatment)) +
    # geom_bar(stat = 'identity', width=0.5) +
    labs(title = "total percent N in soil under labeled litterbags") + 
    geom_errorbar(aes(ymin = mean_percent_n - se_percent_n, ymax = mean_percent_n + se_percent_n, width=0.1))

bp4 <- ggplot(iso_summ, aes(x = treatment, y = mean_percent_c, fill = 
                                treatment))+
    geom_bar(stat = 'identity', width=0.5) +
    labs(title = "total percent C in soil under labeled litterbags") + 
    geom_errorbar(aes(ymin = mean_percent_c - se_percent_c, ymax = mean_percent_c + se_percent_c, width=0.1))

grid.arrange(bp1, bp2, iso_1, bp3, bp4, ncol = 3)


# ANOVA?
# iso_aov_15n <- aov(d13c ~ treatment, data = filter(isotopes_under, treatment != 'control'))
library(lme4)
data_trim <- select(isotopes_e_w, block, treatment, d13c)
model_d13c<- lm(d13c ~ treatment + (1|block), data = data_trim)
par(mfrow = c(2, 2))
plot(model_d13c)

summary(model_d13c)

data_trim <- select(isotopes_e_w, block, treatment, d15n)
model_d15n<- lm(d15n ~ treatment + (1|block), data = data_trim)
summary(model_d15n)
par(mfrow = c(2, 2))
plot(model_d15n)



lm_15n_null <-  lmer(d15n ~ block, data = isotopes_e_w)


iso_aov_15n <- aov(d13c ~ treatment, data = filter(isotopes_e_w, treatment == c('spider', 'mantid')))
# iso_aov_15n <- aov(d15n ~ treatment, data = isotopes_under)
summary(iso_aov_15n)
TukeyHSD(iso_aov_15n)

glm_15n <- lm()


#means centering parameterizing
isotopes_under.c <- isotopes_e_w %>% 
    mutate(d15n.c = d15n - mean(d15n))
iso_aov.c <- aov(d15n.c ~ treatment -1, data = isotopes_under.c)
summary(iso_aov.c)

