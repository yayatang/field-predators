library(plotly)
library(lme4)
library(emmeans)
source(here::here('src/g6.1_litterbag_import_v2.R'))

# remove this once the double entry is resolved
bag_data_all <- filter(bag_data_all, bag_sampID != '29E')

#--- Pre-check: all samplings, litterbag dry masses ----
bag_data_all$treatment <- fct_relevel(bag_data_all$treatment, c('ghop', 'mantid', 'spider', 'control'))
bag_data_all$dir <- fct_relevel(bag_data_all$dir, c('N', 'E', 'W', 'S'))


## look at each litterbag data, across all samplings
bag_data_all %>% 
    ggplot(aes(x = samp_num,
               y = dry_diff_from_start, 
               color = treatment)) +
    geom_jitter(width = 0.2) + 
    scale_color_viridis(name = 'Treatment', labels = c('Control', 'Carcass', 'Mantid', 'Spider'), discrete = TRUE) +
    labs(title = "ALL litterbag dry masses",
         x = "Sampling number",
         y = "Dry mass loss from start (g)") +
    # scale_x_continuous(limits = c(-0.2, 3.2)) +
    scale_y_continuous(limits = c(-0.25, 0.07)) + 
    theme_meso

# BOXPLOT
bag_data_all %>% 
    # group_by(samp_num, treatment) %>%
    ggplot(aes(x = samp_num, y = dry_infer, fill = treatment)) +
    geom_boxplot() + 
    geom_jitter(position = position_jitterdodge()) +
    scale_color_viridis(name = 'Grasshopper fate', discrete = TRUE) +
    scale_fill_viridis(name = 'Grasshopper fate', discrete = TRUE) +
    labs(title = 'All samplings, all litterbags, inferred masses',
         x = 'Sampling',
         y = 'Dry mass (g)',
         color = 'Treatment') +
    theme_meso

# ?????
bag_data_all %>% 
    drop_na() %>%  # remove all inferred data
    # View()
    ggplot(aes(x = samp_num, y = dry_diff_from_start, group = treatment)) +
    geom_jitter(aes(color = treatment, shape = dir), width = 0.2) + 
    scale_color_viridis(name = 'Grasshopper fate', discrete = TRUE) +
    labs(title = 'Mass difference from experimental start',
         x = 'Sampling',
         y = 'Dry mass difference (g)',
         color = 'Treatment') +
    theme_meso

#----..Figure: litterbag mass difference ----
# dry mass difference from the start for sampling 3, summarized by treatment
bag_data_all %>% 
    drop_na() %>%  # remove all inferred data
    filter(samp_num == 2) %>% 
    group_by(treatment) %>%
    summarize(mean_dry_diff = mean(dry_diff_from_start),
              se_dry_diff = se(dry_diff_from_start)) %>%
    # View()
    ggplot(aes(x = treatment, y = mean_dry_diff,
               fill = treatment)) +
    geom_col(position = position_dodge(),
             color = 'black')+
    geom_errorbar(aes( ymin = mean_dry_diff - se_dry_diff, 
                       ymax = mean_dry_diff + se_dry_diff,
                       width = 0.2)) +
    scale_fill_viridis(name = 'Treatment', discrete = TRUE) +
    labs(title = 'Litterbag mass difference from experimental start, \nsampling 2 after rains',
         x = 'Treatment',
         y = 'Dry mass difference from start (g)',
         color = 'Treatment') +
    scale_x_discrete(labels=c('Grasshopper', 'Mantid', 'Spider', 'Control')) +
    theme_yaya() + 
    theme(legend.position = 'none',
          plot.title = element_blank())

my_ggsave(here::here('results/litterbag_decomp_samp2.png'))

    
    
    # theme(axis.text.x = element_text(vjust = 1, hjust =0.5),
    #       panel.background = element_rect(fill = "#f9f9f9",
    #                                       colour = "#f9f9f9"),
    #       panel.border = element_rect(color = "black", fill = NA),
    #       legend.position = "none")

#--- Data analysis: statistics for litterbags ----
bag_data_inferred <- bag_data_all %>% 
    drop_na() %>% 
    mutate(infer_start_dry_diff = dry_infer - start_m_lit_dry,
           infer_start_dry_diff2 = scale(infer_start_dry_diff, scale = TRUE, center = TRUE)) %>% 
    filter(bag_sampID != '2W' ) %>%
    filter(bag_sampID != '4W' ) ## positive, and outliers from QQ
bag_data_inferred$treatment <- fct_relevel(bag_data_inferred$treatment, c('control', 'ghop', 'mantid', 'spider'))

var <- bag_data_inferred %>% 
    ungroup() %>% 
    filter(samp_num == 2)

hist(var$infer_start_dry_diff)
hist(var$infer_start_dry_diff2)

# lm, looking at dry difference from start for all bags inferred mass, by block
## using CENTERED data
lmer_litterbag_infer2_samp2  <- lmer(infer_start_dry_diff2 ~ treatment + (1|block), 
                                filter(bag_data_inferred, samp_num == 2))
summary(lmer_litterbag_infer2_samp2)
emmeans(lmer_litterbag_infer2_samp2, pairwise ~ treatment) #, lmer.df="satterthwaite") #method 1


plot(lmer_litterbag_infer2_samp2)
# qqnorm(resid(lmer_litterbag_infer2_samp2))
# qqline(resid(lmer_litterbag_infer2_samp2))



## NULL Model (do I need this?)
lmer_litterbag_infer2_null  <- lmer(infer_start_dry_diff2 ~ (1|block), 
                                   filter(bag_data_inferred, samp_num == 2))
summary(lmer_litterbag_infer2_null)

qqnorm(resid(lmer_litterbag_infer2_null))
qqline(resid(lmer_litterbag_infer2_null))




## comparing the two models
anova(lmer_litterbag_infer2_null, lmer_litterbag_infer2_samp2)


bag_data_all_model <- bag_data_all
# #     mutate(infer_start_dry_diff = dry_infer - start_m_lit_dry) %>% 
#     filter(bag_sampID != '17E') %>% 
#     filter(bag_sampID != '39E') 
## removing the two outliers didn't change the qualitative conclusions of the model

bag_data_all_model$treatment <- fct_relevel(bag_data_all_model$treatment, c('control', 'ghop', 'mantid', 'spider'))

lm_litterbag_samp2 <- lm(dry_diff_from_start ~ treatment + (1|block), 
                         filter(bag_data_all_model, samp_num == 2))
summary(lm_litterbag_samp2)
par(mfrow = c(2,2))
plot(lm_litterbag_samp2)
alias(lm_litterbag_samp2) # look at contrast/dummy coding

## all are non significant


## this section is for inferred dried mass?
# graph all litter dry masses over samplings
# all_bags <- ggplot(filter(bag_data_all, dir == 'W'), # for only looking at one direction

### FIX
# bag_data_all %>% 
#     drop_na() %>% 
#     filter(samp_num == 3) %>% 
#     group_by(treatment, samp_num) %>% 
#     summarize(mean_dry_diff = mean(dry_diff_from_start),
#               se_trt_cumul_scale = se(cumul_c_daily_scaled),
#               ghop_fate = first(ghop_fate)) %>% 
#     # View()
#     ggplot(aes(x = samp_num,
#                y = dry_infer, 
#                group = treatment)) + 
#     
#     ggplot(aes(x = trt, y = mean_trt_cumul_scale, fill = ghop_fate)) +
#     geom_col(position = 'dodge',
#              color = 'black')+
#     geom_errorbar(aes( ymin = mean_trt_cumul_scale - se_trt_cumul_scale, 
#                        ymax = mean_trt_cumul_scale + se_trt_cumul_scale,
#                        width = 0.15)) +
#     scale_fill_manual(values = vir_4[1:3], name = 'Grasshopper fate') + 
#     scale_x_discrete(labels = bar_x_labels[1:7]) +
#     labs(title = graph_title,
#          x = 'Treatment',
#          y = expression(C~mineralization~(mg~g~dry~wt~inputs^-1)),
#          # y = 'C mineralization (mg g dry wt inputs ^-1)',
#          color = 'Treatment') +
#     theme_pp
# 
# geom_jitter(aes(color = treatment, shape = dir), width = 0.2) +
#     


#### Plot of only real dry litter mass loss (not inferred) ####

# # graph all litterbag dry masses that were measured
# indiv_real <- ggplot(bag_data_all,
#                      aes(x = samp_num, y = dry_diff_from_start, 
#                          group = block)) +
#     geom_jitter(aes(color = dir, shape = treatment), 
#                 width = 0.2) + 
#     labs(title = "ALL litterbag dry masses",
#          x = "sampling number",
#          y = "dry mass loss from start") +
#     theme_bw() +
#     scale_x_continuous(limits = c(-0.2, 3.2)) +
#     scale_y_continuous(limits = c(-0.25, 0.07))
# ggplotly(indiv_real)
# 
# ggsave(here::here('results/litterbag_by_samp_ALL.png'), 
#        width = 7, height = 5, dpi = 600)


#---..Figure: single direction real data (not more useful than above) ----
##### graphs for one direction ONLY ####

graph_dir <- 'S' ## this is from sampling 3
bag_data_all %>% 
    drop_na() %>% 
    filter(dir == graph_dir) %>%
    group_by(treatment, dir) %>%
    summarize(mean_dry_diff = mean(dry_diff_from_start),
              se_dry_diff = se(dry_diff_from_start)) %>%
    # View()
    ggplot(aes(x = treatment, y = mean_dry_diff,
               fill = treatment)) +
    geom_col(position = position_dodge(),
             color = 'black')+
    geom_errorbar(aes( ymin = mean_dry_diff - se_dry_diff, 
                       ymax = mean_dry_diff + se_dry_diff),
                  position = position_dodge(width=0.9), 
                  width = 0.3) +
    scale_fill_viridis(name = 'Treatment', discrete = TRUE) +
    labs(title = 'Litterbag mass difference from experimental start, SOUTH sampling 3',
         x = 'Treatment',
         y = 'Dry mass difference from start (g)',
         color = 'Treatment') +
    scale_x_discrete(labels=c('Grasshopper \ncarcass', 'Mantid', 'Spider', 'Control')) +
    theme(axis.text.x = element_text(vjust = 1, hjust =0.5),
          panel.background = element_rect(fill = "#f9f9f9",
                                          colour = "#f9f9f9"),
          panel.border = element_rect(color = "black", fill = NA),
          legend.position = "none")

















# 
# indiv_real_one <- ggplot(filter(bag_data_simple, dir ==graph_dir),
#                          aes(x = samp_num, y = dry_diff_from_start, 
#                              group = block)) +
#     geom_jitter(aes(color = treatment), 
#                 width = 0.2) +
#     labs(title = paste(graph_dir, "litterbag dry masses"),
#          x = "sampling number",
#          y = "dry mass loss from start") +
#     theme_bw() +
#     scale_x_continuous(limits = c(-0.2, 3.2)) +
#     scale_y_continuous(limits = c(-0.25, 0.07))
# 
# ggplotly(indiv_real_one)
# 
# ggsave(paste0(here::here('results/litterbag_by_samp_'), graph_dir, '.png'), 
#        width = 7, height = 5, dpi = 600)
# 
# ## graph for Samp 3
# bag_data_simple_3 <- bag_data_simple %>% 
#     filter(dir == "W" | dir == "S")
# indiv_real_3 <- ggplot(bag_data_simple_3,
#                        aes(x = samp_num, y = dry_diff_from_start, 
#                            group = block)) +
#     geom_jitter(aes(color = dir, shape = treatment), 
#                 width = 0.2) +
#     labs(title = paste("West and south litterbag dry masses"),
#          x = "sampling number",
#          y = "dry mass loss from start") +
#     theme_bw() +
#     scale_x_continuous(limits = c(-0.2, 3.2)) +
#     scale_y_continuous(limits = c(-0.25, 0.07))
# 
# ggplotly(indiv_real_3)
# 
# ggsave(here::here('results/litterbag_by_samp3.png'), 
#        width = 7, height = 5, dpi = 600)
# 
# ## graph for samp 3, box plot of dry mass loss in litterbags by treatment
# bag_data_simple_3.2 <- bag_data_simple %>% 
#     filter(dir == "E", 
#            samp_num == 2) %>% 
#     group_by(treatment)
# 
# bag_data_simple_3.2 %>% 
#     ggplot(aes(x = treatment, 
#                y = dry_diff_from_start, 
#                fill = treatment)) +
#     geom_boxplot() + 
#     geom_dotplot(binaxis = "y", stackdir = "center") +
#     labs(title = paste("West litterbag dry mass loss"),
#          x = "treatment",
#          y = "dry mass loss from start") +
#     theme_bw() +
#     scale_y_continuous(limits = c(-0.25, 0))
# 
# ggplotly(indiv_real_3.2)
# 
# ggsave(here::here('results/litterbag_by_samp3.2.png'), 
#        width = 7, height = 5, dpi = 600)
# 
# 
# #### t tests for bag samples ####
# # we see from the graph that in sampling 3, S + W may be different
# # run t-tests to compare directions and samplings
# 
# # t test comparing south from west @ samp 3
# t.test(filter(bag_data_all, dir == 'W' & samp_num == 3)$dry_lit, 
#        filter(bag_data_all, dir == 'S' & samp_num == 3)$dry_lit,
#        paired = TRUE)
# 
# # comparing S litterbags between samp 0 & 3
# t.test(filter(bag_data_all, dir == 'S' & samp_num == 0)$dry_lit, 
#        filter(bag_data_all, dir == 'S' & samp_num == 3)$dry_lit,
#        paired = TRUE)
# 
# # comparing W litterbags between samp 0 & 3
# t.test(filter(bag_data_all, dir == 'W' & samp_num == 0)$dry_lit, 
#        filter(bag_data_all, dir == 'W' & samp_num == 3)$dry_lit,
#        paired = TRUE)
# 
# # dry litter mass difference FROM START
# # comparing S litterbags between samp 0 & 3
# t.test(filter(bag_data_all, dir == 'S' & samp_num == 0)$dry_diff_from_start, 
#        filter(bag_data_all, dir == 'S' & samp_num == 3)$dry_diff_from_start,
#        paired = TRUE)
# 
# # ANOVAs to see if the mass loss depends on treatment
# sampW_aov <- aov(dry_lit ~ treatment, data = filter(bag_data_all, dir == 'W'))
# summary(sampW_aov)
# 
# 
# #ANOVA for the effect of litterbag dir on inferred dry mass @ T1
# samp1_aov <- aov(dry_infer ~ dir, data = filter(bag_data_all, samp_num == 1))
# summary(samp1_aov)
# # answer is no
# 
# # ANOVA for the effect of litterbag direction on inferred dry litter mass
# # samp2_aov <- aov(dry_lit ~ dir, data = filter(bag_data_all, samp_num == 3))
# samp2_aov <- aov(dry_lit ~ treatment, data = filter(bag_data_all, samp_num == 3))
# summary(samp2_aov)
# 
# # ANOVA for the effect of litterbag direction on true  dry litter mass
# # samp3_aov <- aov(dry_lit ~ dir, data = sw_compar)
# # summary(samp3_aov)
# # 
# # samp3_aov_trt <- aov(dry_lit ~ treatment, data = sw_compar)
# # summary(samp3_aov_trt)
# 
# 
# some_aov <- aov(dry_infer ~ dir + treatment, data = filter(bag_data_all, samp_num == 2))
# # sw_compar)
# summary(some_aov)
# 
# all_aov <- aov(dry_infer ~ treatment + samp_num, data = bag_data_all)
# summary(all_aov)
# 
# 
# # 
# # indiv_real_s3 <- ggplot( some_aov, #sw_compar,
# #                         aes(x = samp_num, y = dry_lit)) +
# #     geom_point(aes(color = dir), size = 2)
# # indiv_real_s3
# 
# 
# # inferred dry litter bag diffs over time
# inferred <- ggplot(bag_data_infer, aes(x=samp_num, y=mean.infer_trt))+
#     geom_line(aes(color=treatment)) +
#     geom_point(aes(color=treatment))+
#     geom_errorbar(aes(ymin = mean.infer_trt - se.infer_trt, 
#                       ymax = mean.infer_trt + se.infer_trt,
#                       color = treatment), width = 0.05)
# ggplotly(inferred) # though the inferred values shouldn't exist for samp 3...
# 
# # REAL dry litter bag diffs over time, but AVERAGED for each treatment/sampling
# mean_real <- ggplot(bag_data_real, aes(x=samp_num, y=mean.real_trt))+
#     geom_line(aes(color=treatment)) +
#     geom_point(aes(color=treatment))+
#     geom_errorbar(aes(ymin = mean.real_trt - se.real_trt, 
#                       ymax = mean.real_trt + se.real_trt,
#                       color = treatment), width = 0.05)
# ggplotly(mean_real)
# 
# # =====
# # boxplots by treatment for the third sampling
# samp3_by.bag <- ggboxplot(filter(bag_data_all, samp_num==3), 
#                           x = 'samp_num', 
#                           y = 'dry_lit',
#                           color = 'treatment', 
#                           order = c('control', 'ghop', 'mantid', 'spider'),
#                           title = 'litterbag dry masses', 
#                           xlab = 'sampling number',
#                           ylab = 'dry mass') +
#     theme(legend.position='none')
# samp3_by.bag
# 
# samp3_aov <- aov(dry_lit ~ treatment, data = filter(bag_data_all, samp_num==3))
# summary(samp3_aov)
# 
# 
# #====STATS FOR SAMPLING 1=====
# # =====summary stats=====
# # means by direction 
# final_by.dir <- group_by(bag_data_all, dir, samp_num)
# final_by.dir_summ <- summarise(final_by.dir, mean = mean(dry_lit), 
#                                se = se(dry_lit)) %>% 
#     arrange(samp_num, dir)
# final_by.dir_summ
# 
# 
# final_by.trt <- group_by(bag_data_all, treatment, dir, samp_num)
# final_by.trt_summ <- summarise(final_by.trt, 
#                                mean = mean(dry_lit), 
#                                se = se(dry_lit)) %>% 
#     na.omit()
# final_by.trt_summ
# 
# # =====ANOVAs=====
# # ANOVA between wet bags
# samp1_aov <- aov(samp1_wet_diff ~ treatment, data = bag_1)
# summary(samp1_aov)
# 
# # ANOVA between bag direction
# samp1_aov_dir <- aov(samp1_wet_diff ~ dir, data = bag_1)
# summary(samp1_aov_dir)
# 
# # ANOVA between dry bag treatments
# samp1_aov_dry <- aov(samp1_dry_diff ~ treatment, data = bag_north)
# summary(samp1_aov_dry)
# 
# samp1_ttest_start <- t.test(bag_north$samp1_dry_lit, bag_north$start_lit_dry, paired=TRUE)
# samp1_ttest_start

