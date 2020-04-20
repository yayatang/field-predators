library(plotly)

# graph all litter dry masses over samplings
all_bags <- ggplot(filter(bag_data_all, dir == 'W'),
                   aes(x = samp_num, y = dry_infer, group = treatment)) +
    # geom_line(aes(color = bag_sampID )) +
    geom_jitter(aes(color = treatment), width = 0.2)
ggplotly(all_bags)

indiv_real <- ggplot(bag_data_all,
                     aes(x = samp_num, y = dry_lit, group = treatment)) +
    # geom_jitter(aes(color = dir))
    geom_jitter(aes(color = treatment), width = 0.2)
ggplotly(indiv_real)

sw_compar <- bag_data_all %>%
    filter(samp_num==3,
           dir == 'S' | dir == 'W')
# s_compar <- bag_data_all[which(bag_data_all$samp_num == 3 & bag_data_all$dir == 'S'),]
# w_compar <- bag_data_all[which(bag_data_all$samp_num == 3 & bag_data_all$dir == 'W'),]
s_compar <- filter(bag_data_all, samp_num == 3 & dir == 'S')
w_compar <- filter(bag_data_all, samp_num == 3 & dir == 'W')

t.test(filter(bag_data_all, dir == 'S' & samp_num == 0)$dry_lit, 
       filter(bag_data_all, dir == 'S' & samp_num == 3)$dry_lit)
    

# t test comparing south from west
t.test(s_compar$dry_lit, w_compar$dry_lit, paired = TRUE)

# difference in dry litter mass
t.test(s_compar$dry_diff_from_start, w_compar$dry_diff_from_start, paired = TRUE)

sampW_aov <- aov(dry_diff_from_start ~ treatment, data = filter(bag_data_all, dir == 'W'))
summary(sampW_aov)



#ANOVA for the effect of litterbag dir on inferred dry mass @ T1
samp1_aov <- aov(dry_infer ~ dir, data = filter(bag_data_all, samp_num == 1))
summary(samp1_aov)


# ANOVA for the effect of litterbag direction on inferred dry litter mass
samp2_aov <- aov(dry_infer ~ dir, data = filter(bag_data_all, samp_num == 2))
summary(samp2_aov)

# ANOVA for the effect of litterbag direction on true  dry litter mass
samp3_aov <- aov(dry_lit ~ dir, data = sw_compar)
summary(samp3_aov)

samp3_aov_trt <- aov(dry_lit ~ treatment, data = sw_compar)
summary(samp3_aov_trt)


some_aov <- aov(dry_infer ~ dir + treatment, data = filter(bag_data_all, samp_num == 2))
# sw_compar)
summary(some_aov)

all_aov <- aov(dry_infer ~ treatment + samp_num, data = bag_data_all)
summary(all_aov)
















indiv_real_s3 <- ggplot(sw_compar,
                        aes(x = samp_num, y = dry_lit)) +
    geom_point(aes(color = dir), size = 2)
indiv_real_s3


# inferred dry litter bag diffs over time
inferred <- ggplot(bag_data_infer, aes(x=samp_num, y=mean.infer_trt))+
    geom_line(aes(color=treatment)) +
    geom_point(aes(color=treatment))+
    geom_errorbar(aes(ymin = mean.infer_trt - se.infer_trt, 
                      ymax = mean.infer_trt + se.infer_trt,
                      color = treatment), width = 0.05)
ggplotly(inferred) # though the inferred values shouldn't exist for samp 3...

# REAL dry litter bag diffs over time, but AVERAGED for each treatment/sampling
mean_real <- ggplot(bag_data_real, aes(x=samp_num, y=mean.true_trt))+
    geom_line(aes(color=treatment)) +
    geom_point(aes(color=treatment))+
    geom_errorbar(aes(ymin = mean.true_trt - se.true_trt, 
                      ymax = mean.true_trt + se.true_trt,
                      color = treatment), width = 0.05)
ggplotly(mean_real)

# =====
# boxplots by treatment for the third sampling
samp3_by.bag <- ggboxplot(filter(bag_data_all, samp_num==3), 
                          x = 'samp_num', 
                          y = 'dry_lit',
                          color = 'treatment', 
                          order = c('control', 'ghop', 'mantid', 'spider'),
                          title = 'litterbag dry masses', 
                          xlab = 'sampling number',
                          ylab = 'dry mass') +
    theme(legend.position='none')
samp3_by.bag

samp3_aov <- aov(dry_lit ~ treatment, data = filter(bag_data_all, samp_num==3))
summary(samp3_aov)


#====STATS FOR SAMPLING 1=====
# =====summary stats=====
# means by direction 
final_by.dir <- group_by(bag_data_all, dir, samp_num)
final_by.dir_summ <- summarise(final_by.dir, mean = mean(dry_lit), se = se(dry_lit)) %>% 
    arrange(samp_num, dir)
final_by.dir_summ


final_by.trt <- group_by(bag_data_all, treatment, dir, samp_num)
final_by.trt_summ <- summarise(final_by.trt, 
                               mean = mean(dry_lit), 
                               se = se(dry_lit)) %>% 
    na.omit()
final_by.trt_summ

# =====ANOVAs=====
# ANOVA between wet bags
samp1_aov <- aov(samp1_wet_diff ~ treatment, data = bag_1)
summary(samp1_aov)

# ANOVA between bag direction
samp1_aov_dir <- aov(samp1_wet_diff ~ dir, data = bag_1)
summary(samp1_aov_dir)

# ANOVA between dry bag treatments
samp1_aov_dry <- aov(samp1_dry_diff ~ treatment, data = bag_north)
summary(samp1_aov_dry)

samp1_ttest_start <- t.test(bag_north$samp1_dry_lit, bag_north$start_lit_dry, paired=TRUE)
samp1_ttest_start




# =====================================================
# ================== code below not functioning
# boxplots for wet masses of all treatments and directions
ggboxplot(bag_1, x = 'treatment', y = 'samp1_wet_diff',
          color = 'treatment', 
          order = c('control', 'ghop', 'mantid', 'spider'),
          title = '1:treatments litter bag WET mass difference from start', 
          xlab = 'treatment',
          ylab = 'wet mass diff') +
    theme(legend.position='none')

ggboxplot(bag_1, x = 'dir', y = 'samp1_wet_diff',
          color = 'dir', 
          order = c('N', 'E', 'S', 'W'),
          title = '1:direction litter bag WET mass difference from start', 
          xlab = 'direction',
          ylab = 'wet mass diff')+
    theme(legend.position='none')

# by inferred dry mass, by treatment
ggboxplot(bag_1, x = 'treatment', y = 'samp1_dry_infer',
          color = 'treatment',
          order = c('control', 'ghop', 'mantid', 'spider'),
          title = '1: treatments litter bag DRY mass',
          xlab = 'treatment',
          ylab = 'dry mass') +
    theme(legend.position='none')

# by inferred dry mass, by direction
ggboxplot(bag_1, x = 'dir', y = 'samp1_dry_infer',
          color = 'dir', 
          order = c('N', 'E', 'S', 'W'),
          title = '1: direction litter bag DRY mass', 
          xlab = 'direction',
          ylab = 'dry mass')+
    theme(legend.position='none')

ggbarplot(lit_moist, x = 'sampling', y = 'moist_mean',
          color = 'sampling', 
          title = '1:average %moisture of litterbags',
          xlab = 'sampling',
          ylab = '%moisture',
)

# boxplots for DRY masses of all treatments in the NORTH direction
bag_north <- filter(bag_1, dir == 'N')
ggboxplot(bag_north, x = 'treatment', y = 'samp1_dry_diff',
          color = 'treatment', 
          order = c('control', 'ghop', 'mantid', 'spider'),
          title = '1:Dry litter bags by treatments, difference from start', 
          xlab = 'Treatment',
          ylab = 'Dry mass difference')+
    theme(legend.position='none')


#======================
####
# SAME AS ABOVE
# copied code for sampling 2



# boxplots for wet masses of all treatments and directions
# ggboxplot(bag_2, x = 'treatment', y = 'samp2_wet_diff',
#           color = 'treatment', 
#           order = c('control', 'ghop', 'mantid', 'spider'),
#           title = '2: treatments litter bag WET mass difference from start', 
#           xlab = 'treatment',
#           ylab = 'wet mass diff') +
#     theme(legend.position='none')
# 
# ggboxplot(bag_2, x = 'dir', y = 'samp2_wet_diff',
#           color = 'dir', 
#           order = c('N', 'E', 'S', 'W'),
#           title = '2: direction litter bag WET mass difference from start', 
#           xlab = 'direction',
#           ylab = 'wet mass diff')+
#     theme(legend.position='none')

ggboxplot(bag_2, x = 'treatment', y = 'samp2_dry_infer',
          color = 'treatment',
          order = c('control', 'ghop', 'mantid', 'spider'),
          title = '2: treatments litter bag DRY mass',
          xlab = 'treatment',
          ylab = 'dry mass') +
    theme(legend.position='none')

ggboxplot(bag_2, x = 'dir', y = 'samp2_dry_infer',
          color = 'dir', 
          order = c('N', 'E', 'S', 'W'),
          title = '2: direction litter bag DRY mass', 
          xlab = 'direction',
          ylab = 'dry mass')+
    theme(legend.position='none')

ggbarplot(lit_moist, x = 'sampling', y = 'moist_mean',
          color = 'sampling', 
          title = '2: average %moisture of litterbags',
          xlab = 'sampling',
          ylab = '%moisture',
)


# boxplots for DRY masses of all treatments in the EAST direction
bag_east <- filter(bag_2, dir == 'E')
ggboxplot(bag_east, x = 'treatment', y = 'samp2_dry_diff',
          color = 'treatment', 
          order = c('control', 'ghop', 'mantid', 'spider'),
          title = '2: Dry litter bags by treatments, difference from start', 
          xlab = 'Treatment',
          ylab = 'Dry mass difference')+
    theme(legend.position='none')


#================

# litterbag mass over time

