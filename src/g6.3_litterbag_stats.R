## not sure a separate stats script is prudent/necessary



#====STATS FOR SAMPLING 1=====
# =====summary stats=====
# wet litterbag means by treatment
samp1_summ <- group_by(bag_1, treatment)
summarise(samp1_summ, mean = mean(samp1_wet_diff), se = se(samp1_wet_diff))

# means by direction 
samp1_dir <- group_by(bag_1, dir)
summarise(samp1_dir, mean = mean(samp1_wet_diff), se = se(samp1_wet_diff))

samp1_dry <- group_by(bag_north, treatment)
summarise(samp1_dry, mean = mean(samp1_dry_diff), se = se(samp1_dry_diff))

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

# #===assumptions tests===
# # plot(samp1_aov) #check plots for normally distributed values
# # plot(samp1_aov_dir) #check plots for normally distributed values
# plot(samp1_aov_dry)
# 
# # levene's test for homogeneity of variances
# library(car)
# samp1_levenes <- leveneTest(samp1_dry_diff ~ treatment, data = bag_north)
# samp1_levenes



# =====summary stats=====
# wet litterbag means by treatment
samp2_summ <- group_by(bag_2, treatment)
summarise(samp2_summ, mean = mean(samp2_wet_diff), se = se(samp2_wet_diff))

# means by direction 
samp2_dir <- group_by(bag_2, dir)
summarise(samp2_dir, mean = mean(samp2_wet_diff), se = se(samp2_wet_diff))

samp2_dry <- group_by(bag_east, treatment)
summarise(samp2_dry, mean = mean(samp2_dry_diff), se = se(samp2_dry_diff))

# =====ANOVAs=====
# ANOVA between wet bags
samp2_aov <- aov(samp2_wet_diff ~ treatment, data = bag_2)
summary(samp2_aov)

# ANOVA between bag direction
samp2_aov_dir <- aov(samp2_wet_diff ~ dir, data = bag_2)
summary(samp2_aov_dir)

# ANOVA between dry bag treatments
samp2_aov_dry <- aov(samp2_dry_diff ~ treatment, data = bag_east)
summary(samp2_aov_dry)

samp2_ttest_start <- t.test(bag_east$samp2_dry_lit, bag_east$start_lit_dry, paired=TRUE)
samp2_ttest_start

# #===assumptions tests===
# # plot(samp2_aov) #check plots for normally distributed values
# # plot(samp2_aov_dir) #check plots for normally distributed values
# plot(samp2_aov_dry)
# 
# # levene's test for homogeneity of variances
# library(car)
# samp2_levenes <- leveneTest(samp2_dry_diff ~ treatment, data = bag_east)
# samp2_levenes