

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

