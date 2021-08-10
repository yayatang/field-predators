bromeliad <- tibble(treatment = as_factor(c('Spider feces', 'Fly carcass', 'Fly remains', 'Control')),
                    atom_15n = c(1.558, 1.151, 0.684, 0.370),
                    atom_15n_se = c(0.08, 0.17, 0.04, 0.00))

library(ggplot)

bromeliad %>% 
    ggplot(aes(x = treatment, y = atom_15n, fill = treatment))+
    geom_col(position = 'dodge',
             color = 'black')+
    geom_errorbar(aes( ymin = atom_15n - atom_15n_se, 
                       ymax = atom_15n + atom_15n_se,
                       width = 0.15)) +
    scale_fill_viridis(name = 'Grashopper fate', discrete = TRUE, option = 3) +
    labs(title = '',
         x = 'Treatment',
         y = 'Bromeliad leaf atom % 15N') +
    # scale_x_discrete(labels=c('Grasshopper', 'Mantid', 'Spider', 'Control')) +
    theme_yaya() + 
    theme(legend.position = 'none',
          plot.title = element_blank())
