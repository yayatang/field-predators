cage <- c(1:40)
cage
cage <- cage[cage != 3]
cage <- cage[cage != 26]
cage

# dir <- list('N', 'E', c('S', 'W'))
# dir

# U = under litterbag
# B = bare soil
samp_loc <- as_factor(c('U', 'B'))

rep <- c(1, 2)

library(tidyverse)
sample_1 <- expand_grid(cage, dir=as_factor('N'), samp_loc, rep) %>% 
  arrange(cage, samp_loc)
  
sample_2 <- expand_grid(cage, dir=as_factor('E'), samp_loc, rep) %>% 
  arrange(cage, samp_loc)

sample_3a <- expand_grid(cage, dir=as_factor('W'), samp_loc, rep)
sample_3b <- expand_grid(cage, dir=as_factor('S'), samp_loc, rep)

sample_fin <- bind_rows(sample_3a, sample_3b) %>% 
  arrange(cage, dir, samp_loc)


sample_all <- bind_rows(sample_1, sample_2, sample_fin) %>% 
  unite(sample_ID, sep="")


write_csv(sample_all, 'C:/Users/yaya/Dropbox/1 Ecologist/2 EXPERIMENTS/6 poopy plants/data/soil_samples.csv')
