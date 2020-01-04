library(tidyverse)

set.seed(20191016)

bags <- c('n','s','e', 'w')
samp_seq <- sample(4)

df <- tibble(bags, samp_seq) %>% 
    arrange(samp_seq)
df
