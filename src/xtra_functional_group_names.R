library(tidyverse)

cage_nums <- c(1, 2, 4:25, 27:40)
plant_groups <- c('cer', 'C.a.', 'leg', 'ot')

all_names <- expand.grid(cage_nums, plant_groups) %>% 
    mutate(sample_id = paste0(Var1, Var2)) %>% 
    arrange(by = Var1) %>% 
    select(sample_id)

write_csv(all_names, here::here('data/plant_sample_ids.csv'))
