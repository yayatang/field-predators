library(readr)
library(dplyr)
library(janitor)

file_names <- c(here::here('data/isotopes_final_plants_plate2_13c.csv'),
                here::here('data/isotopes_final_plants_plate2_15n.csv'))


# import data + clean up columns
plates <- file_names %>% 
    map_df(~read_csv(., skip=2)) %>% 
    clean_names() %>% 
    remove_empty(c('rows', 'cols')) %>% 
    mutate(cage = substr(sample_id, 1,2),
           fxl_group = substr(sample_id, 3, length(sample_id))) %>%
    rename(d13c = d13cvpdb,
           d15n = d15nair) %>% 
    select(-c(sample_wt_mg, ampl_28_m_v, ampl_44_m_v)) %>% 
    filter(sample_id != 'spinach',
           sample_id != '1577C-B',
           cage != 'PP',
           cage != 'Ll') %>% 
    select(sample_id, cage, everything())

#####
# to fix the cage names and turn into numeric
plates[,2:7] <- sapply(plates[,2:7], as.numeric)

plates_a <- plates %>% 
    select(cage, sample_id, total_percent_c, d13c, atom_percent_13c) %>% 
    filter(complete.cases(.))

plates_b <- plates %>% 
    select(cage, sample_id, total_percent_n, d15n, atom_percent_15n) %>% 
    filter(complete.cases(.))

plates_all <- full_join(plates_a, plates_b)


#####
# merge with cage treatment data

cage_treatments <- read_csv(here::here('data/0_cage_allocation.csv')) %>% 
    clean_names() %>% 
    select(-c('predator_id', 'position', 'replicate'))

# merge isotope data with cage meta-data
isotopes_plants_raw <- plates_all %>% 
    left_join(cage_treatments) %>% 
    select(sample_id, cage, treatment, everything())
