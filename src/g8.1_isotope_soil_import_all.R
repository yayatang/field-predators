# soil isotopic data
library(tidyverse)
# library(here)
library(janitor)
library(gridExtra) # grid.arrange
library(GGally) #for ggpairs
source(here::here('src/yaya_fxns.R'))

##########################
# import all litterbag soil core data
file_names <- c(here::here('data/isotopes_litterbag_soil_plate1a_15n.csv'),
                here::here('data/isotopes_litterbag_soil_plate1a_13c.csv'),
                here::here('data/isotopes_litterbag_soil_plate1b_15n.csv'),
                here::here('data/isotopes_litterbag_soil_plate1b_13c.csv'),
                here::here('data/isotopes_litterbag_soil_plate2_15n.csv'),
                here::here('data/isotopes_litterbag_soil_plate2_13c.csv'))

# import data + clean up columns
plates <- file_names %>% 
    map_df(~read_csv(., skip=2)) %>% 
    clean_names() %>% 
    mutate(cage = substr(sample_id, 1,2)) %>% 
    rename(d13c = d13cvpdb,
           d15n = d15nair) %>% 
    select(-c(sample_wt_mg, x7, x8, x9, ampl_28_m_v, ampl_44_m_v))

# clean up rows
plates <- plates[rowSums(is.na(plates)) != ncol(plates), ] %>% 
    filter(sample_id != 'spinach',
           sample_id != '1577C-B') %>% 
    select(sample_id, cage, everything())

# to fix the cage names and turn into numeric
plates$cage <-  plates$cage %>% 
    str_replace('E', '') %>% 
    str_replace('W', '')
plates[,2:8] <- sapply(plates[,2:8],as.numeric)

plates_a <- plates %>% 
    select(cage, sample_id, total_percent_n, d15n, atom_percent_15n) %>% 
    filter(complete.cases(.))

plates_b <- plates %>% 
    select(cage, sample_id, total_percent_c, d13c, atom_percent_13c) %>% 
    filter(complete.cases(.))

plates_all <- full_join(plates_a, plates_b)

###########################
# merge with cage treatment data

cage_treatments <- read_csv(here::here('data/0_cage_allocation.csv')) %>% 
    clean_names() %>% 
    select(-c('predator_id', 'position', 'replicate'))

# merge isotope data with cage meta-data
isotopes_raw <- plates_all %>% 
    left_join(cage_treatments) %>% 
    select(sample_id, cage, treatment, everything())

## two samples were doubly labeled
## these lines correctly identify them
isotopes_raw[which(isotopes_raw$sample_id == '39WU2-1'),]$sample_id <- '39WU2'
isotopes_raw[which(isotopes_raw$sample_id == '39WU2-2'),]$sample_id <- '39WB2'

# naming it after the two directions/ two samplings it is named after
isotopes_e_w_prep <- isotopes_raw %>% 
    mutate(treatment = fct_relevel(treatment, c('control', 'ghop', 'mantid', 'spider')),
           sample_dir = as_factor(if_else(cage>=10, substr(sample_id, 3, 3), substr(sample_id, 2, 2))),
           sample_type = if_else(cage>=10, substr(sample_id, 4, 4), substr(sample_id, 3, 3)),
           rep = if_else(cage>=10, substr(sample_id, 5, 5),substr(sample_id, 4, 4)),
           treatment_vec = as.numeric(treatment)) %>% 
    select(treatment, sample_id, sample_type, rep, everything()) %>% 
    arrange(sample_id) %>% 
    filter(sample_type == 'U')

# only for cores from under litterbags, averaging the two
# isotopes_under_wide <- isotopes_e_w %>%
isotopes_e_w <- isotopes_e_w_prep %>%
    filter(sample_type == 'U') %>%
    group_by(cage, sample_dir) %>%
    summarise_at(vars(-c('treatment', 'sample_id', 'sample_type', 'rep')), mean) %>%
    left_join(cage_treatments)

# ## UNCOMMMENT TO NOT USE AVERAGING
# isotopes_e_w <- isotopes_e_w_prep

