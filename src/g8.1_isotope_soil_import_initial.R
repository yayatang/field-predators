# soil isotopic data
library(tidyverse)
library(janitor)
library(gridExtra) # grid.arrange
library(GGally) #for ggpairs
source(here::here('src/yaya_fxns.R'))

##########################
# import 15N
plate2_15n <- read_csv(here::here('data/isotopes_litterbag_soil_plate2_15n.csv'), skip=3) %>% 
    clean_names() %>% 
    rename(d15n = d15nair) %>% 
    filter(sample_id != 'spinach',
           sample_id != '1577C-B')
plate2_15n <- plate2_15n[-c(1),]
plate2_15n <- plate2_15n[,-c(7:9)]
plate2_15n$cage <- substr(plate2_15n$sample_id, 1,2)

sapply(plate2_15n, class)
cols.num <- c('cage', 'sample_wt_mg', 'ampl_28_m_v', 'total_percent_n', 'd15n', 'atom_percent_15n')
plate2_15n[cols.num] <- sapply(plate2_15n[cols.num],as.numeric)

plate2_15n <- plate2_15n %>% select(cage, sample_id, everything())
plate2_15n <- select(plate2_15n, cage, sample_id, total_percent_n, d15n, atom_percent_15n)

# import 13c

plate2_13c <- read_csv(here::here('data/isotopes_litterbag_soil_plate2_13c.csv'), skip=3) %>% 
    clean_names() %>% 
    filter(sample_id != 'spinach',
           sample_id != '1577C-B') %>% 
    rename(d13c = d13cvpdb)
plate2_13c <- plate2_13c[-c(1),]
plate2_13c <- plate2_13c[,-c(7,8)]
plate2_13c$cage <- substr(plate2_13c$sample_id, 1,2)

cols.num <- c('cage', 'sample_wt_mg', 'ampl_44_m_v', 'total_percent_c', 'd13c', 'atom_percent_13c')
plate2_13c[cols.num] <- sapply(plate2_13c[cols.num],as.numeric)

plate2_13c <- select(plate2_13c, cage, sample_id, total_percent_c, d13c, atom_percent_13c)

###########################
# merge with cage treatment data

cage_treatments <- read_csv(here::here('data/0_cage_allocation.csv')) %>% 
    clean_names() %>% 
    select(-c('predator_id', 'position', 'replicate'))

# merge isotope data with cage meta-data
isotopes_raw <- left_join(plate2_15n, plate2_13c) %>% 
    left_join(cage_treatments) %>% 
    select(sample_id, cage, treatment, everything())

## two samples were doubly labeled
## these lines correctly identify them
isotopes_raw[which(isotopes_raw$sample_id == '39WU2-1'),]$sample_id <- '39WU2'
isotopes_raw[which(isotopes_raw$sample_id == '39WU2-2'),]$sample_id <- '39WB2'

isotopes_w <- isotopes_raw %>% 
    mutate(treatment = fct_relevel(treatment, c('control', 'ghop', 'mantid', 'spider')),
           sample_type = substr(sample_id, 4, 4),
           rep = substr(sample_id, 5, 5),
           treatment_vec = as.numeric(treatment)) %>% 
    select(treatment, sample_id, sample_type, rep, everything()) %>% 
    arrange(sample_id)

# only for cores from under litterbags, averaging the two
isotopes_under_wide <- isotopes_w %>% 
    filter(sample_type == 'U') %>% 
    group_by(cage) %>% 
    summarise_at(vars(-c('treatment', 'sample_id', 'sample_type', 'rep')), mean) %>% 
    left_join(cage_treatments)

