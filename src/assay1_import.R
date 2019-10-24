library(tidyverse)
library(here)

a_ghops0 <- read_csv(here::here('data/feeding4_assay_ghops.csv'))
a_preds0 <- read_csv(here::here('data/feeding4_assay_predators.csv'))

colnames(a_ghops0) <- c('ghopID', 'ghop_num', 'm_tube', 'm_tube_ghop', 'calc_m_ghop', 
                       'ghop_fate', 'predatorID', 'comments', 'm_tube_poop')

colnames(a_preds0) <- c('predator_type', 'predatorID', 
                      'm_lid','m_lid_pred_start', 'm_lid_pred_24', 
                      'm_tube_feces0', 'm_tube_feces_wet', 
                      'm_tube_remains0', 'm_tube_remains_wet',
                      'm_tube_feces_dry', 'm_tube_remains_dry',
                      'feeding_observations','pred_fieldID', 'eggsac_notes')

a_ghops0 <- a_ghops0[rowSums(is.na(a_ghops0)) != ncol(a_ghops0),]
a_preds0 <- a_preds0[rowSums(is.na(a_preds0)) != ncol(a_preds0),]

