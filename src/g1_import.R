#==============================================
# 1. Import cage allocation + update
#==============================================
library(tidyverse)

cages0 <-  read_csv(here::here('data/0_cage_allocation.csv'))

# week 0: switch mis-installed cages1 26 + 28
cages1 <- cages0
new_28 <- cages1[which(cages1$cage==26), c('treatment','predatorID')]
new_26 <- cages1[which(cages1$cage==28), c('treatment','predatorID')]

cages1$treatment[28] <- new_28$treatment
cages1$predatorID[28] <- new_28$predatorID

cages1$treatment[26] <- new_26$treatment
cages1$predatorID[26] <- new_26$predatorID

# week 1: remove cage 3 (destroyed maybe by porcupine)
cages2 <- cages1
cages2 <- cages2[-which(cages2$cage==3),]

# week 3: remove cage 26 (mantid died)
cages3 <- cages2
cages3 <- cages3[-which(cages3$cage==26),]

# week 7: remove cage 9 (cage disturbance/mantid escape)
#cages4 <- cages3[-which(cages3$cage==9),]  #un-removed bc of mantid switching

# week 9.1: assign the feeding dates to all treatments
cages5 <-  cages3
# randomly assign cages to feedings
set.seed(20190905)
cages5$wk9_rando <- runif(nrow(cages5))

cages5_rand <- arrange(cages5, wk9_rando)

split_ghop <- c(rep(9.1, 4), rep(9.2, 3), rep(9.3, 3))
split_spiders <- c(rep(9.1, 4), rep(9.2, 3), rep(9.3, 3))
split_mantids <- c(rep(9.1, 3), rep(9.2, 2), rep(9.3, 2))

cages5_g <- cages5 %>% 
    filter(treatment=='ghop') %>% 
    arrange(wk9_rando) %>% 
    mutate(feeding_wk = split_ghop)

cages5_s <- cages5 %>% 
    filter(treatment=='spider') %>% 
    arrange(wk9_rando) %>% 
    mutate(feeding_wk = split_spiders)

cages5_m <- cages5 %>% 
    filter(treatment=='mantid') %>% 
    arrange(wk9_rando) %>% 
    mutate(feeding_wk = split_mantids,
           predatorID = NA) # remove mantid ID from this week of cages

# including the double feeding for cage 36
extra_feeding36 <- cages5_m[which(cages5_m$cage==36),]
extra_feeding36$feeding_wk <- 9.2
extra_feeding36$wk9_rando <- runif(1)

#update the mantid cage assignment table
cages5_m_all <- bind_rows(cages5_m, extra_feeding36)

#========
cages5_all <- bind_rows(cages5_g, cages5_s, cages5_m_all)
wk9.1_mantids <- tibble(predatorID = c('M02', 'M05', 'M07'),
                        m_rando = runif(3)) %>% 
    arrange(m_rando)
wk9.2_mantids <- tibble(predatorID = c('M02', 'M05', 'M07'),
                        m_rando = runif(3))%>% 
    arrange(m_rando)
wk9.3_mantids <- tibble(predatorID = c('M02', 'M05'), #removed M07 bc bad eater
                        m_rando = runif(2))%>% 
    arrange(m_rando)


#=========
# find the relevant cages for 9.1 + assign them mantids
cages5_9.1 <- cages5_all %>% 
    filter(feeding_wk==9.1)
cages5_9.1[which(cages5_9.1$treatment=='mantid'),]$predatorID <- wk9.1_mantids$predatorID

# location of each mantid from previous week
wk8_cages <- tibble(wk8_cage = c(9, 28, 31, 36),
                    predatorID = c('M05', 'M02', 'M07', 'M03'))
cages5_9.1 <- cages5_9.1 %>% 
    select(-wk9_rando) %>% 
    left_join(wk8_cages)

#=========
# week 9.2
# find the relevant cages for 9.2 + assign them mantids
cages5_9.2 <- cages5_all %>% 
    filter(feeding_wk==9.2)
cages5_9.2[which(cages5_9.2$treatment=='mantid'),]$predatorID <- wk9.2_mantids$predatorID

# location of each mantid from previous week
wk9.1_cages <- cages5_9.1 %>% 
    filter(treatment=='mantid') %>% 
    select(cage, predatorID) %>% 
    rename(wk9.1_cages = cage)

# wk9.1_cages <- tibble(wk9.1_cage = c(8, 9, 15),
#                       predatorID = c('M02', 'M07', 'M05'))
cages5_9.2 <- cages5_9.2 %>% 
    select(-wk9_rando) %>% 
    left_join(wk9.1_cages)

#=========
# week 9.3
# find the relevant cages for 9.3 + assign them mantids
cages5_9.3 <- cages5_all %>% 
    filter(feeding_wk==9.3)
cages5_9.3[which(cages5_9.3$treatment=='mantid'),]$predatorID <- wk9.3_mantids$predatorID

# location of each mantid from previous week
wk9.2_cages <- cages5_9.2 %>% 
    filter(treatment=='mantid') %>% 
    select(cage, predatorID) %>% 
    rename(wk9.2_cages = cage)

# wk9.2_cages <- tibble(wk9.2_cage = c(31, 22, 36),
#                       predatorID = c('M02', 'M07', 'M05'))
cages5_9.3 <- cages5_9.3 %>% 
    select(-wk9_rando) %>% 
    left_join(wk9.2_cages)
#if M05 doesn't eat (was slow in wk9.2), can be switched with M07

#==========
# assign cage data table version
cages_to_write <- cages3


# export sheet to update cage allocations
write_csv(cages_to_write, here::here(paste0('results/g1_updated_cages_wk',week,'.csv')))
