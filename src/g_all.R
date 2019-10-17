library(here)
library(tidyverse)
# scripts 2 + 3 need w eeks

# import feeding dates
feeding_dates <- read_csv(here::here('data/dates_for_feeding.csv'))
colnames(feeding_dates) <- c('week_num', 'date_index')
week <- 3

# to preseve week 9 randomization, wk 9 feeding dates are all set to 20190905
rand_date <- feeding_dates[which(feeding_dates$week_num == week),]$date_index

# generate ghop allocation tables
if(week != 8) {
    # script 2 needs the date in number form as a seed
    source(here::here('src/g1_import.R'))
    source(here::here('src/g2_randomize.R'))
    source(here::here('src/g3_allocate.R'))
} else {
    source(here::here('src/g4_makeup_weeks.R'))
                           # source(here::here('src/g5_final_week.R')))
}
