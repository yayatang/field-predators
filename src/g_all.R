library(here)
# scripts 2 + 3 need weeks
week <- 4
# script 2 needs the date in number form as a seed
source(here::here('src/g1_import.R'))
source(here::here('src/g2_randomize.R'))
source(here::here('src/g3_allocate.R'))