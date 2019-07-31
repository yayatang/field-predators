#==============================================
# 1. Import cage allocation + update
#==============================================
library(readr)
library(here)

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

cages_to_write <- cages3

# export sheet to update cage allocations
write_csv(cages_to_write, here::here('results/g1_updated_cages.csv'))