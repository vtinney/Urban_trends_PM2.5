# 00.create.RR
# This is a script to use GBD MRBRT estimates and incorporate the TMREL of 2.4-5.9 
# in order to create a RR for use in the HIA.
# Created: 8-30-2020
# Author: V Southerland
# Note - each iteration takes approximately 4 hrs. Did not incorporate into a loop and each age/outcome
# was run separately. 
# Updated: 5-17-2021 to add percentiles

# SUBSET 1
#=========================================================================================================

library(tidyverse)
#setwd('C:/Users/vat05/Dropbox/GBD 2019/MRBRT/June_2020_mrbrt/mrbrt/')

setwd('/GWSPH/groups/anenberggrp/VAS/GBD_2020/final/mrbrt/')

# Set uniform distribution draws, 1000 estimates between 2.4 and 5.9

ages <- c(25,30,35,40,45)

#ages <- c(50,55,60,65,70)
#ages <- c(75,80,85,90,95)

draws <- runif(1000, min = 2.4, max = 5.9)

for (j in 1:length(ages)){

  n <- read.csv(paste('cvd_stroke_',ages[j],'.csv'))

# Create lower and upper bounds for each exposure integer
for (t in 1:nrow(n)){
  n$upper2[t] <- n$exposure_spline[t+1]
}

ls <- list() # store results in a list

for (j in 2:nrow(n)){
  mat <- matrix(ncol = 1000,nrow = 1000) # Create 1000x1000 matrix
      for (k in 3:1001){
        for (i in 1:length(draws)){
      exp <- n[j,1]

      tmrel <- draws[i] # Draw from 1000 distribution of TMREL and set equal to draws

      if(exp > tmrel){
        mrbrt.tmrel <- n[,k][tmrel > n[,1] & tmrel < n[,1006]] # If the exposure level is greater than the tmrel, then divide the MRBRT of the exp integer by the MRBRT of the tmrel
        mrbrt <- n[j,k]

        rr <- mrbrt/mrbrt.tmrel

      }else{ # otherwise set the RR to 1.
        rr <- 1
        }

      mat[k-2,i] <- rr
    }
  }
  ls[[j-1]] <- mat
  print(j)
}

# Create a running average of the 1000x1000 matrix
mat2 <- matrix(ncol = 2,nrow = 385)
for (z in 1:length(ls)){
  mat2[z,1] <- mean(ls[[z]],na.rm=TRUE)
  mat2[z,2] <- n[z+1,1]
}


write.csv(mat2, paste0('cvd_stroke_',ages[j],'_rr2.csv'))

}
