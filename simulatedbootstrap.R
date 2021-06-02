#
# Written by Erik J. Garcia
# Date: 20210602


# Introduction
# There is a call for individual level or precision medicine.
# However, there isn't a robust, reliable way to understand significant
# results. Bootstrapping is a robust resampling method aimed at estimating
# an effect or parameter and importantly, the precision surrounding the
# predicted effect or parameter.

# Load packages
library(tidyverse)
library(tidymodels)

set.seed(123) # reproducible simulations

# Create a dataset

  myd<- tibble(
    rat = rep(c("rat1", "rat2", "rat3", "rat4", "rat5"), 15),
    day = rep(1:15, each = 5),
    infusion = rpois(75, lambda= 35))

  myd %>% 
    group_by(day) %>% 
    summarize(mean = mean(infusion),
              sd = sd(infusion), 
              n = n())
  
  boots<- bootstraps()  
  
  tesat