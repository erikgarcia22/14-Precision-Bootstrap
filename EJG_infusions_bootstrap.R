#
# Written by Erik J. Garcia
# Date: 20210616


# Introduction and packages----

# There is a call for individual level or precision medicine.
# However, there isn't a robust, reliable way to understand significant
# results at an individual by individual level. Bootstrapping is a robust 
# resampling method aimed at estimating an effect or parameter and 
# importantly, the precision surrounding the
# predicted effect or parameter.

# Load packages
library(tidyverse)
library(tidymodels)
library(renv)

# Initiate a reproducible seed and environment
  set.seed(123) # reproducible simulations
  
  #renv::init()  # reproducible environment/packages
  
# Get the REAL data ----
  
  real_d <- read_csv("EJG3_infusions_training_dose_data.csv")

  head(real_d)  # great in and looks accurate
  
# Need to flip data from wide to long format
# and restructure into single replicate column with
# the values going to infusions
  
  real_d <- real_d %>% 
    pivot_longer(-1, names_to = "replicate",
                 names_prefix = "replicate",
                 values_to = "infusion")


# Create bootstrap resample of All data----
  
  real_d_boots <- bootstraps(real_d, times = 1000,
                             apparent = TRUE)
  

# create bootstrap of one rat
  
  p01_boots <- real_d %>% 
    filter(rat == "p01") %>% 
    bootstraps(times = 1000,
               apparent = TRUE)
  
# calculate the mean and median of all boot samples  
  p01_boot_stats <- p01_boots %>% 
    mutate(
      inf_mean = map(splits, mean_on_bootstrap),
      inf_median = map(splits, median_on_bootstrap))

  head(p01_boot_stats)  
  
  p01_infusion_means <- p01_boot_stats %>% 
    unnest(inf_mean)
  
  p01_infusion_means

# make a plot of the resampled means
  p01_infusion_means %>%
    select(estimate) %>% 
    ggplot(aes(estimate))+
    geom_histogram(binwidth = .3)+
    geom_vline(aes(xintercept = .lower), data = percentile_interv,
               col = "blue")+
    geom_vline(aes(xintercept = .upper), data = percentile_interv,
               col = "blue")

# Calculate interval percentile
  percentile_interv <- int_pctl(p01_boot_stats, inf_mean)
  