#
# Written by Erik J. Garcia
# Date: 20210602

 
# Introduction
# This script is me learning how to do these analyses for this reason
# There is a call for individual level or precision medicine.
# However, there isn't a robust, reliable way to understand significant
# results at an individual by individual level. Bootstrapping is a robust 
# resampling method aimed at estimating an effect or parameter and 
# importantly, the precision surrounding the
# predicted effect or parameter.

# Load packages----
library(tidyverse)
library(tidymodels)
library(renv)

set.seed(123) # reproducible simulations

# Create a data set

  myd<- tibble(
    rat = rep(c("rat1", "rat2", "rat3", "rat4", "rat5"), 15),
    day = rep(1:15, each = 5),
    infusion = rpois(75, lambda= 35))

  myd %>% 
    group_by(day) %>% 
    summarize(mean = mean(infusion),
              sd = sd(infusion), 
              n = n())
  
  boots <- bootstraps(myd, times = 1000)
  
  mean_on_bootstrap <- function(split) {
    data <- analysis(split) %>% 
      pull(infusion)
    return(tibble(
        term= "mean",
        estimate = mean(data),
        std.err = sd(data)/sqrt(length(data))))
  }
  
  median_on_bootstrap <- function(split) {
    data <- analysis(split) %>% 
      pull(infusion) 
    return(tibble(
        term= "median",
        estimate = median(data)))
  }

# Add new calculated data to the bootstrap
  # Adds new mean and median tibbles to the bootstraps
  
  boot_stats <- boots %>% 
    mutate(
      inf_mean = map(splits, mean_on_bootstrap),
      inf_median = map(splits, median_on_bootstrap))
    
  head(boot_stats)
  
  # Calculate the interval percentile
  
  int_pctl(boot_stats, inf_mean)

# Visualize data- Means
  
  infusion_means <- boot_stats %>% 
    unnest(inf_mean)

  infusion_means %>%
    select(estimate) %>% 
    ggplot(aes(estimate))+
    geom_histogram()
 
# Visualize data - Median
  
  infusion_median <- boot_stats %>% 
    unnest(inf_median)
  
  infusion_median %>% 
    select(estimate) %>% 
    ggplot(aes(estimate))+
    geom_histogram()
  