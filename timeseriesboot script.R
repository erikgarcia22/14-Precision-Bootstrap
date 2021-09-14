#
# Written by Erik J. Garcia
# Date: 20210621


# Introduction and packages----

# WE NEED to Account for time series aspect of self-
# admin data. So going to try resampling in blocks
# with old boot package

# There is a call for individual level or precision medicine.
# However, there isn't a robust, reliable way to understand significant
# results at an individual by individual level. Bootstrapping is a robust 
# resampling method aimed at estimating an effect or parameter and 
# importantly, the precision surrounding the
# predicted effect or parameter.

# Load packages
library(tidyverse)
library(tidymodels)
library(boot)
library(renv)


# Initiate a reproducible seed and environment
set.seed(123) # reproducible simulations


# Get the REAL data ----

real_d <- read_csv("EJG3_infusions_training_dose_data.csv")

head(real_d)  # great in and looks accurate

# Need to flip data from wide to long format
# and restructure into single replicate column with
# the values going to infusions

real_d <- real_d %>% 
  pivot_longer(-1, names_to = "replicate",
               names_prefix = "replicate",
               values_to = "infusion") %>%
  drop_na()

# Using boot package bootstrap All data----

rat_select_boot_avg_fun <- function(data, rat) {
  picked<-data %>% 
    filter(rat == "p01") %>% 
    select(3)
  return(picked)
}


picked <- ts(rat_select_boot_avg_fun(real_d, "p01"))

inf_data <- real_d %>% 
  select(3)

boot_avg_fun <- function(picked) {
  fit <- ar(picked, order.max = 30)
  c(fit$order, mean(picked))
}

  timeboot <- tsboot(picked, boot_avg_fun, R = 1000, l=15, sim = "geom")

  plot(timeboot)
  
  grp_boot_avg_fun <- function(inf_data) {
    fit <- ar(inf_data, order.max = 50)
    c(fit$order, mean(picked))
  } 
  
  grp_timeboot <- tsboot(inf_data, grp_boot_avg_fun, R = 1000, l=25, sim = "fixed")
  plot(grp_timeboot)
  