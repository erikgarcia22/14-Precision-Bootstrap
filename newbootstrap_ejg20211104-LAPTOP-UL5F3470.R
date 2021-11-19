#
# Written by Erik J. Garcia
# Date: 20211104


# Introduction and packages----

# WE NEED to Account for dependence of observations
# time series aspect of self-admin data. So going to
# try resampling in blocks with old boot package.


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

# Start from scratch here. Bootstrap the dataset

  samplemean <- function(data, i) {
    mean(data[i])
  }

  infus_boot<- boot(real_d$infusion,
                    statistic = samplemean,
                    R=100)

# Can we only sample 30 rows at a time?
  
  sub_infus_boot30 <- boot(real_d$infusion,
                         statistic = samplemean,
                         R=100,
                         sim = "parametric",
                         ran.gen = function(data, p)
                           data[sample(1:550, 30)])
  sub_infus_boot30     

  sub_infus_boot10 <- boot(real_d$infusion,
                         statistic = samplemean,
                         R=100,
                         sim = "parametric",
                         ran.gen = function(data, p)
                           data[sample(1:550, 10)])
  sub_infus_boot10  
  