#
# Written by Erik J. Garcia
# Date: 20211207


# Introduction and packages----
#  This script is designed to perform a single bootstrap with various
 # sample sizes (1,3,10...300 etc) This better illustrates how sample
# size can affect the observed mean.


# Load packages
library(tidyverse)
library(tidymodels)
library(boot)
library(renv)
library(broom)

# Initiate a reproducible seed and environment
set.seed(123) # reproducible simulations

# Uncomment the set() line to resave to your working directory
# set wd
    # setwd("~/R_learning/newbootstrap_cmk20211110")


# Get the REAL data ----

real_d <- read_csv("EJG3_infusions_training_dose_data.csv")

# Just checking data are in correctly

head(real_d)  # great in and looks accurate

# Need to flip data from wide to long format
# and restructure into single replicate column with
# the values going to infusions

real_d <- real_d %>% 
  pivot_longer(-1, names_to = "replicate",
               names_prefix = "replicate",
               values_to = "infusion") %>%
  drop_na()

# Start from scratch here. Bootstrap the dataset----

# Calculates the sample mean for each bootstrap resample

# samplemean <- function(data, i) {
# mean(data[i])
# }

# Run the bootstrap resample 1000x, save to object infus_boot


# Resample subsets---- 
# Only perform a SINGLE bootstrap with various sample sizes
# Can we only sample 1, 3, 5...30 rows (which corresponds to rat sample size) 
# at a time?


# one bootstrap Take 1 
  oneboot_sub_infus_boot001 <- boot(real_d$infusion,
                                  statistic = samplemean,
                                  R=1,
                                  sim = "parametric",
                                  ran.gen = function(data, p)
                                    data[sample(1:550, 1)])
  oneboot_sub_infus_boot001

# one bootstrap Take 3    

  oneboot_sub_infus_boot003 <- boot(real_d$infusion,
                                  statistic = samplemean,
                                  R=1,
                                  sim = "parametric",
                                  ran.gen = function(data, p)
                                    data[sample(1:550, 3)])
  oneboot_sub_infus_boot003

# one bootstrap Take 10    
  
  oneboot_sub_infus_boot010 <- boot(real_d$infusion,
                                    statistic = samplemean,
                                    R=1,
                                    sim = "parametric",
                                    ran.gen = function(data, p)
                                      data[sample(1:550, 10)])
  oneboot_sub_infus_boot010
  
# one bootstrap Take 30    
  
  oneboot_sub_infus_boot030 <- boot(real_d$infusion,
                                    statistic = samplemean,
                                    R=1,
                                    sim = "parametric",
                                    ran.gen = function(data, p)
                                      data[sample(1:550, 30)])
  oneboot_sub_infus_boot030  
 
# one bootstrap Take 100    
  
  oneboot_sub_infus_boot100 <- boot(real_d$infusion,
                                    statistic = samplemean,
                                    R=1,
                                    sim = "parametric",
                                    ran.gen = function(data, p)
                                      data[sample(1:550, 100)])
  oneboot_sub_infus_boot100  

# one bootstrap Take 300    
  
  oneboot_sub_infus_boot300 <- boot(real_d$infusion,
                                    statistic = samplemean,
                                    R=1,
                                    sim = "parametric",
                                    ran.gen = function(data, p)
                                      data[sample(1:550, 300)])
  oneboot_sub_infus_boot300  

  # Confidence intervals Descriptive bootstrap stats----
#### EJG notes Dont need confidence interval bec only a single bootstrap
  # was completed to simulate a single experiment.



# Calculate range, mean, sd

#### EJG kept the range, sd calculation in there bec it should equal
# the mean if the bootstrap only resampled a single time. It did and 
# the simulation worked. yay!!

# Take 1 
# NEED to include $t bec asking for
# the t to be extracted from list
  range(oneboot_sub_infus_boot001$t)   

  mean(oneboot_sub_infus_boot001$t)

  sd(oneboot_sub_infus_boot001$t)

# Take 3 
# NEED to include $t bec asking for
# the t to be extracted from list
  range(oneboot_sub_infus_boot003$t)   
  
  mean(oneboot_sub_infus_boot003$t)
  
  sd(oneboot_sub_infus_boot003$t)

# Take 10 
# NEED to include $t bec asking for
# the t to be extracted from list
  range(oneboot_sub_infus_boot010$t)   
  
  mean(oneboot_sub_infus_boot010$t)
  
  sd(oneboot_sub_infus_boot010$t)
  
# Take 30 
# NEED to include $t bec asking for
# the t to be extracted from list
  range(oneboot_sub_infus_boot030$t)   
  
  mean(oneboot_sub_infus_boot030$t)
  
  sd(oneboot_sub_infus_boot030$t)  
 
# Take 100 
# NEED to include $t bec asking for
# the t to be extracted from list
  range(oneboot_sub_infus_boot100$t)   
  
  mean(oneboot_sub_infus_boot100$t)
  
  sd(oneboot_sub_infus_boot100$t)

  
# Take 300 
# NEED to include $t bec asking for
# the t to be extracted from list
  range(oneboot_sub_infus_boot300$t)   
  
  mean(oneboot_sub_infus_boot300$t)
  
  sd(oneboot_sub_infus_boot300$t)  
  


