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
library(broom)

# Initiate a reproducible seed and environment
set.seed(123) # reproducible simulations


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

#  samplemean <- function(data, i) {
 #   mean(data[i])
#  }

# Run the bootstrap resample 100x, save to object infus_boot
  
  infus_boot<- boot(real_d$infusion,
                    statistic = samplemean,
                    R=1000)

# Look at it
  infus_boot
  
# Resample subsets---- 
# Can we only sample 1, 3, 5...30 rows (which corresponds to rat sample size) 
  # at a time?
 
  # Take 1 
  sub_infus_boot001 <- boot(real_d$infusion,
                          statistic = samplemean,
                          R=1000,
                          sim = "parametric",
                          ran.gen = function(data, p)
                            data[sample(1:550, 1)])
  sub_infus_boot001 
  
  # Take 3
  sub_infus_boot003 <- boot(real_d$infusion,
                         statistic = samplemean,
                         R=1000,
                         sim = "parametric",
                         ran.gen = function(data, p)
                           data[sample(1:550, 3)])
  sub_infus_boot003     

  # Take 10
  sub_infus_boot010 <- boot(real_d$infusion,
                         statistic = samplemean,
                         R=1000,
                         sim = "parametric",
                         ran.gen = function(data, p)
                           data[sample(1:550, 10)])
  sub_infus_boot010  
  
  
# Confidence intervals Descriptive bootstrap stats----
# Calculate the confidence intervals for each sample
  
  # Take 1
    boot.ci(boot.out = sub_infus_boot001, type = c("norm", "basic", "perc"))

# Calculate range, mean, sd
  
  # Take 1
    range(sub_infus_boot001$t)   # NEED to include $t bec asking for
                                 # the t to be extracted from list
  
    mean(sub_infus_boot001$t)

    sd(sub_infus_boot001$t)
    
 # Make a preliminary plot of bootstraps
    plot(sub_infus_boot001)
    
    # as ggplot figure
    sub_boot_results<- as.data.frame(sub_infus_boot001$t)  # Don't forget $t
    
                # V1 is the column name of data.table
    
                # binwidth changes the how the data are grouped into bins
     ggplot()+
      geom_histogram(data = sub_boot_results, aes(V1), binwidth = 3) 
  