
# Written by Erik J. Garcia
# Date: 20211104

# Introduction

# This function calculates the sample mean for each bootstrap resample
# you have to build a function to use the boot() function from boot package

# Calculates the sample mean for each bootstrap resample

samplemean <- function(data, i) {
  mean(data[i])
}

# This next code chuck is not needed for function. It is only included
# to illustrate how to integrate it into the boot()

#   infus_boot<- boot(real_d$infusion,
#                  statistic = samplemean,
#                  R=100)