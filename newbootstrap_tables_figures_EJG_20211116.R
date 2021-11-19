#
# Written by Erik J. Garcia
# Date: 20211116


# Introduction and packages----

# This script to make quick figures for Caleb's data.
# He has already calculated the mean, SD and other descriptives
#
#


# Load packages
library(tidyverse)
library(renv)
library(gt)


# Get the data that Caleb produced

  ck_data <- read_csv("CMK_infusions_training_dose_stats_ejg.csv")

  View(ck_data)

# Make a figure of the mean data
  
  fig1 <- ggplot(ck_data, aes(x=name, y=mean, fill=name))+
    geom_bar(stat = "identity")+
    geom_errorbar(aes(ymin=mean-SD, ymax=mean+SD), width=.5)+
    scale_x_discrete(name= "Sample Size",
                     labels = c("1", "3", "10", 
                                "30", "100", "300"))+
    scale_y_discrete(name= "Bootstrap mean +/- SD")+
    ggtitle("Effects of Sample Size on Bootstrap Mean Estimation", )+
    theme(legend.position="none",
          plot.title = element_text(hjust = 0.5)) # hjust centers the title
    

  fig1    # print object fig1

  #  ggsave(filename = "ck_data.png") # save last ggplot figure
    

    
# Make a table of the confidence interval data with gt package
  
  table1 <- ck_data %>% 
    select(2,3,4,7,8) %>% # This selects the columns
    gt() %>%
    tab_header(
      title = "Caleb's table 1",
      subtitle = "subtitle")
  
    table1 # print object table1
    