#
# Written by Erik J. Garcia
# Date: 20211116
#
#Edited by Caleb M. Kugel
# Date: 20211116


# Introduction and packages----

# This script to make quick figures for Caleb's data.
# He has already calculated the mean, SD and other descriptives
#
#


# Load packages
library(tidyverse)
library(renv)
library(RColorBrewer)
library(viridis)
library(gt)



# Get the data that Caleb produced

  real_stats <- read_csv("CMK_infusions_training_dose_stats_ejg20211122.csv")

  View(real_stats)

# Make a figure of the mean data----
  
  fig1 <- ggplot(real_stats, aes(x=name, y=Mean, fill=name))+
    geom_bar(stat = "identity")+
    geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=.5)+
    scale_x_discrete(name= "Sample Size",
                     labels = c("1", "3", "10", 
                                "30", "100", "300"))+
    scale_y_continuous(name= "Bootstrap mean +/- SD")+
    ggtitle("Effects of Sample Size on Bootstrap Mean Estimation", )+
    theme(legend.position="none",
          plot.title = element_text(hjust = 0.5))+  #hjust centers the title
    geom_smooth(aes(color = name, fill = name), method = "lm") +
    scale_color_viridis(discrete = TRUE, option = "D")+
    scale_fill_viridis(discrete = TRUE)
  # Still Needs Y-axis Labels
  
  

  fig1    # print object fig1
    ggsave(filename = "continuous_real_stats.png") # save last ggplot figure
    
    
# Make a table of the confidence interval data with gt package----
  
  table1 <- real_stats %>% 
    select(2,3,4,7,8) %>% # This selects the columns
    gt() %>%
    tab_header(
      title = "Range and Confidence Intervals",
      subtitle = NULL) %>%
    tab_spanner(
      label = "Range",
      columns = c(2,3)) %>%
    tab_spanner(
      label = "Confidence Interval",
      columns = c(4,5)) %>% 
    #tab_style(style = cell_borders(
      #sides = c("bottom"), 
      #color = "black", 
      #weight = px(2)),
              #locations = cells_column_spanners(
                #spanners = "Range")) %>% 
    #tab_style(style = cell_borders(
      #sides = c("bottom"), 
      #color = "black", 
      #weight = px(2)),
              #locations = cells_column_spanners(
                #spanners = "Confidence Interval",
      #length = .5)) %>% 
    tab_source_note(
      source_note = "Source: [EJG Data]") %>% 
    tab_options(
      table.align = 'center') %>% 
    cols_align(
      align = 'center')
    
  
    table1 # print object table1
    