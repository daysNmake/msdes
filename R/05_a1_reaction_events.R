# load packages
library(descem)
library(dplyr)
library(tidyr)


# Add reaction to initial events


evt_react_list <-
  add_reactevt(name_evt = "start",
               input = {}) %>%
  add_reactevt(name_evt = "edss0",
               input = {
                 modify_item(list("f.edss0"= 1)) #Flag that patient is now in edss0
                 
               }) %>%
  
  add_reactevt(name_evt = "edss1",
               input = {
                 modify_item(list("f.edss0"= 0, "f.edss1"= 1,)) #Flag that patient is now in edss1
                 
               })
  