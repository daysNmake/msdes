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
  add_reactevt(name_evt = "ttot.line1",
               input = {
                 modify_item(list("f.line1"= 0,"f.tx.on"= 0 , "f.tx.off"= 1)) #Flag that patient is now off-treatment, no txt switching
                 
               }) %>%
  
  add_reactevt(name_evt = "edss1",
               input = {
                 modify_item(list("f.edss0"= 0, "f.edss1"= 1)) #Flag that patient is now in edss1
                 
               }) %>%
  
  add_reactevt(name_evt = "edss2",
               input = {
                 modify_item(list("f.edss0"= 0, "f.edss1"= 0, "f.edss2"= 1)) #Flag that patient is now in edss2
                 
               }) %>%
  
  add_reactevt(name_evt = "edss3",
               input = {
                 modify_item(list("f.edss0"= 0, "f.edss1"= 0, "f.edss2"= 0, "f.edss3"= 1)) #Flag that patient is now in edss3
                 
               }) %>%
  
  add_reactevt(name_evt = "edss4",
               input = {
                 modify_item(list("f.edss0"= 0, "f.edss1"= 0, "f.edss2"= 0, "f.edss3"= 0, "f.edss4"= 1)) #Flag that patient is now in edss4
                 
               }) %>%
  
  add_reactevt(name_evt = "edss5",
               input = {
                 modify_item(list("f.edss0"= 0, "f.edss1"= 0, "f.edss2"= 0, "f.edss3"= 0, "f.edss4"= 0, "f.edss5"= 1)) #Flag that patient is now in edss5
                 
               }) %>%
  
  add_reactevt(name_evt = "edss6",
               input = {
                 modify_item(list("f.edss0"= 0, "f.edss1"= 0, "f.edss2"= 0, "f.edss3"= 0, "f.edss4"= 0, "f.edss5"= 0, "f.edss6"= 1)) #Flag that patient is now in edss6
                 
               }) %>%
  
  add_reactevt(name_evt = "edss7",
               input = {
                 modify_item(list("f.edss0"= 0, "f.edss1"= 0, "f.edss2"= 0, "f.edss3"= 0, "f.edss4"= 0, "f.edss5"= 0, "f.edss6"= 0, "f.edss7"= 1)) #Flag that patient is now in edss7
                 
               })  %>%
  
  add_reactevt(name_evt = "edss8",
               input = {
                 modify_item(list("f.edss0"= 0, "f.edss1"= 0, "f.edss2"= 0, "f.edss3"= 0, "f.edss4"= 0, "f.edss5"= 0, "f.edss6"= 0, "f.edss7"= 0, "f.edss8"= 1)) #Flag that patient is now in edss8
                 
               })  %>%
  
  add_reactevt(name_evt = "edss9",
               input = {
                 modify_item(list("f.edss0"= 0, "f.edss1"= 0, "f.edss2"= 0, "f.edss3"= 0, "f.edss4"= 0, "f.edss5"= 0, "f.edss6"= 0, "f.edss7"= 0, "f.edss8"= 0, "f.edss9"= 1)) #Flag that patient is now in edss9
                 
               })  %>%
  
  add_reactevt(name_evt = "death",
               input = {
                 modify_item(list("f.edss0"= 0, "f.edss1"= 0, "f.edss2"= 0, "f.edss3"= 0, "f.edss4"= 0, "f.edss5"= 0, "f.edss6"= 0, "f.edss7"= 0, "f.edss8"= 0, "f.edss9"= 0,
                                  "curtime"=Inf )) #
                 
               })
  