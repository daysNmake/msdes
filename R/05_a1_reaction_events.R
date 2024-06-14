# Add reaction to initial events


evt_react_list <-
  add_reactevt(name_evt = "start",
               input = {
                 modify_item(list("f.tx.nat"= if(trt=="int"){1}else{0}, # Natalizumab for the treatment group
                                  "f.tx.ib1b"= if(trt=="noint"){1}else{0}, # IF-1b for the reference group
                                  "f.tx.on" = 1, "f.line1"= 1, "f.rrms"= 1, "f.tx.off"= 0)) 
               }) %>%
  add_reactevt(name_evt = "edss0",
               input = {
                 modify_item(list("f.edss0"= 1)) #Flag that patient is now in edss0
                 
               }) %>%
  add_reactevt(name_evt = "tot.line1",
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
  
#common utility across arms

util_ongoing <- add_util(evt = c("start", "edss0", "edss1","edss2","edss3","edss4", "edss5","edss6","edss7", "edss8","edss9",
                                 "tot.line1"),
                         trt = c("int", "noint"),
                         util = if (f.edss0==1) {
                           u.edss0 * f.edss0  
                             } else if (f.edss1==1) {
                               u.edss1 * f.edss1 
                               } else if (f.edss2==1) {
                                 u.edss2 * f.edss2 
                                 } else if (f.edss3==1) {
                                   u.edss3 * f.edss3 
                                   } else if (f.edss4==1) {
                                     u.edss4 * f.edss4 
                                   } else if (f.edss5==1) {
                                   u.edss5 * f.edss5 
                                 } else if (f.edss6==1) {
                                 u.edss6 * f.edss6 
                               } else if (f.edss7==1) {
                               u.edss7 * f.edss7 
                             } else if (f.edss8==1) {
                             u.edss8 * f.edss8 
                           } else if (f.edss9==1) {
                             u.edss9 * f.edss9 
                         }else{0.1})

cost_ongoing <-
  add_cost(
    evt = c("start", "tot.line1") ,
    trt = "noint",
    cost = f.tx.on*f.tx.ib1b*c.tx.ib1b + f.tx.on*f.tx.ib1b*c.tx_admn.ib1b) %>%
  add_cost(
    evt = c("start", "tot.line1") ,
    trt = "int",
    cost = f.tx.on*f.tx.nat*c.tx.nat + f.tx.on*f.tx.nat*c.tx_admn.nat) %>%
  add_cost(
    evt = c("edss0", "edss1","edss2","edss3","edss4", "edss5","edss6","edss7", "edss8","edss9"),
    trt = c("noint","int"),
    cost = c.m_edss0*f.edss0 + c.nm_edss0*f.edss0 +
           c.m_edss1*f.edss1 + c.nm_edss1*f.edss1 +
           c.m_edss2*f.edss2 + c.nm_edss2*f.edss2 + 
           c.m_edss3*f.edss3 + c.nm_edss3*f.edss3 + 
           c.m_edss4*f.edss4 + c.nm_edss4*f.edss4 + 
           c.m_edss5*f.edss5 + c.nm_edss5*f.edss5 +
           c.m_edss6*f.edss6 + c.nm_edss6*f.edss6 + 
           c.m_edss7*f.edss7 + c.nm_edss7*f.edss7 +
           c.m_edss8*f.edss8 + c.nm_edss8*f.edss8 +
           c.m_edss9*f.edss9 + c.nm_edss9*f.edss9 )
  
                         