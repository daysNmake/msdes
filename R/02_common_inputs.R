# load packages
library(descem)
library(dplyr)
library(flexsurv)
library(ggplot2)
library(kableExtra)
library(purrr)
library(tidyr)

#### Put objects here that do not change on any patient or intervention loop
common_all_inputs <- add_item( #utilities
  util_v = if(psa_bool){
    setNames(MASS::mvrnorm(1,util.data$value,diag(util.data$se^2)),util.data$name) # multivariate normal with no correlation
  } else{setNames(util.data$value,util.data$name)},
  util.edss0 = util_v[["u.edss0"]],     util.edss1 = util_v[["u.edss1"]], 
  util.edss2 = util_v[["u.edss2"]],     util.edss3 = util_v[["u.edss3"]], 
  util.edss4 = util_v[["u.edss4"]],     util.edss5 = util_v[["u.edss5"]], 
  util.edss6 = util_v[["u.edss6"]],     util.edss7 = util_v[["u.edss7"]], 
  util.edss8 = util_v[["u.edss8"]], 
  util.edss9 = util_v[["u.edss9"]] 
) %>%
  add_item( #costs
    cost_v = if(psa_bool){
      setNames(draw_gamma(cost.data$value,cost.data$se),cost.data$name) #in this case I choose a gamma distribution
    } else{setNames(cost.data$value,cost.data$name)},
    cost.m_edss0  = cost_v[["c.m_edss0"]],     cost.m_edss1  = cost_v[["c.m_edss1"]],
    cost.m_edss2  = cost_v[["c.m_edss2"]],     cost.m_edss3  = cost_v[["c.m_edss3"]],
    cost.m_edss4  = cost_v[["c.m_edss4"]],     cost.m_edss5  = cost_v[["c.m_edss5"]],
    cost.m_edss6  = cost_v[["c.m_edss6"]],     cost.m_edss7  = cost_v[["c.m_edss7"]],
    cost.m_edss8  = cost_v[["c.m_edss8"]],     cost.m_edss9  = cost_v[["c.m_edss9"]],
    
    cost.nm_edss0  = cost_v[["c.nm_edss0"]],     cost.nm_edss1  = cost_v[["c.nm_edss1"]],
    cost.nm_edss2  = cost_v[["c.nm_edss2"]],     cost.nm_edss3  = cost_v[["c.nm_edss3"]],
    cost.nm_edss4  = cost_v[["c.nm_edss4"]],     cost.nm_edss5  = cost_v[["c.nm_edss5"]],
    cost.nm_edss6  = cost_v[["c.nm_edss6"]],     cost.nm_edss7  = cost_v[["c.nm_edss7"]],
    cost.nm_edss8  = cost_v[["c.nm_edss8"]],     cost.nm_edss9  = cost_v[["c.nm_edss9"]],
    
    cost.tx.clab  = cost_v[["c.tx.clab"]],     cost.tx.alem  = cost_v[["c.tx.alem"]],
    cost.tx.fing  = cost_v[["c.tx.fing"]],     cost.tx.nat  = cost_v[["c.tx.nat"]],
    cost.tx.dac  = cost_v[["c.tx.dac"]]     
    #cost.ae = cost_v[["cost.ae"]]
  ) %>%
  
  add_item(sex_pt = ifelse(rbernoulli(1,p=0.01),"male","female"),
           nat.mort = draw_resgompertz(1,
                                       shape=if(sex_pt=="male"){0.102}else{0.115},
                                       rate=if(sex_pt=="male"){0.000016}else{0.0000041},
                                       lower_bound = 50) #in years, for a patient who is 50yo
           
  ) 


