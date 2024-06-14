

#### Put objects here that do not change on any patient or intervention loop
common_all_inputs <- add_item( #utilities
  u_v = if(psa_bool){
    setNames(MASS::mvrnorm(1,u.data$value,diag(u.data$se^2)),u.data$name) # multivariate normal with no correlation
  } else{setNames(u.data$value, u.data$name)},
  u.edss0 = u_v[["u.edss0"]], u.edss1 = u_v[["u.edss1"]], 
  u.edss2 = u_v[["u.edss2"]], u.edss3 = u_v[["u.edss3"]], 
  u.edss4 = u_v[["u.edss4"]], u.edss5 = u_v[["u.edss5"]], 
  u.edss6 = u_v[["u.edss6"]], u.edss7 = u_v[["u.edss7"]], 
  u.edss8 = u_v[["u.edss8"]], u.edss9 = u_v[["u.edss9"]] 
) %>%
  add_item( #costs
    c_v = if(psa_bool){
      setNames(draw_gamma(c.data$value,c.data$se),c.data$name) # gamma distribution
    } else{setNames(c.data$value,c.data$name)},
    c.m_edss0  = c_v[["c.m_edss0"]],     c.m_edss1  = c_v[["c.m_edss1"]],
    c.m_edss2  = c_v[["c.m_edss2"]],     c.m_edss3  = c_v[["c.m_edss3"]],
    c.m_edss4  = c_v[["c.m_edss4"]],     c.m_edss5  = c_v[["c.m_edss5"]],
    c.m_edss6  = c_v[["c.m_edss6"]],     c.m_edss7  = c_v[["c.m_edss7"]],
    c.m_edss8  = c_v[["c.m_edss8"]],     c.m_edss9  = c_v[["c.m_edss9"]],
    
    c.nm_edss0  = c_v[["c.nm_edss0"]],   c.nm_edss1  = c_v[["c.nm_edss1"]],
    c.nm_edss2  = c_v[["c.nm_edss2"]],   c.nm_edss3  = c_v[["c.nm_edss3"]],
    c.nm_edss4  = c_v[["c.nm_edss4"]],   c.nm_edss5  = c_v[["c.nm_edss5"]],
    c.nm_edss6  = c_v[["c.nm_edss6"]],   c.nm_edss7  = c_v[["c.nm_edss7"]],
    c.nm_edss8  = c_v[["c.nm_edss8"]],   c.nm_edss9  = c_v[["c.nm_edss9"]],
    
    c.tx.clab  = c_v[["c.tx.clab"]],  c.tx.alem  = c_v[["c.tx.alem"]],
    c.tx.fing  = c_v[["c.tx.fing"]],  c.tx.nat  = c_v[["c.tx.nat"]],
    c.tx.dac   = c_v[["c.tx.dac"]],   c.tx.ib1a  = c_v[["c.tx.ib1a"]],     
    c.tx.ib1b  = c_v[["c.tx.ib1b"]],  c.tx.glat  = c_v[["c.tx.glat"]],
    
    c.tx_admn.clab  = c_v[["c.tx_admn.clab"]],  c.tx_admn.alem  = c_v[["c.tx_admn.alem"]],
    c.tx_admn.fing  = c_v[["c.tx_admn.fing"]],  c.tx_admn.nat   = c_v[["c.tx_admn.nat"]],
    c.tx_admn.dac   = c_v[["c.tx_admn.dac"]],   c.tx_admn.ib1a  = c_v[["c.tx_admn.ib1a"]],     
    c.tx_admn.ib1b  = c_v[["c.tx_admn.ib1b"]],  c.tx_admn.glat  = c_v[["c.tx_admn.glat"]]
    
  ) 
  
  #Put objects here that do not change as we loop through interventions for a patient
  common_pt_inputs <-   add_item(sex_pt = ifelse(rbernoulli(1,p=0.5),"male","female"),
           nat.mort = draw_resgompertz(1,
                                       shape=0, # a=0 is equivalent to the exponential distribution
                                       rate=if(sex_pt=="male"){0.000016*1.68}else{0.0000041*1.68}, #how do we make this depend on age? how do we make SMR probailistic?
                                       lower_bound = 50) #in years, for a patient who is 50yo
           
  ) 


