library(descem)

library(dplyr)

library(flexsurv)

library(ggplot2)
library(kableExtra)

library(purrr)
library(tidyr)


#Utilities
util.data <- data.frame( name = c(),
                         value = c(),
                         se=rep(),
                         stringsAsFactors = FALSE
)


#Costs
cost.data <- data.frame( name = c(),
                         value = c(),
                         stringsAsFactors = FALSE
) %>%
  mutate(se= value/5)

           

#### Put objects here that do not change on any patient or intervention loop
  common_all_inputs <- add_item( #utilities
    util_v = if(psa_bool){
      setNames(MASS::mvrnorm(1,util.data$value,diag(util.data$se^2)),util.data$name) # multivariate normal with no correlation
    } else{setNames(util.data$value,util.data$name)},
    #util.idfs.ontx = util_v[["util.idfs.ontx"]], 
    #util.mbc.pps = util_v[["util.mbc.pps"]]
  ) %>%
  add_item( #costs
    cost_v = if(psa_bool){
      setNames(draw_gamma(cost.data$value,cost.data$se),cost.data$name) #in this case I choose a gamma distribution
    } else{setNames(cost.data$value,cost.data$name)},
    #cost.idfs.tx  = cost_v[["cost.idfs.tx"]],
    #cost.ae = cost_v[["cost.ae"]]
  )



#### Put objects here that do not change as we loop through treatments for a patient
  common_pt_inputs <- add_item(sex_pt = ifelse(rbernoulli(1,p=0.01),"male","female"),
                               nat.os.s = draw_resgompertz(1,
                                                           shape=if(sex_pt=="male"){0.102}else{0.115},
                                                           rate=if(sex_pt=="male"){0.000016}else{0.0000041},
                                                           lower_bound = 50) ) #in years, for a patient who is 50yo



#### Put objects here that change as we loop through treatments for each patient (e.g. events can affect fl.tx, but events do not affect nat.os.s)
#common across trt but changes per pt could be implemented here (if (trt==)... )
  unique_pt_inputs <- add_item(
    #fl.idfs.ontx             = 1,
    #fl.remission             = rbernoulli(1,0.8) #80% probability of going into remission
  )



#### Add initial events; two groups intervantion and no intervention
  
  init_event_list <- 
    add_tte(trt="int",
            evts = c("start"),
            other_inp = c("os.early","os.mbc"),
            input={ #intervention
              start <- 0
              
            })
  init_event_list <- 
    add_tte(trt="noint",
            evts = c("start"),
            other_inp = c("os.early","os.mbc"),
            input={ #intervention
              start <- 0
              
            })
  
  
#### Add reaction to initial events
  
  
  
  
  

  
#### Run model
#Logic is: per patient, per intervention, per event, react to that event.
results <- RunSim(  
  npats=2000,                              # number of patients to be simulated
  n_sim=1,                                  # number of simulations to run
  psa_bool = FALSE,                         # use PSA or not. If n_sim > 1 and psa_bool = FALSE, then difference in outcomes is due to sampling (number of pats simulated)  
  trt_list = c("int", "noint"),             # intervention list
  common_all_inputs = common_all_inputs,    # inputs common that do not change within a simulation
  common_pt_inputs = common_pt_inputs,      # inputs that change within a simulation but are not affected by the intervention
  unique_pt_inputs = unique_pt_inputs,      # inputs that change within a simulation between interventions
  init_event_list = init_event_list,        # initial event list
  evt_react_list = evt_react_list,          # reaction of events
  util_ongoing_list = util_ongoing,
  cost_ongoing_list = cost_ongoing,
  cost_instant_list = cost_instant,
  ncores = 2,                               # number of cores to use, recommended not to use all
  drc = 0.035,                              # discount rate for costs
  drq = 0.035,                              # discount rate for QALYs
  input_out = c(                            # list of additional outputs (Flags, etc) that the user wants to export for each patient and event
    "os.early",
    "os.mbc",
    "nat.os.s",
    "sex_pt"
  )
)