### We adapt the Early breast cancer example by creating a 3 state MS model; RRMS, SPMS, Death
# load packages
library(descem)
library(dplyr)
library(flexsurv)
library(ggplot2)
library(kableExtra)
library(purrr)
library(tidyr)

#load data

df_nh <- read.csv("data/df_nh_rrms_bc_ta616.csv")
u_nh <- read.csv("data/df_u_edss_ta616.csv")

read
#Utilities
util.data <- data.frame( name = c("u.edss0.ontx"),
                         value = c(u_nh[1,3]),
                         se=repc(u_nh[1,4]),
                         stringsAsFactors = FALSE
)


#Costs
cost.data <- data.frame( name = c("c.tx1","c.tx2","c.tx2",
                                  "c.edss0",
                                  c.),
                         value = c(1000,
                                   2000),
                         stringsAsFactors = FALSE
) %>%
  mutate(se= value/5)

           

#### Put objects here that do not change on any patient or intervention loop
  common_all_inputs <- add_item( #utilities
    #util_v = if(psa_bool){
    #  setNames(MASS::mvrnorm(1,util.data$value,diag(util.data$se^2)),util.data$name) # multivariate normal with no correlation
   # } else{setNames(util.data$value,util.data$name)},
    #util.idfs.ontx = util_v[["util.idfs.ontx"]], 
    #util.mbc.pps = util_v[["util.mbc.pps"]],
    nh = -log(1-df_nh_rrms),
    coef_e00= nh[1,1]
  ) %>%
  add_item( #costs
    #cost_v = if(psa_bool){
   #   setNames(draw_gamma(cost.data$value,cost.data$se),cost.data$name) #in this case I choose a gamma distribution
   # } else{setNames(cost.data$value,cost.data$name)},
    #cost.idfs.tx  = cost_v[["cost.idfs.tx"]],
    #cost.ae = cost_v[["cost.ae"]]
  ) %>%

  add_item(sex_pt = ifelse(rbernoulli(1,p=0.01),"male","female"),
                               nat.os.s = draw_resgompertz(1,
                                shape=if(sex_pt=="male"){0.102}else{0.115},
                                 rate=if(sex_pt=="male"){0.000016}else{0.0000041},
                                  lower_bound = 50) #in years, for a patient who is 50yo
                              
                               ) 



#### Put objects here that change as we loop through treatments for each patient (e.g. events can affect fl.tx, but events do not affect nat.os.s)
#common across trt but changes per pt could be implemented here (if (trt==)... )
  unique_pt_inputs <- add_item(
    f.edss0   = 1,
    f.edss1   = 0,
    f.edss2   = 0,
    f.edss3   = 0,
    f.edss4   = 0,
    f.edss5   = 0,
    f.edss6   = 0,
    f.edss7   = 0,
    f.edss8   = 0,
    f.edss9   = 0,
    f.rrms    = 0,
    f.spms    = 0,
    f.relapse = 0,
    f.line1   = 1,
    f.line2   = 0, 
    f.line3   = 0,
    f.line4   = 0,
    f.ontx    = 1,
    f.offtx   = 0,
    
    #fl.idfs.ontx             = 1,
    #fl.remission             = rbernoulli(1,0.8) #80% probability of going into remission
  )



#### Add initial events; two groups intervention and no intervention
  
  init_event_list <- 
    add_tte(trt="int",
            evts = c("start", "edss0"),
            other_inp = c("mort.trt",), #generate an intermediate input which is not an event but that we want to save
            input={ #intervention
              start <- 0
              edss0 <- draw_tte(1,dist="exp", coef1=coef_e00)
            })
  init_event_list <- 
    add_tte(trt="noint",
            evts = c("start"),
            other_inp = c("mort.trt"), #generate an intermediate input which is not an event but that we want to save
            input={ #reference strategy
              start <- 0
              edss0 <- draw_tte(1,dist="exp", coef1=coef_e00)
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