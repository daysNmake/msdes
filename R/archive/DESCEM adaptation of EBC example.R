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
init_params <- read.csv("data/df_init_params.csv")

p_nh <- read.csv("data/df_nh_rrms_bc_ta616.csv")

u_nh <- read.csv("data/df_u_edss_ta616.csv")
#u_relapse
#u_carer
#u_ae

c_m_nh <- read.csv("data/df_c_directmedical_edss.csv")
c_nm_nh <- read.csv("data/df_c_direct_nonmedical_edss.csv")
c_tx <- read.csv("data/df_c_drugs_ta616.csv")
#c_monitoring
#c_replase
#c_disutilities


# Transform data

r_nh <- -log(1-p_nh[,2:11]) # natural history rates by taking -log(1-P)


#Utilities data: health states
util.data <- data.frame( name =  c(paste0("u.edss", 0:9, "")),
                         value = c(u_nh[,4]),
                         se=     c(u_nh[,5]),
                         stringsAsFactors = FALSE
)

util.data[10,2:3] <-0

#Costs : direct medical, direct non-medical, treatment
cost.data <- data.frame( name = c(paste0("c.m_edss", 0:9, ""),
                                  paste0("c.nm_edss", 0:9, ""),
                                  "c.tx.clab", "c.tx.alem", "c.tx.fing", "c.tx.nat", "c.tx.dac"),
                         value = c(c_m_nh[1:9,2],c_m_nh[10,6], c_nm_nh[1:10,6],c_tx[2:6,3]),
                         se=     c(c_m_nh[1:9,3],c_m_nh[10,7], c_nm_nh[1:10,7], value[21:25]/5 ),
                         stringsAsFactors = FALSE
) 


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



#### Put objects here that change as we loop through treatments for each patient (e.g. events can affect fl.tx, but events do not affect mat.mort.s)
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
    f.rrms    = 1,
    f.spms    = 0,
    f.relapse = 0,
    f.line1   = 1,
    f.line2   = 0, 
    f.line3   = 0,
    f.line4   = 0,
    f.tx.on   = 1,
    f.tx.off  = 0,
    f.tx.nat  = 0,
    f.tx.clab = 0,
    f.tx.fing = 0,
    f.tx.dac  = 0,
    f.rr2sp             = rbernoulli(1,0.1) #10% probability of converting from rrms to spms
  )



#### Add initial events; two groups intervention and no intervention
  
  init_event_list <- 
    add_tte(trt="int",
            evts = c("start", "edss0", "edss1","edss2","edss3","edss4", "edss5","edss6","edss7", "edss8","edss9",
                     "tot.line1"),
            other_inp = c("mort.trt",), #generate an intermediate input which is not an event but that we want to save
            input={ #intervention
              #start
              start <- 0
              
              #EDSS0
              edss0 <- draw_tte(1,dist="exp", coef1=)
              tot.line1 # draw time on tot.line1
              
              #EDSS1-9
              if (f.edss0==1){ 
                
               #draw event times transitioning from edss0 to edss1-9 and select the earliest
                edss_0_1 <- draw_tte(1,dist="exp", coef1=)
                edss_0_2 <- draw_tte(1,dist="exp", coef1=)
                edss_0_3 <- draw_tte(1,dist="exp", coef1=)
                edss_0_4 <- draw_tte(1,dist="exp", coef1=)
                edss_0_5 <- draw_tte(1,dist="exp", coef1=)
                edss_0_6 <- draw_tte(1,dist="exp", coef1=)
                edss_0_7 <- draw_tte(1,dist="exp", coef1=)
                edss_0_8 <- draw_tte(1,dist="exp", coef1=)
                edss_0_9 <- draw_tte(1,dist="exp", coef1=)
                
                f.edss1<-1 #use min function to determine next edss state?
                f.edss0<-0
                } else if(f.edss1==1) {f.edss1<-0
                    f.edss2<-1
                    edss2 <- draw_tte(1,dist="exp", coef1=)
                    
                    } else if (f.edss2==1) {f.edss2<-0
                          f.edss3<-1
                          edss3 <- draw_tte(1,dist="exp", coef1=)
                          
                          } else if (f.edss3==1) {f.edss3<-0
                              f.edss4<-1
                              edss4 <- draw_tte(1,dist="exp", coef1=)
                              
                              } else if (f.edss4==1) {f.edss4<-0
                                f.edss5<-1
                                edss5 <- draw_tte(1,dist="exp", coef1=)
                                
                                } else if (f.edss5==1) {f.edss5<-0
                            f.edss6<-1
                            edss6 <- draw_tte(1,dist="exp", coef1=)
                            
                            } else if (f.edss6==1) {f.edss6<-0
                        f.edss7<-1
                        edss7 <- draw_tte(1,dist="exp", coef1=)
                        
                        } else if (f.edss7==1) {f.edss7<-0
                    f.edss8<-1
                    edss8 <- draw_tte(1,dist="exp", coef1=)
                    
                    } else if (f.edss8==1) {f.edss8<-0
                f.edss9<-1
                edss9 <- draw_tte(1,dist="exp", coef1=)
                
                }
              
            # mortality  
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
    "mat.mort.s",
    "sex_pt"
  )
)