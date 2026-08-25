library(WARDEN)

library(dplyr)
library(ggplot2)
library(kableExtra)
library(purrr)

options(scipen = 999)
options(digits=3)
options(tibble.print_max = 50)

#functions to generate initial events and reaction events
source("R/warden_functions.R")
#still to figure out how to make a gen inputs function
# can modify elements of a call using rlang::expr refer to https://adv-r.hadley.nz/quasiquotation.html?

#We don't need to use sensitivity_inputs here, so we don't add that object

#Put objects here that do not change on any patient or intervention loop
#We use add_item and add_item to showcase how the user can implement the inputs (either works, add_item is just faster)
common_all_inputs <-add_item(input = {
  util.sick   <- 0.8
  util.sicker <- 0.5
  cost.sick   <- 3000
  cost.sicker <- 7000
  cost.int    <- 1000
  coef_noint  <- log(0.2)
  HR_int      <- 0.8
  drc         <- 0.035 #different values than what's assumed by default
  drq         <- 0.035
  random_seed_sicker_i <- sample.int(100000,npats,replace = FALSE)
})  #to be used as seeds to draw the time to event for sicker, to ensure same luck for the same patient independently of the arm


#Put objects here that do not change as we loop through treatments for a patient
common_pt_inputs <- add_item(death= max(0.0000001,rnorm(n=1, mean=12, sd=3))) 

#Put objects here that change as we loop through treatments for each patient (e.g. events can affect fl.tx, but events do not affect nat.os.s)
unique_pt_inputs <- add_item(fl.sick = 1,
                             q_default = util.sick,
                             c_default = cost.sick + if(arm=="int"){cost.int}else{0}) 
# 
# init_event_list <- 
#   add_tte(arm=c("noint","int"), evts = c("sick","sicker","death") ,input={
#     sick <- 0
#     sicker <- draw_tte(1,dist="exp", coef1=coef_noint, beta_tx = ifelse(arm=="int",HR_int,1), seed = random_seed_sicker_i[i]) #this way the value would be the same if it wasn't for the HR, effectively "cloning" patients luck
#     
#   })
# 
# 
# evt_react_list <-
#   add_reactevt(name_evt = "sick",
#                input = {}) %>%
#   add_reactevt(name_evt = "sicker",
#                input = {
#                  q_default <- util.sicker
#                  c_default <- cost.sicker + if(arm=="int"){cost.int}else{0}
#                  fl.sick   <- 0 
#                }) %>%
#   add_reactevt(name_evt = "death",
#                input = {
#                  q_default <- 0
#                  c_default <- 0
#                  curtime   <- Inf
#                }) 
# 



v_events<- c("sick","sicker","death")
init_event_list <- gen_init_events(RRMS_events=v_events)
evt_react_list <- gen_react_events(RRMS_events=v_events)

util_ongoing <- "q_default"


cost_ongoing <- "c_default"

#Logic is: per patient, per intervention, per event, react to that event.
results <- run_sim(  
  npats=1000,                               # number of patients to be simulated
  n_sim=1,                                  # number of simulations to run
  psa_bool = FALSE,                         # use PSA or not. If n_sim > 1 and psa_bool = FALSE, then difference in outcomes is due to sampling (number of pats simulated)  
  arm_list = c("int", "noint"),             # intervention list
  common_all_inputs = common_all_inputs,    # inputs common that do not change within a simulation
  common_pt_inputs = common_pt_inputs,      # inputs that change within a simulation but are not affected by the intervention
  unique_pt_inputs = unique_pt_inputs,      # inputs that change within a simulation between interventions
  init_event_list = init_event_list,        # initial event list
  evt_react_list = evt_react_list,          # reaction of events
  util_ongoing_list = util_ongoing,
  cost_ongoing_list = cost_ongoing,
  ipd = 1
)


psa_ipd <- bind_rows(map(results[[1]], "merged_df")) 

psa_ipd[1:10,] %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
