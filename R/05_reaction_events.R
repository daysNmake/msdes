
evt_react_list <-
  add_reactevt(name_evt = "initialise",
               input = {
                 # This ensures that the current treatment is the initial treatment
                 # of this strategy.
                 modify_item(list(current_treatment = trt,
                                  current_treatment_code = treatment_codes[trt],
                                  #future death events do not exceed 100 years
                                  "death"= min(cur_evtlist[["death"]], time_horizon + initial_age)
                 ))
               }) %>%
  add_reactevt(name_evt = "age",
               input = {
                 modify_item(list(age = initial_age + round(curtime)
                 ))
               }) %>%
  add_reactevt(name_evt = "relapse",
               input = {
                 modify_item(list(n_relapse= n_relapse + 1,
                                  age = initial_age + round(curtime)))
                 # Currenly no impact on costs or utilities of relapse
                 # Just need to simulate the next time to relapse
                 new_event(list(
                   "relapse" = curtime + draw_tte(
                     1, 
                     dist = "exp", 
                     coef1 = (1 - spms_flag) * (v_baseline_harrms_arr["(Intercept)"] +
                                                  v_baseline_harrms_arr["BaselineWebEDSS"]*(edss_value+1)  +
                                                  v_baseline_harrms_arr["DMTNatalizumab"])+
                       spms_flag * (v_baseline_spms_arr["rate"] +
                                      v_baseline_spms_arr["BaselineWebEDSS"]*(edss_value+1)),
                     hr = ((1 - spms_flag) * v_hr_arr[current_treatment_code]) + spms_flag
                   )
                 ))
               }) %>%
  add_reactevt(name_evt = "edss_increase",
               input = {
                 modify_item(list(#edss.i = edss_value,
                   edss_value = min(edss_value + 1, max(v_edss_values)),
                   age = initial_age + round(curtime)))
                 
                 # Need to simulate the next time to EDSS increase
                 if (edss_value < 9) {new_event(list(
                   "edss_increase" = curtime + draw_tte(
                     1, 
                     dist = "exp",
                     coef1 = (1 - spms_flag) * (v_baseline_harrms_edss_increase["rate"] +
                                                  v_baseline_harrms_edss_increase["BaselineWebEDSS"]*(edss_value+1) ) + 
                       v_baseline_harrms_edss_increase["DMTNatalizumab"] +
                       spms_flag * (v_spms_edss_increase["rate"] +
                                      v_spms_edss_increase["BaselineWebEDSS"]*(edss_value+1)),  
                     hr = ((1 - spms_flag) * v_hr_edss_increase[current_treatment_code])+ spms_flag 
                   )
                   
                 ))}
               }) %>%
  add_reactevt(name_evt = "edss_decrease",
               input = {
                 modify_item(list(#edss.i = edss_value,
                   edss_value = max(edss_value - 1, min(v_edss_values)),
                   age = initial_age + round(curtime)))
                 
                 # Need to simulate the next time to EDSS decrease
                 if (edss_value > 0) {new_event(list(
                   "edss_decrease" = curtime + draw_tte(
                     1, 
                     dist = "exp",
                     # Below is zero if spms_flag is 1 since patients should have EDSS decrase if SPMS
                     coef1 = (1 - spms_flag) * (v_rrms_edss_decrease["rate"] +
                                                  v_rrms_edss_decrease["BaselineWebEDSS"]*(edss_value+1) )  + 
                       spms_flag * (v_spms_edss_decrease["rate"]  +
                                      v_spms_edss_decrease["BaselineWebEDSS"]*(edss_value+1) ) 
                   )
                 ) )}
               }) %>%
  add_reactevt(name_evt = "progression",
               # Progression can only occur once so just change the SPMS status
               input = {
                 modify_item(list(spms_flag = 1,
                                  age = initial_age + round(curtime)))
               }) %>%
  add_reactevt(name_evt = "death",
               # Death only occurs once.
               # Set current time to Inf so simulation ends.
               input = {
                 modify_item(list(curtime = Inf #,
                                  #age = initial_age + round(curtime)
                 )) 
               }) %>%
  add_reactevt(name_evt = "serious_adverse_event",
               #input = {               }
               # Sample next discontinuation event. T
               new_event(list(
                 "serious_adverse_event" = curtime + draw_tte(
                   1,
                   dist = "exp",
                   # SAE rates same for all treatments
                   # change to include HR from safety NMA
                   coef1 = log(mean(v_r_sae))
                 )
               ))
  ) %>%
  # Currently assumes patients switch only twice so 4th line is final (As in protocol)
  add_reactevt(name_evt = "treatment_switch",
               input = {
                 modify_item(list(current_treatment = sample(x = v_higher_line_treatments,
                                                             size = 1),
                                  current_treatment_code = sample(x = v_higher_line_treatment_codes,
                                                                  size = 1),
                                  treatment_line = treatment_line + 1,
                                  v_discontinuation_rate = (treatment_line < 4) *  v_discontinuation_rate,
                                  n_switch = n_switch +1,
                                  age = initial_age + round(curtime)))
                 
                 # Sample next discontinuation event. This will be zero if treatment_line >=4
                 new_event(list(
                   "treatment_switch" = curtime + draw_tte(
                     1,
                     dist = "exp",
                     coef1 = log(v_discontinuation_rate[current_treatment_code])
                   )
                 ))
               })

# Add ongoing utilities
# Can also add util_instant to include disutilities of events (e.g., of relapse or adverse events)
util_ongoing <- add_util(evt = v_event_names,
                         trt = v_treatment_names,
                         util = (1 - spms_flag) * u_rrms_edss[edss_value == v_edss_values] +
                           spms_flag * u_rrms_edss[edss_value == v_edss_values]
)

# Add ongoing costs
# Can also add instaneous costs with cost_instant (For example cost of managing relapse)
cost_ongoing <- add_cost(evt = v_event_names,
                         trt = v_treatment_names,
                         cost = (1 - spms_flag) * (
                           v_c_rrms_edss[edss_value == v_edss_values] #+
                           #treatment_costs[current_treatment = trt] treatment costs should be instantaneous not ongoing
                         ) +
                           spms_flag * v_c_rrms_edss[edss_value == v_edss_values]
)

##Add instant costs

# Treatment Costs right now using an average but need to figure out how to manipulate the costs according to year 1 or 2+ of treatment
cost_instant <-
  add_cost(evt = c("initial", "treatment_switch") ,
           trt = v_treatment_names,
           cost = df_c_tx[current_treatment_code] +
             v_c_trt_admin[current_treatment_code] +
             v_c_trt_monitor[current_treatment_code]) %>% 
  add_cost(evt = "relapse" ,
           trt = v_treatment_names,
           cost = v_c_relapsing[4]) %>% 
  add_cost(evt = "serious_adverse_event" ,
           trt = v_treatment_names,
           cost = sum(v_c_adverse_events*v_weight_sae)) 

# Add instant utilities
# Can also add util_instant to include disutilities of events (e.g., of relapse or adverse events)
util_instant <-   add_util(evt = "relapse" ,
                           trt = v_treatment_names,
                           util = u_relapsing) %>% 
  add_util(evt = "serious_adverse_event" ,
           trt = v_treatment_names,
           util = sum(v_u_adverse_events*v_weight_sae)) 

