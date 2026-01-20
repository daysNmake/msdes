gen_react_events <- function(RRMS_events){
  
  add_reactevt(name_evt = RRMS_events[1],
               input = {}) %>%
    add_reactevt(name_evt = RRMS_events[2],
                 input = {
                   q_default <- util.sicker
                   c_default <- cost.sicker + if(arm=="int"){cost.int}else{0}
                   fl.sick   <- 0 
                 }) %>%
    add_reactevt(name_evt = RRMS_events[3],
                 input = {
                   q_default <- 0
                   c_default <- 0
                   curtime   <- Inf
                 }) 
  
  
  
}