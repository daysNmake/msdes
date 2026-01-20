gen_init_events<-function(RRMS_events){
  #init_event_list <- 
    add_tte(arm=c("noint","int"), evts = RRMS_events ,input={
       sick <- 0
       sicker <- draw_tte(1,dist="exp", coef1=coef_noint, beta_tx = ifelse(arm=="int",HR_int,1), seed = random_seed_sicker_i[i]) #this way the value would be the same if it wasn't for the HR, effectively "cloning" patients luck
      
    })
}




