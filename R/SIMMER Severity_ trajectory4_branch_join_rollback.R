
########### EDSS severity Trajectories by :Ayman Sadek on 02 May 2024############

##Issues
# patients not moving through the Vertical trajectory; 
# possible fix is to move the rollback out of the sub trajectories and place in between the branches

## 1. INITIALIZATION ----

#install.packages(pkgs = c("simmer", "simmer.plot", "flexsurv", "data.table"))

# Loading the packages
library(simmer)           
library(simmer.plot)      
library(DiagrammeRsvg)
library(rsvg) 


## 2. PARAMETERS ----
NH <- readRDS("NH_RRMS_BC_TA616")

n_cycles <- 10

P_EDSS00 <- NH[1,1]
P_EDSS01 <- NH[1,2]
P_EDSS02 <- NH[1,3]
P_EDSS03 <- NH[1,4]
P_EDSS04 <- NH[1,5]
P_EDSS05 <- NH[1,6]
P_EDSS06 <- NH[1,7]
P_EDSS07 <- NH[1,8]
P_EDSS08 <- NH[1,9]
P_EDSS09 <- NH[1,10]

T_EDSS0 <- T_EDSS1 <- T_EDSS2 <- T_EDSS3 <- T_EDSS4 <- T_EDSS5 <- T_EDSS6 <- T_EDSS7 <- T_EDSS8 <- T_EDSS9 <-1

## 3. FUNCTIONS ----
# Function to determine whether the individual is eligible to continue for ..... at the
# time of the intake, with the following output value that is used to determine the sub-
# trajectory in the corresponding branch:
#   0) continue to testing (0 = skip the branch)
#   1) not eligible for testing (1 = enter the first sub-trajectory in the branch)
   



## 4. TRAJECTORY ----

# This section implements the trajectory/model structure through which individuals will be
# simulated. In summary, the pathway for the individuals is as follows:
# Entry into the Model & set baseline severity values

severity <-  simmer()

set.seed(6369)

## Entry trajectory stet patient global attributes
  entry_trj <- trajectory() %>% 
   # recording the time of Entry 
  set_attribute(keys = "TimeOfEntry", values = function() now(.env = severity)) %>% 
  # recording the severity at base line 
  set_global(keys = 'severity_start', values = -1) %>% #  adding a function that assigns severity based on the distribution of patients not suited to simmer, 
                                                         #  the recommended strategy is to have separate trajectories and matching generators for each type of patient 
  # Ask patients about their attributes  
  log_(function() paste("I entered model& my severity is:", get_global(severity, 'severity_start')))
  # plot
  #plot(entry_trj, verbose = T)

## RRMS EDSS0 to EDSS9 trajectory to be called upon whenever we needed 
  edss0_trj<- trajectory("Patients_EDSS0") %>% 
    seize(resource = "EDSS0", tag = "EDSS0") %>% 
    timeout(task = T_EDSS0) %>% 
    set_global('severity', 0) %>% 
    log_(function() paste("in_EDSS0 & my severity is:", get_global(severity, 'severity'))) %>%
    # spend time in EDSS0 add a resource for relapses (parallel?) & renege in the event of mortality and SPMS
    release(resource = "EDSS0") %>%
    rollback("EDSS0", times = n_cycles)
  #plot trajectory
  #plot(edss0_trj, verbose = T)

edss1_trj<- trajectory("Patients_EDSS1") %>% 
  seize(resource = "EDSS1", tag="EDSS1") %>% 
  timeout(task = T_EDSS1) %>% 
  set_global('severity', 1) %>% 
  log_(function() paste("in_EDSS1 & my severity is:", get_global(severity, 'severity'))) %>%
  # spend time in EDSS1 add a resource for relapses (parallel?) & renege in the event of mortality and SPMS
  release(resource = "EDSS1")%>%
  rollback("EDSS1", times = n_cycles)

#plot(edss1_trj, verbose = T)

edss2_trj<- trajectory("Patients_EDSS2") %>% 
  seize(resource = "EDSS2", tag="EDSS2") %>% 
  timeout(task = T_EDSS2) %>% 
  set_global('severity', 2) %>% 
  log_(function() paste("in_EDSS2 & my severity is:", get_global(severity, 'severity'))) %>%
  # spend time in EDSS2 add a resource for relapses (parallel?) & renege in the event of mortality and SPMS
  release(resource = "EDSS2")%>%
  rollback("EDSS2", times = n_cycles)
#plot trajectory
#plot(edss2_trj, verbose = T)

edss3_trj<- trajectory("Patients_EDSS3") %>% 
  seize(resource = "EDSS3", tag= "EDSS3") %>% 
  timeout(task = T_EDSS3) %>% 
  set_global('severity', 3) %>% 
  log_(function() paste("in_EDSS3 & my severity is:", get_global(severity, 'severity'))) %>%
  # spend time in EDSS3 add a resource for relapses (parallel?) & renege in the event of mortality and SPMS
  release(resource = "EDSS3") %>%
  rollback("EDSS3", times = n_cycles)
  #plot trajectory
  #plot(edss3_trj, verbose = T)
  
  edss4_trj<- trajectory("Patients_EDSS4") %>% 
    seize(resource = "EDSS4", tag= "EDSS4") %>% 
    timeout(task = T_EDSS4) %>% 
    set_global('severity', 4) %>% 
    log_(function() paste("in_EDSS4 & my severity is:", get_global(severity, 'severity'))) %>%
    # spend time in EDSS4 add a resource for relapses (parallel?) & renege in the event of mortality and SPMS
    release(resource = "EDSS4") %>%
    rollback("EDSS4", times = n_cycles)
  #plot trajectory
  #plot(edss4_trj, verbose = T)
  
  edss5_trj<- trajectory("Patients_EDSS5") %>% 
    seize(resource = "EDSS5", tag= "EDSS5") %>% 
    timeout(task = T_EDSS5) %>% 
    set_global('severity', 5) %>% 
    log_(function() paste("in_EDSS5 & my severity is:", get_global(severity, 'severity'))) %>%
    # spend time in EDSS5 add a resource for relapses (parallel?) & renege in the event of mortality and SPMS
    release(resource = "EDSS5") %>%
    rollback("EDSS5", times = n_cycles)
  #plot trajectory
  #plot(edss5_trj, verbose = T)
  
  edss6_trj<- trajectory("Patients_EDSS6") %>% 
    seize(resource = "EDSS6", tag= "EDSS6") %>% 
    timeout(task = T_EDSS6) %>% 
    set_global('severity', 6) %>% 
    log_(function() paste("in_EDSS6 & my severity is:", get_global(severity, 'severity'))) %>%
    # spend time in EDSS6 add a resource for relapses (parallel?) & renege in the event of mortality and SPMS
    release(resource = "EDSS6") %>%
    rollback("EDSS6", times = n_cycles)
  #plot trajectory
  #plot(edss6_trj, verbose = T)
  
  edss7_trj<- trajectory("Patients_EDSS7") %>% 
    seize(resource = "EDSS7", tag= "EDSS7") %>% 
    timeout(task = T_EDSS7) %>% 
    set_global('severity', 7) %>% 
    log_(function() paste("in_EDSS7 & my severity is:", get_global(severity, 'severity'))) %>%
    # spend time in EDSS3 add a resource for relapses (parallel?) & renege in the event of mortality and SPMS
    release(resource = "EDSS3") %>%
    rollback("EDSS7", times = n_cycles)
  #plot trajectory
  #plot(edss7_trj, verbose = T)
  
  edss8_trj<- trajectory("Patients_EDSS8") %>% 
    seize(resource = "EDSS8", tag= "EDSS8") %>% 
    timeout(task = T_EDSS8) %>% 
    set_global('severity', 8) %>% 
    log_(function() paste("in_EDSS8 & my severity is:", get_global(severity, 'severity'))) %>%
    # spend time in EDSS8 add a resource for relapses (parallel?) & renege in the event of mortality and SPMS
    release(resource = "EDSS8") %>%
    rollback("EDSS8", times = n_cycles)
  #plot trajectory
  #plot(edss8_trj, verbose = T)
  
  edss9_trj<- trajectory("Patients_EDSS9") %>% 
    seize(resource = "EDSS9", tag= "EDSS9") %>% 
    timeout(task = T_EDSS9) %>% 
    set_global('severity', 9) %>% 
    log_(function() paste("in_EDSS9 & my severity is:", get_global(severity, 'severity'))) %>%
    # spend time in EDSS9 add a resource for relapses (parallel?) & renege in the event of mortality and SPMS
    release(resource = "EDSS9") %>%
    rollback("EDSS9", times = n_cycles)
  #plot trajectory
 # plot(edss9_trj, verbose = T)



#### A Long patient Trajectory, incomplete, have to turn this into multiple branches
patient_trj_long_startin0 <- trajectory() %>% 
  # Entry:
  join(entry_trj)%>%
  # Branch into EDSS states
  branch(
    function() ifelse(runif(1) < P_EDSS00, 1, 
                      ifelse(runif(1) < P_EDSS01, 2, 1
                      )
    ), 
    continue = c(T,T),
    # what is the best way to determine the probability of events? sample(x=c(0,1), size=1, replace=T,prob=c(0.65,0.2))
    join(edss0_trj), 
    join(edss1_trj)  %>% 
      
      branch(
        function() ifelse(runif(1) < P_EDSS02, 3,
                          ifelse(runif(1) < P_EDSS03, 4, 3
                          )), 
        continue = c(T,T),
        # what is the best way to determine the probability of events? sample(x=c(0,1), size=1, replace=T,prob=c(0.65,0.2))
        join(edss3_trj) , 
        join(edss4_trj) %>%
          
          branch(
            function() ifelse(runif(1) < P_EDSS04, 5,
                              ifelse(runif(1) < P_EDSS05, 6, 5
                              )
            ), 
            continue = c(T,T),
            # what is the best way to determine the probability of events? sample(x=c(0,1), size=1, replace=T,prob=c(0.65,0.2))
            
            join(edss5_trj) , 
            join(edss6_trj) %>%
              
              branch(
                function() ifelse(runif(1) < P_EDSS06, 7,
                                  ifelse(runif(1) < P_EDSS07, 8, 7
                                  ) 
                ), 
                continue = c(T,T),
                # what is the best way to determine the probability of events? sample(x=c(0,1), size=1, replace=T,prob=c(0.65,0.2))
                join(edss7_trj) , 
                join(edss8_trj) %>%
                  
                  
                  branch(
                    function() if(runif(1) < P_EDSS08) {9} else {0} , 
                    continue = c(T),
                    # what is the best way to determine the probability of events? sample(x=c(0,1), size=1, replace=T,prob=c(0.65,0.2))
                    join(edss9_trj) %>%
                    
                      log_(function() {paste("Im out my severity is:", get_global(severity, 'severity'))})
                  )
              )
          )
      )
  )


plot(patient_trj_long_startin0 , verbose = T)
plong <- plot(patient_trj_long_startin0 , verbose = T)
svg <- charToRaw(export_svg(plong))
rsvg_png(svg, "out_long.png", width=2000)



# 5. run the sim and monitor

severity %>% 
    add_resource("EDSS0",capacity = Inf) %>% 
    add_resource("EDSS1",capacity = Inf) %>% 
    add_resource("EDSS2",capacity = Inf) %>%
    add_resource("EDSS3",capacity = Inf) %>%
    add_resource("EDSS4",capacity = Inf) %>% 
    add_resource("EDSS5",capacity = Inf) %>% 
    add_resource("EDSS6",capacity = Inf) %>%
    add_resource("EDSS7",capacity = Inf) %>%
    add_resource("EDSS8",capacity = Inf) %>% 
    add_resource("EDSS9",capacity = Inf) %>% 
    add_generator("startin0_patient_", patient_trj_long_startin0, from_to(0, 100, function() {rexp(3,1/0.5)}), mon=2) %>% 
  run()

#get_n_generated(severity)
get_mon_arrivals(severity)%>%
  transform(waiting_time = end_time - start_time - activity_time)
get_mon_resources(severity)
get_mon_attributes(severity)




#### A wide patient Trajectory, if wanted  to use this would have to add a long trajectory to each severity state (size 9-n)
patient_trj_wide_startin0 <- trajectory() %>% 
  
  # Entry:
  join(entry_trj)%>%
  # Branch into EDSS states
  
  branch(
    function() ifelse(runif(1) < P_EDSS00, 1, 
                      ifelse(runif(1) < P_EDSS01, 2,
                             ifelse(runif(1) < P_EDSS02, 3,
                                    ifelse(runif(1) < P_EDSS03, 4,
                                           ifelse(runif(1) < P_EDSS04, 5,
                                                  ifelse(runif(1) < P_EDSS05, 6,
                                                         ifelse(runif(1) < P_EDSS06, 7,
                                                                ifelse(runif(1) < P_EDSS07, 8,
                                                                       ifelse(runif(1) < P_EDSS08, 9,
                                                                              ifelse(runif(1) < P_EDSS09, 10,9
                                                                              )
                                                                       )
                                                                ) 
                                                         )
                                                  )
                                           )
                                    )
                             )
                      )
    ), 
    continue = c(T,T,T,T,T,T,T,T,T,T),
    # what is the best way to determine the probability of events? sample(x=c(0,1), size=1, replace=T,prob=c(0.65,0.2))
    join(edss0_trj), 
    join(edss1_trj) , 
    join(edss2_trj) , 
    join(edss3_trj) , 
    join(edss4_trj) , 
    join(edss5_trj) , 
    join(edss6_trj) , 
    join(edss7_trj) , 
    join(edss8_trj) ,
    join(edss9_trj) %>% 
      log_(function() {paste("outof_00 & 01 & my severity is:", get_global(severity, 'severity'))})
  )

#plot(patient_trj_wide_startin0, verbose = T)
pwide <- plot(patient_trj_wide_startin0, verbose = T)
svg <- charToRaw(export_svg(pwide))
rsvg_png(svg, "out_wide.png", width=2000)
