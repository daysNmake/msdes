
########### EDSS Severity Trajectories by :Ayman Sadek on 02 May 2024############

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

n_cycles <- 10

P_EDSS00 <- Natural_History[1,1]
P_EDSS01 <- Natural_History[1,2]
P_EDSS02 <- Natural_History[1,3]
P_EDSS03 <- Natural_History[1,4]
P_EDSS04 <- Natural_History[1,5]
P_EDSS05 <- Natural_History[1,6]
P_EDSS06 <- Natural_History[1,7]
P_EDSS07 <- Natural_History[1,8]
P_EDSS08 <- Natural_History[1,9]
P_EDSS09 <- Natural_History[1,10]

t_EDSS0 <- t_EDSS1 <- t_EDSS2 <- t_EDSS3 <- t_EDSS4 <- t_EDSS5 <- t_EDSS6 <- t_EDSS7 <- t_EDSS8 <- t_EDSS9 <-1

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

Severity <-  simmer()

set.seed(6369)

## Entry trajectory stet patient global attributes
  Entry_trj <- trajectory() %>% 
   # recording the time of Entry 
  set_attribute(keys = "TimeOfEntry", values = function() now(.env = Severity)) %>% 
  # recording the severity at base line 
  set_global(keys = 'Severity_start', values = -1) %>% #  adding a function that assigns severity based on the distribution of patients not suited to simmer, 
                                                         #  the recommended strategy is to have separate trajectories and matching generators for each type of patient 
  # Ask patients about their attributes  
  log_(function() paste("I entered model& my severity is:", get_global(Severity, 'Severity_start')))
  # plot
  #plot(Entry_trj, verbose = T)

## RRMS EDSS0 to EDSS9 trajectory to be called upon whenever we needed 
  EDSS0_trj<- trajectory("Patients_EDSS0") %>% 
    seize(resource = "EDSS0", tag = "EDSS0") %>% 
    timeout(task = t_EDSS0) %>% 
    set_global('Severity', 0) %>% 
    log_(function() paste("in_EDSS0 & my severity is:", get_global(Severity, 'Severity'))) %>%
    # spend time in EDSS0 add a resource for relapses (parallel?) & renege in the event of mortality and SPMS
    release(resource = "EDSS0") %>%
    rollback("EDSS0", times = n_cycles)
  #plot trajectory
  #plot(EDSS0_trj, verbose = T)

EDSS1_trj<- trajectory("Patients_EDSS1") %>% 
  seize(resource = "EDSS1", tag="EDSS1") %>% 
  timeout(task = t_EDSS1) %>% 
  set_global('Severity', 1) %>% 
  log_(function() paste("in_EDSS1 & my severity is:", get_global(Severity, 'Severity'))) %>%
  # spend time in EDSS1 add a resource for relapses (parallel?) & renege in the event of mortality and SPMS
  release(resource = "EDSS1")%>%
  rollback("EDSS1", times = n_cycles)

#plot(EDSS1_trj, verbose = T)

EDSS2_trj<- trajectory("Patients_EDSS2") %>% 
  seize(resource = "EDSS2", tag="EDSS2") %>% 
  timeout(task = t_EDSS2) %>% 
  set_global('Severity', 2) %>% 
  log_(function() paste("in_EDSS2 & my severity is:", get_global(Severity, 'Severity'))) %>%
  # spend time in EDSS2 add a resource for relapses (parallel?) & renege in the event of mortality and SPMS
  release(resource = "EDSS2")%>%
  rollback("EDSS2", times = n_cycles)
#plot trajectory
#plot(EDSS2_trj, verbose = T)

EDSS3_trj<- trajectory("Patients_EDSS3") %>% 
  seize(resource = "EDSS3", tag= "EDSS3") %>% 
  timeout(task = t_EDSS3) %>% 
  set_global('Severity', 3) %>% 
  log_(function() paste("in_EDSS3 & my severity is:", get_global(Severity, 'Severity'))) %>%
  # spend time in EDSS3 add a resource for relapses (parallel?) & renege in the event of mortality and SPMS
  release(resource = "EDSS3") %>%
  rollback("EDSS3", times = n_cycles)
  #plot trajectory
  #plot(EDSS3_trj, verbose = T)
  
  EDSS4_trj<- trajectory("Patients_EDSS4") %>% 
    seize(resource = "EDSS4", tag= "EDSS4") %>% 
    timeout(task = t_EDSS4) %>% 
    set_global('Severity', 4) %>% 
    log_(function() paste("in_EDSS4 & my severity is:", get_global(Severity, 'Severity'))) %>%
    # spend time in EDSS4 add a resource for relapses (parallel?) & renege in the event of mortality and SPMS
    release(resource = "EDSS4") %>%
    rollback("EDSS4", times = n_cycles)
  #plot trajectory
  #plot(EDSS4_trj, verbose = T)
  
  EDSS5_trj<- trajectory("Patients_EDSS5") %>% 
    seize(resource = "EDSS5", tag= "EDSS5") %>% 
    timeout(task = t_EDSS5) %>% 
    set_global('Severity', 5) %>% 
    log_(function() paste("in_EDSS5 & my severity is:", get_global(Severity, 'Severity'))) %>%
    # spend time in EDSS5 add a resource for relapses (parallel?) & renege in the event of mortality and SPMS
    release(resource = "EDSS5") %>%
    rollback("EDSS5", times = n_cycles)
  #plot trajectory
  #plot(EDSS5_trj, verbose = T)
  
  EDSS6_trj<- trajectory("Patients_EDSS6") %>% 
    seize(resource = "EDSS6", tag= "EDSS6") %>% 
    timeout(task = t_EDSS6) %>% 
    set_global('Severity', 6) %>% 
    log_(function() paste("in_EDSS6 & my severity is:", get_global(Severity, 'Severity'))) %>%
    # spend time in EDSS6 add a resource for relapses (parallel?) & renege in the event of mortality and SPMS
    release(resource = "EDSS6") %>%
    rollback("EDSS6", times = n_cycles)
  #plot trajectory
  #plot(EDSS6_trj, verbose = T)
  
  EDSS7_trj<- trajectory("Patients_EDSS7") %>% 
    seize(resource = "EDSS7", tag= "EDSS7") %>% 
    timeout(task = t_EDSS7) %>% 
    set_global('Severity', 7) %>% 
    log_(function() paste("in_EDSS7 & my severity is:", get_global(Severity, 'Severity'))) %>%
    # spend time in EDSS3 add a resource for relapses (parallel?) & renege in the event of mortality and SPMS
    release(resource = "EDSS3") %>%
    rollback("EDSS7", times = n_cycles)
  #plot trajectory
  #plot(EDSS7_trj, verbose = T)
  
  EDSS8_trj<- trajectory("Patients_EDSS8") %>% 
    seize(resource = "EDSS8", tag= "EDSS8") %>% 
    timeout(task = t_EDSS8) %>% 
    set_global('Severity', 8) %>% 
    log_(function() paste("in_EDSS8 & my severity is:", get_global(Severity, 'Severity'))) %>%
    # spend time in EDSS8 add a resource for relapses (parallel?) & renege in the event of mortality and SPMS
    release(resource = "EDSS8") %>%
    rollback("EDSS8", times = n_cycles)
  #plot trajectory
  #plot(EDSS8_trj, verbose = T)
  
  EDSS9_trj<- trajectory("Patients_EDSS9") %>% 
    seize(resource = "EDSS9", tag= "EDSS9") %>% 
    timeout(task = t_EDSS9) %>% 
    set_global('Severity', 9) %>% 
    log_(function() paste("in_EDSS9 & my severity is:", get_global(Severity, 'Severity'))) %>%
    # spend time in EDSS9 add a resource for relapses (parallel?) & renege in the event of mortality and SPMS
    release(resource = "EDSS9") %>%
    rollback("EDSS9", times = n_cycles)
  #plot trajectory
 # plot(EDSS9_trj, verbose = T)



#### A Long patient Trajectory, incomplete, have to turn this into multiple branches
Patient_trj_long_startin0 <- trajectory() %>% 
  # Entry:
  join(Entry_trj)%>%
  # Branch into EDSS states
  branch(
    function() ifelse(runif(1) < P_EDSS00, 1, 
                      ifelse(runif(1) < P_EDSS01, 2, 1
                      )
    ), 
    continue = c(T,T),
    # what is the best way to determine the probability of events? sample(x=c(0,1), size=1, replace=T,prob=c(0.65,0.2))
    join(EDSS0_trj), 
    join(EDSS1_trj)  %>% 
      
      branch(
        function() ifelse(runif(1) < P_EDSS02, 3,
                          ifelse(runif(1) < P_EDSS03, 4, 3
                          )), 
        continue = c(T,T),
        # what is the best way to determine the probability of events? sample(x=c(0,1), size=1, replace=T,prob=c(0.65,0.2))
        join(EDSS3_trj) , 
        join(EDSS4_trj) %>%
          
          branch(
            function() ifelse(runif(1) < P_EDSS04, 5,
                              ifelse(runif(1) < P_EDSS05, 6, 5
                              )
            ), 
            continue = c(T,T),
            # what is the best way to determine the probability of events? sample(x=c(0,1), size=1, replace=T,prob=c(0.65,0.2))
            
            join(EDSS5_trj) , 
            join(EDSS6_trj) %>%
              
              branch(
                function() ifelse(runif(1) < P_EDSS06, 7,
                                  ifelse(runif(1) < P_EDSS07, 8, 7
                                  ) 
                ), 
                continue = c(T,T),
                # what is the best way to determine the probability of events? sample(x=c(0,1), size=1, replace=T,prob=c(0.65,0.2))
                join(EDSS7_trj) , 
                join(EDSS8_trj) %>%
                  
                  
                  branch(
                    function() if(runif(1) < P_EDSS08) {9} else {0} , 
                    continue = c(T),
                    # what is the best way to determine the probability of events? sample(x=c(0,1), size=1, replace=T,prob=c(0.65,0.2))
                    join(EDSS9_trj) %>%
                    
                      log_(function() {paste("Im out my severity is:", get_global(Severity, 'Severity'))})
                  )
              )
          )
      )
  )


plot(Patient_trj_long_startin0 , verbose = T)
plong <- plot(Patient_trj_long_startin0 , verbose = T)
svg <- charToRaw(export_svg(plong))
rsvg_png(svg, "out_long.png", width=2000)



# 5. run the sim and monitor

Severity %>% 
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
    add_generator("startin0_Patient_", Patient_trj_long_startin0, from_to(0, 100, function() {rexp(3,1/0.5)}), mon=2) %>% 
  run()

#get_n_generated(Severity)
get_mon_arrivals(Severity)%>%
  transform(waiting_time = end_time - start_time - activity_time)
get_mon_resources(Severity)
get_mon_attributes(Severity)




#### A wide patient Trajectory, if wanted  to use this would have to add a long trajectory to each severity state (size 9-n)
Patient_trj_wide_startin0 <- trajectory() %>% 
  
  # Entry:
  join(Entry_trj)%>%
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
    join(EDSS0_trj), 
    join(EDSS1_trj) , 
    join(EDSS2_trj) , 
    join(EDSS3_trj) , 
    join(EDSS4_trj) , 
    join(EDSS5_trj) , 
    join(EDSS6_trj) , 
    join(EDSS7_trj) , 
    join(EDSS8_trj) ,
    join(EDSS9_trj) %>% 
      log_(function() {paste("outof_00 & 01 & my severity is:", get_global(Severity, 'Severity'))})
  )

#plot(Patient_trj_startin0, verbose = T)
pwide <- plot(Patient_trj_wide_startin0, verbose = T)
svg <- charToRaw(export_svg(pwide))
rsvg_png(svg, "out_wide.png", width=2000)
