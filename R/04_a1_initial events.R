# load packages
library(descem)
library(dplyr)
library(tidyr)


# one of two approaches for modeling the transitions between severity states
#### Add initial events; two groups intervention and no intervention

init_event_list <- 
  add_tte(trt="int",
          evts = c("start", "edss0", "edss1","edss2","edss3","edss4", "edss5","edss6","edss7", "edss8","edss9",
                   "tot.line1"),
          other_inp = c("mort.trt",), #generate an intermediate input which is not an event but that we want to save
          input={ #intervention
            #start
            start <- 0
            
            #mortality is a common patient input, could be setup for every patent here?
            
            # draw time to EDSS0
            edss0 <- draw_tte(1,dist="exp", coef1=) # r edss 0 to 0
            tot.line1 # draw time on tot.line1
            
            #if EDSS0 and alive
            if (f.edss0==1 & mort.trt>edss0){ 
              
              #draw event times transitioning to edss1
              edss1 <- edss0 + draw_tte(1,dist="exp", coef1=) # r edss 0 to 1
              #edss1 <-  r edss 1 to 1?
                
                  } 
                  #if EDSS1 and alive
                  else if(f.edss1==1 & mort.trt>edss1) {
                  #draw event times transitioning to edss2
                  #edss2 <- r edss 0 to 2?
                  edss2 <- edss0 +  edss1 + draw_tte(1,dist="exp", coef1=)# r edss 1 to 2
                  #edss2 <-  r edss 2 to 2?
                    
                      } 
                      #if EDSS2 and alive
                      else if (f.edss2==1 & mort.trt>edss3) {
                      #draw event times transitioning to edss3
                      #edss3 <- r edss 0 to 3?
                      #edss3 <- r edss 1 to 3?
                      edss3 <- edss0 +  edss1 + edss2 + draw_tte(1,dist="exp", coef1=) # r edss 2 to 3
                      
                          } 
                          #if EDSS3 and alive
                          else if (f.edss3==1 & mort.trt>edss3) {
                          #draw event times transitioning to edss4
                          #edss4 <- r edss 0 to 4?
                          #edss4 <- r edss 1 to 4?
                          #edss4 <- r edss 2 to 4?
                          edss4 <- edss0 +  edss1 + edss2 + edss3 + draw_tte(1,dist="exp", coef1=) # r edss 3 to 4
                          
                              } 
                              #if EDSS4 and alive
                              else if (f.edss4==1 & mort.trt>edss4) {
                              #draw event times transitioning to edss5
                              #edss5 <- r edss 0 to 5?
                              #edss5 <- r edss 1 to 5?
                              #edss5 <- r edss 2 to 5?
                              #edss5 <- r edss 3 to 5?
                              edss5 <- edss0 +  edss1 + edss2 + edss3 + edss4 + draw_tte(1,dist="exp", coef1=) # r edss 4 to 5
                              
                              } 
                              #if EDSS5 and alive 
                              else if (f.edss5==1 & mort.trt>edss5) {
                              #draw event times transitioning to edss6
                              #edss6 <- r edss 0 to 6?
                              #edss6 <- r edss 1 to 6?
                              #edss6 <- r edss 2 to 6?
                              #edss6 <- r edss 3 to 6?
                              #edss6 <- r edss 4 to 6?
                              #edss6 <- r edss 5 to 6?
                              edss6 <- edss0 +  edss1 + edss2 + edss3 + edss4 + edss5 + draw_tte(1,dist="exp", coef1=) #r edss 5 to 6
                              
                      } 
                      #if EDSS6 and alive 
                      else if (f.edss6==1 & mort.trt>edss6) {
                      #draw event times transitioning to edss7
                      #edss7 <- r edss 0 to 7?
                      #edss7 <- r edss 1 to 7?
                      #edss7 <- r edss 2 to 7?
                      #edss7 <- r edss 3 to 7?
                      #edss7 <- r edss 4 to 7?
                      #edss7 <- r edss 5 to 7?
                      edss7 <- edss0 +  edss1 + edss2 + edss3 + edss4 + edss5 + edss6 + draw_tte(1,dist="exp", coef1=) #r edss 6 to 7
                      
                  } 
                  #if EDSS7 and alive 
                  else if (f.edss7==1 & mort.trt>edss7) {
                  #draw event times transitioning to edss7
                  #edss8 <- r edss 0 to 8?
                  #edss8 <- r edss 1 to 8?
                  #edss8 <- r edss 2 to 8?
                  #edss8 <- r edss 3 to 8?
                  #edss8 <- r edss 4 to 8?
                  #edss8 <- r edss 5 to 8?
                  #edss8 <- r edss 6 to 8?
                  edss8 <- edss0 +  edss1 + edss2 + edss3 + edss4 + edss5 + edss6 + edss7 + draw_tte(1,dist="exp", coef1=) #r edss7 to 8
                  
            } 
            #if EDSS8 and alive  
            else if (f.edss8==1 & mort.trt>edss8) {
            #draw event times transitioning to edss9
            #edss9 <- r edss 0 to 9?
            #edss9 <- r edss 1 to 9?
            #edss9 <- r edss 2 to 9?
            #edss9 <- r edss 3 to 9?
            #edss9 <- r edss 4 to 9?
            #edss9 <- r edss 5 to 9?
            #edss9 <- r edss 6 to 9?
            #edss9 <- r edss 7 to 9?
            edss9 <- edss0 +  edss1 + edss2 + edss3 + edss4 + edss5 + edss6 + edss7 + edss8 + draw_tte(1,dist="exp", coef1=) #r edss 8 to 9
            
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
