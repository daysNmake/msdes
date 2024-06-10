# load packages
library(descem)
library(dplyr)
library(tidyr)


# two approaches for modeling the transitions between severity states
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
            edss0 <- draw_tte(1,dist="exp", coef1=) # r edss 0 to 0
            tot.line1 # draw time on tot.line1
            
            #EDSS1-9
            if (f.edss0==1){ 
              
              #draw event times transitioning from edss0 to edss1
              edss1 <- edss0 + draw_tte(1,dist="exp", coef1=) # r edss 0 to 1
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
