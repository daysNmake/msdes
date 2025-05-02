## dataset for survival curves
psa_ipd <- bind_rows(map(results$output_psa, "merged_df")) 
death_ipd<- os<- array(NA, dim = c(n_samples, n_treatments, n_patients),
                            dimnames = list(NULL, v_treatment_names, NULL))
dim(death_ipd)
## extract eventtimes  for death by sample and treatment
for (treatment_name in v_treatment_names){
for (i_patient in 1:n_patients){
  for(i_sample in 1:n_samples){
    death_ipd[i_sample, treatment_name, i_patient] <-  psa_ipd$evttime[psa_ipd$pat_id == i_patient & 
                              psa_ipd$simulation == i_sample & 
                              psa_ipd$trt == treatment_name & psa_ipd$evtname == "death"]
   # death_ipd[isample, treatment_name, i_patient] <- psa_ipd$evttime[temp]
  
    }}
}
#head(death_ipd[1,2,])

## Sort death times ascending
for (i_sample in 1:n_samples){
    for(treatment_name in v_treatment_names){
      death_ipd[i_sample, treatment_name,] <- sort(death_ipd[i_sample, treatment_name,])
    }}

dim(death_ipd)
setwd("C:/Users/po22525/University of Bristol/grp-Bristol TAG - MTA-MS - MTA-MS/Working Folder_HE/MS Models/Git Repo/too large for git")

saveRDS(death_ipd, file ="results/death_ipd.RDS")


########
library(survival)
library(flexsurv)
library(survminer)
library(ggplot2)

#loop over treatments, change over to ggplot to get better graphs
for (i_trt in v_treatment_names) {
os <- data.frame(cbind(death_ipd[1,i_trt,], 
            c(rep(1, length(death_ipd[1,i_trt,]))))
            )
colnames(os) <- c("time", "event")
## plot a curve
curve<- survfit(Surv(time, event)~1, data =os)
plot(curve, xlab="age", ylab="proportion alive")
title = ("Survival on Treatment 1" )
}


os <- data.frame(cbind(death_ipd[1,1,], 
                       c(rep(1, length(death_ipd[1,1,]))))
)
colnames(os) <- c("time", "event")
## plot a curve
curve<- survfit(Surv(time, event)~1, data =os, type="kaplan-meier")
curve
#plot1
plot(curve, xlab="age", ylab="proportion alive")
title = ("Survival on Treatment 1" )
#plot2
ggsurvplot(curve, title = "treatment")
#plot3
t<- curve$time
s<-curve$surv
ts<-data.frame(t,s)
ggplot()+
  geom_step(size=1)+
  geom_step(data=ts, aes(t, s, col="Nat"), size=1)+
  labs(title ="Predicted mortality over time models", y = "Survival (probability)", x = "Time (years)") +
  theme(legend.position = "bottom")

#### wide to long
library(tidyr)
library(dplyr)

v1<- v2<- v3<- v4<- v5<- v6<- v7<- v8<- matrix(rep(0, n_patients* n_samples) , nrow = n_patients, ncol =  n_samples)
v1 <- as_tibble(death_ipd[,1,])
v2 <- as_tibble(death_ipd[,2,])
v3 <- as_tibble(death_ipd[,3,])
v4 <- as_tibble(death_ipd[,4,])
v5 <- as_tibble(death_ipd[,5,])
v6 <- as_tibble(death_ipd[,6,])
v7 <- as_tibble(death_ipd[,7,])
v8 <- as_tibble(death_ipd[,8,])

v1 <- v1 %>%
mutate(event = 1) %>%
  pivot_longer(cols=starts_with("V"), values_to = "time")
v2 <- v2 %>%
  mutate(event = 1) %>%
  pivot_longer(cols=starts_with("V"), values_to = "time")
v3 <- v3 %>%
  mutate(event = 1) %>%
  pivot_longer(cols=starts_with("V"), values_to = "time")
v4 <- v4 %>%
  mutate(event = 1) %>%
  pivot_longer(cols=starts_with("V"), values_to = "time")
v5 <- v5 %>%
  mutate(event = 1) %>%
  pivot_longer(cols=starts_with("V"), values_to = "time")
v6 <- v6 %>%
  mutate(event = 1) %>%
  pivot_longer(cols=starts_with("V"), values_to = "time")
v7 <- v7 %>%
  mutate(event = 1) %>%
  pivot_longer(cols=starts_with("V"), values_to = "time")
v8 <- v8 %>%
  mutate(event = 1) %>%
  pivot_longer(cols=starts_with("V"), values_to = "time")

v1 <- as.data.frame(v1[,-2])
v2 <- as.data.frame(v2[,-2])
v3 <- as.data.frame(v3[,-2])
v4 <- as.data.frame(v4[,-2])
v5 <- as.data.frame(v5[,-2])
v6 <- as.data.frame(v6[,-2])
v7 <- as.data.frame(v7[,-2])
v8 <- as.data.frame(v8[,-2])

v1 <- sort_by.data.frame(v1, v1$time)
v2 <- sort_by.data.frame(v2, v2$time)
v3 <- sort_by.data.frame(v3, v3$time)
v4 <- sort_by.data.frame(v4, v4$time)
v5 <- sort_by.data.frame(v5, v5$time)
v6 <- sort_by.data.frame(v6, v6$time)
v7 <- sort_by.data.frame(v7, v7$time)
v8 <- sort_by.data.frame(v8, v8$time)

curve1<- survfit(Surv(time, event)~1, data =v1, type="kaplan-meier")
curve2<- survfit(Surv(time, event)~1, data =v2, type="kaplan-meier")
curve3<- survfit(Surv(time, event)~1, data =v3, type="kaplan-meier")
curve4<- survfit(Surv(time, event)~1, data =v4, type="kaplan-meier")
curve5<- survfit(Surv(time, event)~1, data =v5, type="kaplan-meier")
curve6<- survfit(Surv(time, event)~1, data =v6, type="kaplan-meier")
curve7<- survfit(Surv(time, event)~1, data =v7, type="kaplan-meier")
curve8<- survfit(Surv(time, event)~1, data =v8, type="kaplan-meier")

t1<- curve1$time
t2<- curve2$time
t3<- curve3$time
t4<- curve4$time
t5<- curve5$time
t6<- curve6$time
t7<- curve7$time
t8<- curve8$time

s1<-curve1$surv
s2<-curve2$surv
s3<-curve3$surv
s4<-curve4$surv
s5<-curve5$surv
s6<-curve6$surv
s7<-curve7$surv
s8<-curve8$surv

ts1<-data.frame(t1,s1)
ts2<-data.frame(t2,s2)
ts3<-data.frame(t3,s3)
ts4<-data.frame(t4,s4)
ts5<-data.frame(t5,s5)
ts6<-data.frame(t6,s6)
ts7<-data.frame(t7,s7)
ts8<-data.frame(t8,s8)

ggplot()+
  geom_step(size=1)+
  geom_step(data=ts1, aes(t1, s1, col="Natalizumab-IV"), size=1)+
  geom_step(data=ts2, aes(t2, s2, col="Natalizumab-SC"), size=1)+
  geom_step(data=ts3, aes(t3, s3, col="Natalizumab biosimilar-IV"), size=1)+
  #geom_step(data=ts4, aes(t4, s4, col="Natalizumab biosimilar-SC"), size=1)+
  geom_step(data=ts5, aes(t5, s5, col="Cladribine"), size=1, , linetype ="longdash")+
  geom_step(data=ts6, aes(t6, s6, col="Ublituximab"), size=1)+
  geom_step(data=ts7, aes(t7, s7, col="Ofatumumab"), size=1, , linetype ="dotted")+
  geom_step(data=ts8, aes(t8, s8, col="Ocrelizumab"), size=1, linetype ="dotdash")+
  labs(title ="Predicted mortality over time - HARRMS Model ", y = "Survival (probability)", x = "Time (years)") +
    theme_bw() +
  theme(legend.position = "bottom") 

seq(1:length(v_treatment_names))


  
for (i in 1:8) {
  v[i] <- as_tibble(death_ipd[,i,])
  #colnames(os) <- c("time", "event")
  ## plot a curve
  #curve<- survfit(Surv(time, event)~1, data =os)
  #plot(curve, xlab="age", ylab="proportion alive")
  #title = ("Survival on Treatment 1" )
}
