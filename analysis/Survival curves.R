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


########
library(survival)
library(flexsurv)

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

  

