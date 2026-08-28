### Validation Script can be used if ipd data is saved 
### Ayman Sadek 25-08-2026

#### run after analysis for the purposes of validation of output

##save ipd if not saved
#psa_ipd <- bind_rows(map(results[[1]], "merged_df"))
#saveRDS(psa_ipd, paste0(c("results/ipd warden", n_samples, "x", n_patients, ".csv"), collapse = " "))

##load ipd if saved
psa_ipd<- readRDS("ipd model run 100x100 public prices.RDS")
v_treatment_names <- c(unique(psa_ipd$arm))
n_treatments <- as.numeric(length(unique(psa_ipd$arm)))
n_samples <- as.numeric(length(unique(psa_ipd$simulation)))
n_patients <- as.numeric(length(unique(psa_ipd$pat_id)))

## ipd for dead patients with data on final recorded characteristics
psa_ipd_dead <- psa_ipd[evtname == "death"]

## ipd arrays for key model outputs of interest
n_spms_ipd <- n_relapse_ipd <- n_edssincrease_ipd <- 
  n_edssincrease_ipd <- n_saes_ipd <- final_edss_value_ipd <- array(NA, dim = c(n_samples, n_treatments, n_patients),
                                            dimnames = list(NULL, v_treatment_names, NULL))


for(i_sample in 1:n_samples) {
  for(treatment_name in v_treatment_names) {
    for(i_patient in 1:n_patients) {

      
      n_spms_ipd[i_sample, treatment_name, i_patient] <-
        psa_ipd_dead$spms_flag[psa_ipd_dead$simulation == i_sample &
                                   psa_ipd_dead$arm == treatment_name & 
                                   psa_ipd_dead$pat_id == i_patient]
      
      n_relapse_ipd[i_sample, treatment_name, i_patient] <-
        psa_ipd_dead$n_relapse[psa_ipd_dead$simulation == i_sample &
                                  psa_ipd_dead$arm == treatment_name & 
                                  psa_ipd_dead$pat_id == i_patient]
      
      n_edssincrease_ipd[i_sample, treatment_name, i_patient] <-
        psa_ipd_dead$n_edssinc_evts[psa_ipd_dead$simulation == i_sample &
                                 psa_ipd_dead$arm == treatment_name & 
                                 psa_ipd_dead$pat_id == i_patient]
      
      n_edssincrease_ipd[i_sample, treatment_name, i_patient] <-
        psa_ipd_dead$n_edssdec_evts[psa_ipd_dead$simulation == i_sample &
                                          psa_ipd_dead$arm == treatment_name & 
                                          psa_ipd_dead$pat_id == i_patient]
      
      n_saes_ipd[i_sample, treatment_name, i_patient] <-
        psa_ipd_dead$n_sae_evts[psa_ipd_dead$simulation == i_sample &
                                          psa_ipd_dead$arm == treatment_name & 
                                          psa_ipd_dead$pat_id == i_patient]
      
      final_edss_value_ipd[i_sample, treatment_name, i_patient] <-
        psa_ipd_dead$edss_value[psa_ipd_dead$simulation == i_sample &
                                  psa_ipd_dead$arm == treatment_name & 
                                  psa_ipd_dead$pat_id == i_patient]
        
    } # End loop over patients
    # Summarise for just this sample
    
  }
}

#total events of interest for each simulation
total_n_spms<- apply(n_spms_ipd, c(1, 2), mean)
total_n_relapse<- apply(n_relapse_ipd, c(1, 2), mean)
total_edss_increases<- apply(n_edssincrease_ipd, c(1, 2), mean)
total_edssdecreases<- apply(n_edssincrease_ipd, c(1, 2), mean)
total_saes<- apply(n_saes_ipd, c(1, 2), mean)
total_final_edss_value <- apply(final_edss_value_ipd, c(1, 2), mean)

#name data structures for results 

n_spms_trt <- n_rel_trt<- n_edssincrease_trt<- n_edssdecrease_trt<- n_saes_trt<- n_final_edss_value<- c(rep(0, length(v_treatment_names)))
names(n_rel_trt) <- names(n_edssincrease_trt) <- names(n_edssdecrease_trt) <- names(n_saes_trt) <- names(n_final_edss_value) <- v_treatment_names

# summarise results

for (i  in 1:length(v_treatment_names)) {
  n_spms_trt[i] <- summarise_samples(total_n_spms[,i])
  n_rel_trt[i] <- summarise_samples(total_n_relapse[,i])
  n_edssincrease_trt[i] <- summarise_samples(total_edss_increases[,i])
  n_edssdecrease_trt[i] <- summarise_samples(total_edss_decreases[,i])
  n_saes_trt[i] <- summarise_samples(total_saes[,i])
  n_final_edss_value[i] <- summarise_samples(total_final_edss_value[,i])
}
key_events_trt_grps <- as.data.frame(cbind(n_spms_trt, n_rel_trt, n_edssincrease_trt, n_edssdecrease_trt, n_saes_trt)
)

###compare the results per group if interest

View(key_events_trt_grps)



write.csv(key_events_trt_grps, paste0(c("results/key events warden", n_samples, "x", n_patients, ".csv"), collapse = " "))

### compare by event time
evttime05 <- psa_ipd[psa_ipd$evttime == 5]
evttime10 <- psa_ipd[psa_ipd$evttime == 10]
evttime15 <- psa_ipd[psa_ipd$evttime == 15]

n_spms_yr05_ipd<- n_spms_yr10_ipd<- n_spms_yr15_ipd<-  c(rep(0, length(v_treatment_names)))
names(n_spms_yr05_ipd) <- names(n_spms_yr10_ipd) <- names(n_spms_yr15_ipd) <- v_treatment_names
for (i  in 1:length(v_treatment_names)) {
  n_spms_yr05_ipd[i] <- sum(evttime05$spms_flag[evttime05$arm == names(n_spms_yr05_ipd[i])])
  n_spms_yr10_ipd[i] <- sum(evttime10$spms_flag[evttime10$arm == names(n_spms_yr10_ipd[i])])
  n_spms_yr15_ipd[i] <- sum(evttime15$spms_flag[evttime15$arm == names(n_spms_yr15_ipd[i])])
}

spms_trt_grps <- as.data.frame (cbind(n_spms_yr05_ipd, n_spms_yr10_ipd, n_spms_yr15_ipd))

spms_trt_grps <- spms_trt_grps/(n_patients*n_samples) 
View(spms_trt_grps)
write.csv(spms_trt_grps, paste0(c("results/key events overtime warden", n_samples, "x", n_patients, ".csv"), collapse = " "))
