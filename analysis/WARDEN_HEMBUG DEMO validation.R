#### run after analysis for the purposes of validation and compare to DESCEM output

### Simulated patients for all simulations
psa_ipd <- bind_rows(map(results[[1]], "merged_df"))
#saveRDS(psa_ipd, paste0(c("results/ipd warden", n_samples, "x", n_patients, ".csv"), collapse = " "))


###compare the results per group if interest
# compare number of relapses, edssincrese, edss decrease and saes across trt groups
dead <- psa_ipd[psa_ipd$evtname == "death"]
n_spms_trt <- n_rel_trt<- n_edssincrease_trt<- n_edssdecrease_trt<- n_saes_trt<- c(rep(0, length(v_treatment_names)))
names(n_rel_trt) <- names(n_edssincrease_trt) <- names(n_edssdecrease_trt) <- names(n_saes_trt) <-v_treatment_names
for (i  in 1:length(v_treatment_names)) {
  n_spms_trt[i] <- summarise_samples(dead$spms_flag[dead$arm == names(n_spms_trt[i])])
  n_rel_trt[i] <- summarise_samples(dead$n_relapse[dead$arm == names(n_rel_trt[i])])
  n_edssincrease_trt[i] <- summarise_samples(dead$n_edssinc_evts[dead$arm == names(n_edssincrease_trt[i])])
  n_edssdecrease_trt[i] <- summarise_samples(dead$n_edssdec_evts[dead$arm == names(n_edssdecrease_trt[i])])
  n_saes_trt[i] <- summarise_samples(dead$n_sae_evts[dead$arm == names(n_saes_trt[i])])
}
key_events_trt_grps <- as.data.frame(cbind(n_spms_trt, n_rel_trt, n_edssincrease_trt, n_edssdecrease_trt, n_saes_trt)
)

#key_events_trt_grps <- key_events_trt_grps/(n_samples+n_patients)
key_events_trt_grps
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
