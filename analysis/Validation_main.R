### Validation script can be called after Warden model Run 
### Ayman Sadek 25-08-2026


### ipd arrays for key model outputs of interest at years  5, 10 & 15
### I could not replicate this analysis by conditioning on event times as it is missing some entries (ex 757/800) so throws up an error
### crude solution is to average for each drug over all patients and simulations combined (i.e not in aloop)

# move ipd into data table (default)
# results[[1]] works for WARDEN to run  DESCEM replace with results$output_psa
psa_ipd <- bind_rows(map(results[[1]], "merged_df"))

# create ipd by event time

evttime05_ipd <- psa_ipd[psa_ipd$evttime == 5]
evttime10_ipd <- psa_ipd[psa_ipd$evttime == 10]
evttime15_ipd <- psa_ipd[psa_ipd$evttime == 15]

# outcomes of interst
p_spms_yr05 <- p_spms_yr10 <- p_spms_yr15 <-
n_rel_yr05<- n_rel_yr10 <- n_rel_yr15 <-  
n_edssincrease_yr05<- n_edssincrease_yr10<- n_edssincrease_yr15 <- 
n_edssdecrease_yr05<- n_edssdecrease_yr10<- n_edssdecrease_yr15 <-   
n_saes_yr05 <- n_saes_yr10 <- n_saes_yr15 <- 
average_edss_value_yr05 <-  average_edss_value_yr10 <- average_edss_value_yr15 <- 
c(rep(0, length(v_treatment_names)))

  
names(p_spms_yr05) <- names(p_spms_yr10) <- names(p_spms_yr15) <- 
names(n_rel_yr05) <-  names(n_rel_yr10) <- names(n_rel_yr15) <-
names(n_edssincrease_yr05) <- names(n_edssincrease_yr10) <- names(n_edssincrease_yr15) <-  
names(n_edssdecrease_yr05) <- names(n_edssdecrease_yr10) <- names(n_edssdecrease_yr15) <- 
names(n_saes_yr05) <- names(n_saes_yr10) <- names(n_saes_yr15) <-
names(average_edss_value_yr05)  <- names(average_edss_value_yr10) <- names(average_edss_value_yr15)<-
v_treatment_names



# summarise ipd for outcomes, years 05, 10, 15
#_ipd$arm works for WARDEN to run with DESCEM replace with _ipd$trt

for (i  in 1:length(v_treatment_names)) {
  p_spms_yr05[i] <- summarise_samples(evttime05_ipd$spms_flag[evttime05_ipd$arm == names(p_spms_yr05[i])])
  p_spms_yr10[i] <- summarise_samples(evttime10_ipd$spms_flag[evttime10_ipd$arm == names(p_spms_yr10[i])])
  p_spms_yr15[i] <- summarise_samples(evttime15_ipd$spms_flag[evttime15_ipd$arm == names(p_spms_yr15[i])])

  n_rel_yr05[i] <- summarise_samples(evttime05_ipd$n_relapse[evttime05_ipd$arm == names(n_rel_yr05[i])])
  n_rel_yr10[i] <- summarise_samples(evttime10_ipd$n_relapse[evttime10_ipd$arm == names(n_rel_yr10[i])])
  n_rel_yr15[i] <- summarise_samples(evttime15_ipd$n_relapse[evttime15_ipd$arm == names(n_rel_yr15[i])])

  n_edssincrease_yr05[i] <- summarise_samples(evttime05_ipd$n_edssinc_evts[evttime05_ipd$arm == names(n_edssincrease_yr05[i])])
  n_edssincrease_yr10[i] <- summarise_samples(evttime10_ipd$n_edssinc_evts[evttime10_ipd$arm == names(n_edssincrease_yr10[i])])
  n_edssincrease_yr15[i] <- summarise_samples(evttime15_ipd$n_edssinc_evts[evttime15_ipd$arm == names(n_edssincrease_yr15[i])])

  n_edssdecrease_yr05[i] <- summarise_samples(evttime05_ipd$n_edssdec_evts[evttime05_ipd$arm == names(n_edssincrease_yr05[i])])
  n_edssdecrease_yr10[i] <- summarise_samples(evttime10_ipd$n_edssdec_evts[evttime10_ipd$arm == names(n_edssincrease_yr10[i])])
  n_edssdecrease_yr15[i] <- summarise_samples(evttime15_ipd$n_edssdec_evts[evttime15_ipd$arm == names(n_edssincrease_yr15[i])])
  
  n_saes_yr05[i] <- summarise_samples(evttime05_ipd$n_sae_evts[evttime05_ipd$arm == names(n_saes_yr05[i])])
  n_saes_yr10[i] <- summarise_samples(evttime10_ipd$n_sae_evts[evttime10_ipd$arm == names(n_saes_yr10[i])])
  n_saes_yr15[i] <- summarise_samples(evttime15_ipd$n_sae_evts[evttime15_ipd$arm == names(n_saes_yr15[i])])
  
  average_edss_value_yr05[i] <- summarise_samples(evttime05_ipd$edss_value[evttime05_ipd$arm == names(average_edss_value_yr05[i])])
  average_edss_value_yr10[i] <- summarise_samples(evttime10_ipd$edss_value[evttime10_ipd$arm == names(average_edss_value_yr10[i])])
  average_edss_value_yr15[i] <- summarise_samples(evttime15_ipd$edss_value[evttime15_ipd$arm == names(average_edss_value_yr15[i])])
  
}

#tabulate outcomes
spms_over_time <- as.data.frame(cbind(p_spms_yr05, p_spms_yr10, p_spms_yr15))
relapses_over_time <- as.data.frame(cbind(n_rel_yr05, n_rel_yr10, n_rel_yr15))
edssincrease_over_time <- as.data.frame(cbind(n_edssincrease_yr05, n_edssincrease_yr10, n_edssincrease_yr15))
edssdecrease_over_time <- as.data.frame(cbind(n_edssdecrease_yr05, n_edssdecrease_yr10, n_edssdecrease_yr15))
saes_over_time <- as.data.frame(cbind(n_saes_yr05, n_saes_yr10, n_saes_yr15))
average_edss_over_time <- as.data.frame(cbind(average_edss_value_yr05, average_edss_value_yr10, average_edss_value_yr15))

View(spms_over_time)
View(relapses_over_time)
View(edssincrease_over_time)
View(edssdecrease_over_time)
View(saes_over_time)
View(average_edss_over_time)

#save validation output

write.csv(key_events_trt_grps, paste0(c("results/keyeventstrtgrps warden probabilistic", n_samples, "x", n_patients, ".csv"), collapse = " "))
write.csv(spms_over_time, paste0(c("results/spmsovertime warden probabilistic", n_samples, "x", n_patients, ".csv"), collapse = " "))
write.csv(relapses_over_time, paste0(c("results/relapsesovertime warden probabilistic", n_samples, "x", n_patients, ".csv"), collapse = " "))
write.csv(edssincrease_over_time, paste0(c("results/edssincreaseovertime warden probabilistic", n_samples, "x", n_patients, ".csv"), collapse = " "))
write.csv(edssdecrease_over_time, paste0(c("results/edssdecreaseovertime warden probabilistic", n_samples, "x", n_patients, ".csv"), collapse = " "))
write.csv(saes_over_time, paste0(c("results/saesovertime warden probabilistic", n_samples, "x", n_patients, ".csv"), collapse = " "))
write.csv(average_edss_over_time, paste0(c("results/averageedssovertime warden probabilistic", n_samples, "x", n_patients, ".csv"), collapse = " "))
ggsave(paste0(c("results/mortalityovertime warden probabilistic", n_samples, "x", n_patients, ".png"), collapse = " "),
       plot=plot_mortality, dpi=1200)


#### This is validation for years spent in edss states, but is not activated within model script ( to add if requested)

## ipd arrays for key model outputs of interest (final when patients are dead)

  #   yrs_edss_0_ipd <- yrs_edss_1_ipd <- yrs_edss_2_ipd <- yrs_edss_3_ipd <- yrs_edss_4_ipd <- yrs_edss_5_ipd <- yrs_edss_6_ipd <- 
  #     yrs_edss_7_ipd <- yrs_edss_8_ipd <- yrs_edss_9_ipd 
  # <- array(NA, dim = c(n_samples, n_treatments, n_patients),dimnames = list(NULL, v_treatment_names, NULL))

# Below is not very efficient but only needs to be run once at end of simulation
# this works for WARDEN to run for DESCEM replace results[[1]][[i_sample]] with results$output_psa[[i_sample]]
for(treatment_name in v_treatment_names) {
  for(i_sample in 1:n_samples) {
    for(i_patient in 1:n_patients) {
      # With statement is neater but much slower
      temp_index <- results[[1]][[i_sample]]$merged_df$pat_id == i_patient &
        results[[1]][[i_sample]]$merged_df$arm == treatment_name &
        #results[[1]][[i_sample]]$merged_df$evttime == 0 & # conditioning on event time 0 misses the number of relapses and final edss value
        #results[[1]][[i_sample]]$merged_df$evtname == "initialise"
        results[[1]][[i_sample]]$merged_df$evtname == "death" # condition on death to count final n relapses and edss value
      
      #validation
      #yrs_edss_0_ipd[i_sample, treatment_name, i_patient] <- results[[1]][[i_sample]]$merged_df$yrs_edss_0[temp_index]
      # yrs_edss_1_ipd[i_sample, treatment_name, i_patient] <- results[[1]][[i_sample]]$merged_df$yrs_edss_1[temp_index]
      #yrs_edss_2_ipd[i_sample, treatment_name, i_patient] <- results[[1]][[i_sample]]$merged_df$yrs_edss_2[temp_index]
      #yrs_edss_3_ipd[i_sample, treatment_name, i_patient] <- results[[1]][[i_sample]]$merged_df$yrs_edss_3[temp_index]
      #yrs_edss_4_ipd[i_sample, treatment_name, i_patient] <- results[[1]][[i_sample]]$merged_df$yrs_edss_4[temp_index]
      #yrs_edss_5_ipd[i_sample, treatment_name, i_patient] <- results[[1]][[i_sample]]$merged_df$yrs_edss_5[temp_index]
      #yrs_edss_6_ipd[i_sample, treatment_name, i_patient] <- results[[1]][[i_sample]]$merged_df$yrs_edss_6[temp_index]
      #yrs_edss_7_ipd[i_sample, treatment_name, i_patient] <- results[[1]][[i_sample]]$merged_df$yrs_edss_7[temp_index]
      #yrs_edss_8_ipd[i_sample, treatment_name, i_patient] <- results[[1]][[i_sample]]$merged_df$yrs_edss_8[temp_index]
      #yrs_edss_9_ipd[i_sample, treatment_name, i_patient] <- results[[1]][[i_sample]]$merged_df$yrs_edss_9[temp_index]
      

    } # End loop over patients
    # Summarise for just this sample
    
  }
}

# Take mean over patients to remove individual variation
#   total_yrs_edss_0 <- apply(yrs_edss_0_ipd, c(1, 2), mean)
#    total_yrs_edss_1 <- apply(yrs_edss_1_ipd, c(1, 2), mean)
#    total_yrs_edss_2 <- apply(yrs_edss_2_ipd, c(1, 2), mean)
#    total_yrs_edss_3 <- apply(yrs_edss_3_ipd, c(1, 2), mean)
#    total_yrs_edss_4 <- apply(yrs_edss_4_ipd, c(1, 2), mean)
#    total_yrs_edss_5 <- apply(yrs_edss_5_ipd, c(1, 2), mean)
#    total_yrs_edss_6 <- apply(yrs_edss_6_ipd, c(1, 2), mean)
#    total_yrs_edss_7 <- apply(yrs_edss_7_ipd, c(1, 2), mean)
#    total_yrs_edss_8 <- apply(yrs_edss_8_ipd, c(1, 2), mean)
#    total_yrs_edss_9 <- apply(yrs_edss_9_ipd, c(1, 2), mean)


#compare events by treatment groups

# yrs_edss_0 <- yrs_edss_1<- yrs_edss_2 <- yrs_edss_3 <- yrs_edss_4 <- 
# yrs_edss_5 <- yrs_edss_6 <- yrs_edss_7 <- yrs_edss_8 <- yrs_edss_9 <- 
# c(rep(0, length(v_treatment_names)))

# names(yrs_edss_0) <- names(yrs_edss_1) <- names(yrs_edss_2) <- names(yrs_edss_3) <- names(yrs_edss_4) <-
# names(yrs_edss_5) <- names(yrs_edss_6) <- names(yrs_edss_7) <-names(yrs_edss_8) <- names(yrs_edss_9) <-
# v_treatment_names

# summarise results

for (i  in 1:length(v_treatment_names)) {
  # yrs_edss_0[i] <- summarise_samples(total_yrs_edss_0[,i])
  # yrs_edss_1[i] <- summarise_samples(total_yrs_edss_1[,i])
  # yrs_edss_2[i] <- summarise_samples(total_yrs_edss_2[,i])
  # yrs_edss_3[i] <- summarise_samples(total_yrs_edss_3[,i])
  # yrs_edss_4[i] <- summarise_samples(total_yrs_edss_4[,i])
  # yrs_edss_5[i] <- summarise_samples(total_yrs_edss_5[,i])
  # yrs_edss_6[i] <- summarise_samples(total_yrs_edss_6[,i])
  # yrs_edss_7[i] <- summarise_samples(total_yrs_edss_7[,i])
  # yrs_edss_8[i] <- summarise_samples(total_yrs_edss_8[,i])
  # yrs_edss_9[i] <- summarise_samples(total_yrs_edss_9[,i])
}

# years_on_edss <- as.data.frame(cbind(yrs_edss_0, yrs_edss_1, 
# yrs_edss_2, yrs_edss_3, yrs_edss_4, yrs_edss_5, yrs_edss_6, yrs_edss_7, yrs_edss_8, yrs_edss_9))
# View(years_on_edss)

