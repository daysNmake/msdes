### Post-hoc analysis of patient trajectories to produce results for the 3 natalizumabs with identical QALYs but differing costs

library(dplyr)
#ipd from model with only natalizumab_IV
psa_ipd <- bind_rows(map(results$output_psa, "merged_df"))


#seperate out Nat-IV ipd and triplicate (recombine once analysis is complete)
natorgiv_ipd <-natorigsc_ipd <- natbiosimsc_ipd <- psa_ipd[ psa_ipd$trt == "Natalizumab-IV"]
remainder_ipd <- psa_ipd[ psa_ipd$trt != "Natalizumab-IV"]


# correct treatment names
v_treatment_names <- c("Natalizumab-IV",
                       "Cladribine","Ublituximab" ,
                       "Ofatumumab","Ocrelizumab","Natalizumab-SC","Natalizumab biosimilar-IV" )

# number of treatment
n_treatments <- length(v_treatment_names)

#add treatment codes for Natalizumab-SC and Natalizumab biosimilar-IV
v_treatment_codes <- c(seq(1, n_treatments))
names(v_treatment_codes) <- v_treatment_names

# natalizumab-sc ipd
natorigsc_ipd[,"trt"] <-"Natalizumab-SC"
for( row_i in 1:length(natorigsc_ipd)){
  if (natorigsc_ipd[row_i, current_treatment_code == 1]) {
    natorigsc_ipd[row_i, "current_treatment_code"] <- v_treatment_codes["Natalizumab-SC"]
  }
}

#biosimilar-iv ipd
natbiosimsc_ipd[,"trt"] <- "Natalizumab biosimilar-IV"
for( row_i in 1:length(natbiosimsc_ipd)){
  if (natbiosimsc_ipd[row_i, current_treatment_code == 1]) {
    natbiosimsc_ipd[row_i, "current_treatment_code"] <- v_treatment_codes["Natalizumab biosimilar-IV"]
  }
}

# group the 3 natalizumabs ipd into a single list
nat_list <- list(natorgiv_ipd, natorigsc_ipd, natbiosimsc_ipd)

# Originator -SC and biosimilar-IV treatment, relapse,  health state cost data input ( when not available outside descem environment)
#---------------------------------------------------------------------------------------------
df_c_tx <-t(read.csv("data/df_c_drugs.csv", header = T, row.names = 1))
df_c_tx[,"Natalizumab-IV"] <- (df_c_tx[,"Natalizumab-IV"]*0.6*(round(52/5.55, 2)/13)) + df_c_tx[,"Natalizumab-IV"]*0.4 # 60% on EID interval dosing
df_c_tx <- df_c_tx[,intersect(colnames(df_c_tx),v_treatment_names)]

df_c_tx_admin <- t(read.csv("data/df_c_drugs_admin_v2.csv", header = T, row.names = 1))
df_c_tx_admin[df_c_tx_admin==0] <-0.0001 # set zero costs to positive number stops generating NaNs
df_c_tx_admin[,"Natalizumab-IV"] <- (df_c_tx_admin[,"Natalizumab-IV"]*0.6*(round(52/5.55, 2)/13)) + df_c_tx_admin[,"Natalizumab-IV"]*0.4 # 60% on EID interval dosing
df_c_tx_admin[1,"Natalizumab-SC"] <- (df_c_tx_admin[1,"Natalizumab-SC"] * 0.5) # Scenario for reduced admin cost for Nat SC year 1
df_c_tx_admin[2:3,"Natalizumab-SC"] <- (df_c_tx_admin[2:3,"Natalizumab-SC"] * 0.5) # Scenario for reduced admin cost for Nat SC year 2
df_c_tx_admin <- df_c_tx_admin[,intersect(colnames(df_c_tx_admin),v_treatment_names)]
sd_psa <- 10
cost_yr <- c("cost_annual_yr1", "cost_annual_yr2", "average_annual_cost")
a_c_trt_admin <- array(NA , dim = c(n_samples, nrow(df_c_tx_admin), n_treatments ),
                        dimnames = list( NULL, row.names(df_c_tx_admin), v_treatment_names))
for (i_sample in 1:n_samples){
for (j_cost in 1:nrow(df_c_tx_admin)) {
  for(i_trt in 1:n_treatments) {
    #draw from random gamma distribution with draws = number of simulations
    a_c_trt_admin[i_sample,j_cost ,i_trt] <- rgamma(1, ((df_c_tx_admin[j_cost,i_trt])^2) / ((df_c_tx_admin[j_cost,i_trt] / sd_psa)^2),
                                          df_c_tx_admin[j_cost,i_trt] / ((df_c_tx_admin[j_cost,i_trt]/sd_psa)^2))
  }}}





df_c_tx_monitor <-t(read.csv("data/df_c_monitoring.csv", header = T, row.names = 1))
df_c_tx_monitor[,"Natalizumab-IV"] <- (df_c_tx_monitor[,"Natalizumab-IV"] - 247) #  free JCV test 
df_c_tx_monitor[,"Natalizumab-SC"] <- (df_c_tx_monitor[,"Natalizumab-SC"] - 247) #  free JCV test 
df_c_tx_monitor[,"Natalizumab biosimilar-IV"] <- (df_c_tx_monitor[,"Natalizumab biosimilar-IV"] - 247) #  free JCV test 
df_c_tx_monitor <- df_c_tx_monitor[,intersect(colnames(df_c_tx_monitor),v_treatment_names)]
a_c_trt_monitor <- array(NA , dim = c(n_samples, nrow(df_c_tx_monitor), n_treatments ),
                       dimnames = list( NULL, row.names(df_c_tx_monitor), v_treatment_names))
for (i_sample in 1:n_samples){
  for (j_cost in 1:nrow(df_c_tx_monitor)) {
    for(i_trt in 1:n_treatments) {
      #draw from random gamma distribution with draws = number of simulations
      a_c_trt_monitor[i_sample,j_cost ,i_trt] <- rgamma(1, ((df_c_tx_monitor[j_cost,i_trt])^2) / ((df_c_tx_monitor[j_cost,i_trt] / sd_psa)^2),
                                                      df_c_tx_monitor[j_cost,i_trt] / ((df_c_tx_monitor[j_cost,i_trt]/sd_psa)^2))
    }}}

df_c_tx_load <-t(read.csv("data/df_c_retreatment.csv", header = T, row.names = 1))
df_c_tx_load <- df_c_tx_load[,intersect(colnames(df_c_tx_load),v_treatment_names)]
rownames(df_c_tx_load)<- c(seq(1,5,1))

v_c_rrms_edss <- m_c_edss_rrms[,"rrms_mean"] # health state costs assumed 0 as no difference between the natalizumabs
v_c_spms_edss <- m_c_edss_spms[,"spms_mean"] # health state costs assumed 0 as no difference between the natalizumabs
names(v_c_spms_edss) <- names(v_c_rrms_edss) <- v_edss_values

v_c_relapse <- m_c_relapse[, "mean"] # relapse costs assumed 0 as no difference between the natalizumabs
v_c_sae_events <- t(matrix(df_c_sae, nrow = n_sae, ncol = 1,
                           dimnames = list(sae_names, "sae_cost"))) # sae costs assumed 0 as no difference between the natalizumabs

## discount rates:
lcldrc=0.035
Instantdrc <- log(1+lcldrc)

## addin instant  costs:
#-----------------------
# treatment costs
# add empty treatment costs into a list of the 3 natalizumab ipd data frames
for (trt_i in 1:length(nat_list)){
  nat_list[[trt_i]] <- cbind(nat_list[[trt_i]],"trt_cost" = 0, "relapse_cost" =0, "sae_cost"=0, "hs_cost"=0, "disc_trt_cost"=0, "disc_relapse_cost" =0, "disc_sae_cost"=0, "disc_hs_cost"=0) #, "tot_posthoc"=0)
}

# loop over each natalizumab ipd data frame
for (trt_i in 1:length(nat_list)){
  # temporary object to store ipd per treatment
  nat_list_itrt <-  nat_list[[trt_i]]
  
#loop over each simulation
for(i_sample in 1:n_samples) {
  # temporary object to store ipd per treatment per simulation
  nat_list_itrt_isample <- nat_list_itrt[simulation == i_sample]
  
  #admin and monitoring costs per sample
  v_c_trt_admin <- a_c_trt_admin[i_sample, ,]
  v_c_trt_monitor <- a_c_trt_monitor[i_sample, ,]
  
#loop over each patient
for(i_patient in 1:n_patients) {
  # temporary object to store ipd per treatment per simulation per patient
  nat_list_itrt_isample_ipatient <- nat_list_itrt_isample[pat_id == i_patient]
  
#loop over each row of events for each patient
for (row_i in 1:nrow(nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient])) {
  
  # temporary object to store ipd per treatment per simulation per patient per event
  temp_row <- nat_list_itrt_isample_ipatient[row_i, ]
  
  # addin treatment costs for patients on year 1 treatment
  if(temp_row[,evtname == "annual_treatment" & years_on_treatment <= 1]) {
    
    temp_row[,"trt_cost"] <- (1- temp_row[,treatment_stop] ) *(
      (1 - temp_row[,spms_flag]) * (df_c_tx["cost_annual_yr1",temp_row[,current_treatment_code]] +
                                               v_c_trt_admin["cost_annual_yr1",temp_row[,current_treatment_code]] +
                                               v_c_trt_monitor["cost_annual_yr1",temp_row[,current_treatment_code]]
      ) +
        (temp_row[,spms_flag] * (((1-p_siponimod) * df_c_spms["cost_annual_yr1","PegIFN-β-1 SC 125μg"]) + 
                                            (p_siponimod * df_c_spms["cost_annual_yr1","Siponimod"])))
    )
   
    #  addin treatment costs for patients on year 2 treatment
  } else if (temp_row[,evtname == "annual_treatment" & years_on_treatment == 2]) {
    
    temp_row[,"trt_cost"] <- (1- temp_row[,"treatment_stop"] ) *(
      (1 - temp_row[,spms_flag]) * (df_c_tx["cost_annual_yr2",temp_row[,current_treatment_code]] +
                                               v_c_trt_admin["cost_annual_yr2",temp_row[,current_treatment_code]] +
                                               v_c_trt_monitor["cost_annual_yr2",temp_row[,current_treatment_code]]
      ) +
        (temp_row[,spms_flag] * (((1-p_siponimod) * df_c_spms["cost_annual_yr2","PegIFN-β-1 SC 125μg"]) + 
                                            (p_siponimod * df_c_spms["cost_annual_yr2","Siponimod"])))
    )
    
    #addin treatment costs for patients on year 3 treatment
  } else if (temp_row[,evtname == "annual_treatment" & years_on_treatment == 3]) {
    
    temp_row[,"trt_cost"] <- (1- temp_row[,treatment_stop] ) *(
      (1 - temp_row[,spms_flag]) * ((df_c_tx["cost_annual_yr2",temp_row[,current_treatment_code]] +
                                                v_c_trt_admin["cost_annual_yr2",temp_row[,current_treatment_code]] +
                                                v_c_trt_monitor["cost_annual_yr2",temp_row[,current_treatment_code]]
      ) * df_c_tx_load[3,temp_row[,current_treatment_code]]) +
        (temp_row[,spms_flag] * (((1-p_siponimod) * df_c_spms["cost_annual_yr2","PegIFN-β-1 SC 125μg"]) + 
                                            (p_siponimod * df_c_spms["cost_annual_yr2","Siponimod"])))
    )
    
    #addin treatment costs for patients on year 4 treatment
  } else if (temp_row[,evtname == "annual_treatment" & years_on_treatment == 4]) {
    
    temp_row[,"trt_cost"] <- (1- temp_row[,"treatment_stop"] ) *(
      (1 - temp_row[,spms_flag]) * ((df_c_tx["cost_annual_yr2",temp_row[,current_treatment_code]] +
                                                v_c_trt_admin["cost_annual_yr2",temp_row[,current_treatment_code]] +
                                                v_c_trt_monitor["cost_annual_yr2",temp_row[,current_treatment_code]]
      )  * df_c_tx_load[4,temp_row[,current_treatment_code]]) +
        (temp_row[,spms_flag] * (((1-p_siponimod) * df_c_spms["cost_annual_yr2","PegIFN-β-1 SC 125μg"]) + 
                                            (p_siponimod * df_c_spms["cost_annual_yr2","Siponimod"])))
    )
    
  }
  #addin treatment costs for patients on year 5+ treatment
  else if (temp_row[,evtname == "annual_treatment" &  years_on_treatment >= 5]) {
    
    temp_row[,"trt_cost"] <- (1- temp_row[,"treatment_stop"] ) *(
      (1 - temp_row[,spms_flag]) * ((df_c_tx["cost_annual_yr2",temp_row[,current_treatment_code]] +
                                                v_c_trt_admin["cost_annual_yr2",temp_row[,current_treatment_code]] +
                                                v_c_trt_monitor["cost_annual_yr2",temp_row[,current_treatment_code]]
      )  * df_c_tx_load[5,temp_row[,current_treatment_code]]) +
        (temp_row[,spms_flag] * (((1-p_siponimod) * df_c_spms["cost_annual_yr2","PegIFN-β-1 SC 125μg"]) + 
                                            (p_siponimod * df_c_spms["cost_annual_yr2","Siponimod"])))
    )
    
  } 
  
  # addin relapse costs for patient
  if(temp_row[,evtname == "relapse"]) {
    temp_row[,"relapse_cost"] <- (temp_row[,relapse_hospital_flag]* v_c_relapse[6])+
      ((1 - temp_row[,relapse_hospital_flag] ) * v_c_relapse[4]) 
  } 
  
  # addin saes costs for patient
  if(temp_row[,evtname == "serious_adverse_event"]) {
    temp_row[,"sae_cost"] <- (1- temp_row[,treatment_stop]) * sum(v_c_sae_events * v_weight_sae)
  } 
  
  # addin healthstate costs for patient
  temp_row[,"hs_cost"] <- (1 - temp_row[,spms_flag]) * (v_c_rrms_edss[temp_row[,edss_value] == v_edss_values]) +
    (temp_row[,spms_flag] * v_c_spms_edss[temp_row[,edss_value] == v_edss_values])
  
  #discount treatment costs
  temp_row[,"disc_trt_cost"]<- temp_row[,trt_cost] *  ((1+lcldrc)^(-temp_row[,curtime])) # Note use of DISCRETE TIME discounting for instantaneous costs and benefits
  
  #discount relapse costs
  temp_row[,"disc_relapse_cost"] <- temp_row[,relapse_cost] *  ((1+lcldrc)^(-temp_row[,curtime])) # Note use of DISCRETE TIME discounting for instantaneous costs and benefits
 
   #discount saes costs
  temp_row[,"disc_sae_cost"] <- temp_row[,sae_cost] *  ((1+lcldrc)^(-temp_row[,curtime])) # Note use of DISCRETE TIME discounting for instantaneous costs and benefits

  #discount healthstate costs
  temp_row[,"disc_hs_cost"] <- ((temp_row[,hs_cost])/(0 - Instantdrc)) * (exp(temp_row[,curtime] * ( 0 - Instantdrc)) - exp(temp_row[,prevtime] * (0 - Instantdrc)))

  # Sum up costs per patient
  temp_row[,"cost"] <- temp_row[,disc_trt_cost]+ temp_row[,disc_hs_cost] +  temp_row[,disc_relapse_cost] + temp_row[,disc_sae_cost]

  # save event ipd 
  nat_list_itrt_isample_ipatient[row_i, ] <- temp_row 
  
}
  # save patient ipd
  nat_list_itrt_isample[pat_id == i_patient] <- nat_list_itrt_isample_ipatient
}
  # save simulation ipd
  nat_list_itrt[simulation == i_sample] <- nat_list_itrt_isample
}
  #save treatment ipd
  nat_list[[trt_i]] <- nat_list_itrt
  
  }

#combine processed ipd with remainder of the treatments
results_ipd<-rbind(nat_list[[1]][,-c(22:29)], nat_list[[2]][,-c(22:29)], nat_list[[3]][,-c(22:29)] , remainder_ipd)

# Summarise the results
total_costs_ipd <- total_qalys_ipd <- array(NA, dim = c(n_samples, n_treatments, n_patients),
                                            dimnames = list(NULL, v_treatment_names, NULL))
for (i_trt in v_treatment_names){
for(i_sample in 1:n_samples) {
  for(i_patient in 1:n_patients) {

    total_costs_ipd[i_sample, i_trt, i_patient] <- sum(results_ipd[simulation == i_sample][trt == i_trt][pat_id == i_patient,"cost"])
    total_qalys_ipd[i_sample, i_trt, i_patient] <- sum(results_ipd[simulation == i_sample][trt == i_trt][pat_id == i_patient,"qaly"])
  }
  }}



# Summarise the results


options(scipen = 999)
total_costs <- apply(total_costs_ipd, c(1, 2), mean)
total_qalys <- apply(total_qalys_ipd, c(1, 2), mean)



results_table <- summarise_ce_results(total_qalys = total_qalys, 
                                      total_costs = total_costs,
                                      reference = "Natalizumab-IV")

View(results_table)
