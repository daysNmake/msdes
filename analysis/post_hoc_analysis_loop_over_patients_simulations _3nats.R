
#rm(list = ls())
#setwd("C:/Users/po22525/University of Bristol/grp-Bristol TAG - MTA-MS - MTA-MS/Working Folder_HE/MS Models/Git Repo/MS-MTA")

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

n_treatments <- length(v_treatment_names)

#add treatment codes for Natalizumab-SC and Natalizumab biosimilar-IV
v_treatment_codes <- c(seq(1, n_treatments))
names(v_treatment_codes) <- v_treatment_names


natorigsc_ipd[,"trt"] <-"Natalizumab-SC"
for( row_i in 1:length(natorigsc_ipd)){
  if (natorigsc_ipd[row_i, current_treatment_code == 1]) {
    natorigsc_ipd[row_i, "current_treatment_code"] <- v_treatment_codes["Natalizumab-SC"]
  }
}

natbiosimsc_ipd[,"trt"] <- "Natalizumab biosimilar-IV"
for( row_i in 1:length(natbiosimsc_ipd)){
  if (natbiosimsc_ipd[row_i, current_treatment_code == 1]) {
    natbiosimsc_ipd[row_i, "current_treatment_code"] <- v_treatment_codes["Natalizumab biosimilar-IV"]
  }
}

#natall_ipd<- rbind(natorgiv_ipd, natorigsc_ipd, natbiosimsc_ipd)

nat_list <- list(natorgiv_ipd, natorigsc_ipd, natbiosimsc_ipd)

# treatment, relpase,  health state cost data input ( when not avaible outside descem enviroment)
#---------------------------------------------------------------------------------------------

df_c_tx <-t(read.csv("data/df_c_drugs.csv", header = T, row.names = 1))
df_c_tx[,"Natalizumab-IV"] <- (df_c_tx[,"Natalizumab-IV"]*0.6*(round(52/5.55, 2)/13)) + df_c_tx[,"Natalizumab-IV"]*0.4 # 60% on EID interval dosing
df_c_tx <- df_c_tx[,intersect(colnames(df_c_tx),v_treatment_names)]

df_c_tx_admin <- t(read.csv("data/df_c_drugs_admin_v2.csv", header = T, row.names = 1))
df_c_tx_admin[df_c_tx_admin==0] <-0.1 # set zero costs to positive number stops generating NaNs
df_c_tx_admin[,"Natalizumab-IV"] <- (df_c_tx_admin[,"Natalizumab-IV"]*0.6*(round(52/5.55, 2)/13)) + df_c_tx_admin[,"Natalizumab-IV"]*0.4 # 60% on EID interval dosing
df_c_tx_admin[1,"Natalizumab-SC"] <- (df_c_tx_admin[1,"Natalizumab-SC"] * 0.5) # Scenario for reduced admin cost for Nat SC year 1
df_c_tx_admin[2:3,"Natalizumab-SC"] <- (df_c_tx_admin[2:3,"Natalizumab-SC"] * 0.5) # Scenario for reduced admin cost for Nat SC year 2
df_c_tx_admin <- df_c_tx_admin[,intersect(colnames(df_c_tx_admin),v_treatment_names)]
v_c_trt_admin <- matrix(df_c_tx_admin, nrow = nrow(df_c_tx_admin), ncol = n_treatments,
                        dimnames = list(row.names(df_c_tx_admin), v_treatment_names))

df_c_tx_monitor <-t(read.csv("data/df_c_monitoring.csv", header = T, row.names = 1))
df_c_tx_monitor[,"Natalizumab-IV"] <- (df_c_tx_monitor[,"Natalizumab-IV"] - 247) #  free JCV test 
df_c_tx_monitor[,"Natalizumab-SC"] <- (df_c_tx_monitor[,"Natalizumab-SC"] - 247) #  free JCV test 
df_c_tx_monitor[,"Natalizumab biosimilar-IV"] <- (df_c_tx_monitor[,"Natalizumab biosimilar-IV"] - 247) #  free JCV test 
df_c_tx_monitor <- df_c_tx_monitor[,intersect(colnames(df_c_tx_monitor),v_treatment_names)]
v_c_trt_monitor <- matrix(df_c_tx_monitor, nrow = nrow(df_c_tx_monitor), ncol = n_treatments,
                          dimnames = list(row.names(df_c_tx_monitor), v_treatment_names))

df_c_tx_load <-t(read.csv("data/df_c_retreatment.csv", header = T, row.names = 1))
df_c_tx_load <- df_c_tx_load[,intersect(colnames(df_c_tx_load),v_treatment_names)]
rownames(df_c_tx_load)<- c(seq(1,5,1))

v_c_rrms_edss <- m_c_edss_rrms[,"rrms_mean"] 
v_c_spms_edss <- m_c_edss_spms[,"spms_mean"] 
names(v_c_spms_edss) <- names(v_c_rrms_edss) <- v_edss_values

v_c_relapse <- m_c_relapse[, "mean"]
v_c_sae_events <- t(matrix(df_c_sae, nrow = n_sae, ncol = 1,
                           dimnames = list(sae_names, "sae_cost")))

## addin instant  costs:
#-----------------------
# treatment costs
for (trt_i in 1:length(nat_list)){
  nat_list[[trt_i]] <- cbind(nat_list[[trt_i]],"trt_cost" = 0, "relapse_cost" =0, "sae_cost"=0, "hs_cost"=0, "disc_trt_cost"=0, "disc_relapse_cost" =0, "disc_sae_cost"=0, "disc_hs_cost"=0) #, "tot_posthoc"=0)
}

for (trt_i in 1:length(nat_list)){
for(i_sample in 1:n_samples) {
for(i_patient in 1:n_patients) {
for (row_i in 1:nrow(nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient])) {
  
  
  if(nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,evtname == "annual_treatment" & years_on_treatment <= 1]) {
    
    nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,"trt_cost"] <- (1- nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,treatment_stop] ) *(
      (1 - nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,spms_flag]) * (df_c_tx["cost_annual_yr1",nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,current_treatment_code]] +
                                               v_c_trt_admin["cost_annual_yr1",nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,current_treatment_code]] +
                                               v_c_trt_monitor["cost_annual_yr1",nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,current_treatment_code]]
      ) +
        (nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,spms_flag] * (((1-p_siponimod) * df_c_spms["cost_annual_yr1","PegIFN-β-1 SC 125μg"]) + 
                                            (p_siponimod * df_c_spms["cost_annual_yr1","Siponimod"])))
    )
    
  } else if (nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,evtname == "annual_treatment" & years_on_treatment == 2]) {
    
    nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,"trt_cost"] <- (1- nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,"treatment_stop"] ) *(
      (1 - nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,spms_flag]) * (df_c_tx["cost_annual_yr2",nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,current_treatment_code]] +
                                               v_c_trt_admin["cost_annual_yr2",nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,current_treatment_code]] +
                                               v_c_trt_monitor["cost_annual_yr2",nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,current_treatment_code]]
      ) +
        (nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,spms_flag] * (((1-p_siponimod) * df_c_spms["cost_annual_yr2","PegIFN-β-1 SC 125μg"]) + 
                                            (p_siponimod * df_c_spms["cost_annual_yr2","Siponimod"])))
    )
    
  } else if (nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,evtname == "annual_treatment" & years_on_treatment == 3]) {
    
    nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,"trt_cost"] <- (1- nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,treatment_stop] ) *(
      (1 - nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,spms_flag]) * ((df_c_tx["cost_annual_yr2",nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,current_treatment_code]] +
                                                v_c_trt_admin["cost_annual_yr2",nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,current_treatment_code]] +
                                                v_c_trt_monitor["cost_annual_yr2",nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,current_treatment_code]]
      ) * df_c_tx_load[3,nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,current_treatment_code]]) +
        (nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,spms_flag] * (((1-p_siponimod) * df_c_spms["cost_annual_yr2","PegIFN-β-1 SC 125μg"]) + 
                                            (p_siponimod * df_c_spms["cost_annual_yr2","Siponimod"])))
    )
    
  } else if (nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,evtname == "annual_treatment" & years_on_treatment == 4]) {
    
    nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,"trt_cost"] <- (1- nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,"treatment_stop"] ) *(
      (1 - nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,spms_flag]) * ((df_c_tx["cost_annual_yr2",nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,current_treatment_code]] +
                                                v_c_trt_admin["cost_annual_yr2",nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,current_treatment_code]] +
                                                v_c_trt_monitor["cost_annual_yr2",nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,current_treatment_code]]
      )  * df_c_tx_load[4,nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,current_treatment_code]]) +
        (nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,spms_flag] * (((1-p_siponimod) * df_c_spms["cost_annual_yr2","PegIFN-β-1 SC 125μg"]) + 
                                            (p_siponimod * df_c_spms["cost_annual_yr2","Siponimod"])))
    )
    
  }
  else if (nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,evtname == "annual_treatment" &  years_on_treatment >= 5]) {
    
    nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,"trt_cost"] <- (1- nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,"treatment_stop"] ) *(
      (1 - nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,spms_flag]) * ((df_c_tx["cost_annual_yr2",nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,current_treatment_code]] +
                                                v_c_trt_admin["cost_annual_yr2",nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,current_treatment_code]] +
                                                v_c_trt_monitor["cost_annual_yr2",nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,current_treatment_code]]
      )  * df_c_tx_load[5,nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,current_treatment_code]]) +
        (nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,spms_flag] * (((1-p_siponimod) * df_c_spms["cost_annual_yr2","PegIFN-β-1 SC 125μg"]) + 
                                            (p_siponimod * df_c_spms["cost_annual_yr2","Siponimod"])))
    )
    
  } #else {nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,trt_cost] <- 0}
  
  
  if(nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,evtname == "relapse"]) {
    nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,"relapse_cost"] <- (nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,relapse_hospital_flag]* v_c_relapse[6])+
      ((1 - nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,relapse_hospital_flag] ) * v_c_relapse[4]) 
  } #else {nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,relapse_cost] <- 0}
  
  if(nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,evtname == "serious_adverse_event"]) {
    nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,"sae_cost"] <- (1- nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,treatment_stop]) * sum(v_c_sae_events * v_weight_sae)
  } #else {nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,sae_cost] <- 0}
  
  nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,"hs_cost"] <- (1 - nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,spms_flag]) * (v_c_rrms_edss[nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,edss_value] == v_edss_values]) +
    (nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,spms_flag] * v_c_spms_edss[nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,edss_value] == v_edss_values])
  
}}}}

## discount  costs
#--------------------------------------
# the instantaneous costs

lcldrc=0.035
for (trt_i in 1:length(nat_list)){
for(i_sample in 1:n_samples) {
for(i_patient in 1:n_patients) {
for (row_i in 1:nrow(nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient])) {
  
#  if (nat_list[[trt_i]][row_i,trt_cost > 0] ) {
    
    nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,"disc_trt_cost"]<- nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,trt_cost] *  ((1+lcldrc)^(-nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,curtime])) # Note use of DISCRETE TIME discounting for instantaneous costs and benefits
  
#    } else {nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,"disc_trt_cost"]<-0}
  
  nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,"disc_relapse_cost"] <- nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,relapse_cost] *  ((1+lcldrc)^(-nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,curtime])) # Note use of DISCRETE TIME discounting for instantaneous costs and benefits
  
  nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,"disc_sae_cost"] <- nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,sae_cost] *  ((1+lcldrc)^(-nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,curtime])) # Note use of DISCRETE TIME discounting for instantaneous costs and benefits
  
  #nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,disc_hs_cost] <- nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,hs_cost] *  ((1+lcldrc)^(-nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,curtime])) # ongoing not instantaious costs
  
}}}}

## discount on going costs

Instantdrc <- log(1+lcldrc)
for (trt_i in 1:length(nat_list)){
for(i_sample in 1:n_samples) {
for(i_patient in 1:n_patients) {
for (row_i in 1:nrow(nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient])) {
  
  #if (nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,hs_cost > 0] ) {
    nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,"disc_hs_cost"] <- ((nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,hs_cost])/(0 - Instantdrc)) * (exp(nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,curtime] * ( 0 - Instantdrc)) - exp(nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,prevtime] * (0 - Instantdrc)))
  #} #else {nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,disc_hs_cost] <-0}
}}}}

## Sum up costs per patient

for (trt_i in 1:length(nat_list)){
for(i_sample in 1:n_samples) {
for(i_patient in 1:n_patients) {
for (row_i in 1:nrow(nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient])) {

  #nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,"tot_posthoc"] <- nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,disc_trt_cost]+ nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,disc_hs_cost] + 
  #                                                            nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,disc_relapse_cost] + nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,disc_sae_cost]
  nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,"cost"] <- nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,disc_trt_cost]+ nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,disc_hs_cost] + 
                                                              nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,disc_relapse_cost] + nat_list[[trt_i]][simulation == i_sample][pat_id == i_patient][row_i,disc_sae_cost]
  
  }
}}}

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
