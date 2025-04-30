### Other inputs
v_edss_values <- c(0:9)
names(v_edss_values) <- paste("EDSS", v_edss_values)

## baseline distribution of EDSS health states (AFFIRM baseline both arms)
# EDSS distribution categories: 0, 1+1.5, 2+2.5, 3+3.5, 4+4.5, 5+5.5, 6+6.5, 7+7.5, 8+8.5, 9
v_n_ha_patients_edss <- c(29, 5+1, 39+17, 25+11, 35+21, 18+8, 27+55, 3+7, 0+0, 0) #HA patients, smaller sample
v_n_patients_edss <- c(50, 13+5, 113+87, 126+62, 109+41, 67+23, 74+140, 25+20, 5+0, 0)
m_n_patients_edss <- matrix(c(v_n_ha_patients_edss,v_n_patients_edss), byrow=T, ncol=10,
                            dimnames=list(c("harrms", "rrms"),v_edss_values))

write.csv(m_n_patients_edss, file="data/m_n_patients_edss.csv")


## 19% on Natalizumab would have experienced SAEs, follow-up time 2 years, AFFIRM (Polman 2006)
arm_names <- c("AFFIRM Placebo", "AFFIRM Natalizumab IV300")
n_arm<- c(312,627) 
r_arm<- c(75,119) 
p_arm<-r_arm/n_arm
v_arm <- (p_arm*(1-p_arm))/n_arm
f_arm <- c(2,2) #years
parameters_names<- c("alpha","beta","follow_up" ,"mean", "sd")

m_saes <- matrix(c(r_arm, (n_arm-r_arm), f_arm, p_arm, sqrt(v_arm)), byrow=FALSE, ncol =5, 
                            dimnames=list(arm_names,parameters_names) )
m_saes
write.csv(m_saes, file="data/m_saes.csv")

#r_sae <- -log(1 - 0.19) /2 # two year follow up time
#write.csv(r_sae, file="data/r_sae.csv")

## Excess all cause mortality for MS patients HR 1.68 95% CI: 1.38-2.05 Jick 2014 (sd calculated below)
v_smr <- as.double(c(1.68, (2.05-1.38)/3.92))
names(v_smr) <- c("hr", "sd")
write.csv(v_smr, file="data/v_smr.csv")

## Excess all cause mortality for MS patients HR by severity
v_smr_severity <- matrix(c(rep(1.6, 4), rep(1.84, 3), rep(4.44,3)),
                         nrow=1, byrow = FALSE, dimnames = list("mean", v_edss_values))
write.csv(v_smr_severity, file="data/v_smr_severity.csv")

## Excess all cause mortality by severity edss<4 (Jick et al), edss>=4 (Harding et al)
v_smr_severity_Harding <- matrix(
  c(rep(1.68,4)                      , rep(2.02, 2)                      , 3.86                      , 4.76                     , 22.17                        , 60.74,
    rep((2.05-1.38)/3.92,4)          , rep((3.71-0.98)/3.92, 2)          ,(5.47-2.63)/3.92           , (7.56-2.82)/3.92          , (26.75-18.20)/3.92          ,(76.41-47.62)/3.92,
    rep((log(2.05)-log(1.38))/3.92,4), rep((log(3.71)-log(0.98))/3.92, 2),(log(5.47)-log(2.63))/3.92 , (log(7.56)-log(2.82))/3.92, (log(26.75)-log(18.20))/3.92,(log(76.41)-log(47.62))/3.92
    ),
  nrow=3, byrow = TRUE, dimnames = list(c("smr", "sd", "logsd"), v_edss_values)
)
write.csv(v_smr_severity_Harding, file="data/v_smr_severity_Harding.csv")

## baseline discontinuation AFFIRM and Antilope
arm_names <- c("AFFIRM Placebo", "AFFIRM Natalizumab IV300", 
               "Antelope Natalizumab biosimiliar", "Antelope Natalizumab IV300" )
n_arm<- c(312,627,131,103) 
r_arm<- c(12,38,8,3) 
p_arm<-r_arm/n_arm
v_arm <- (p_arm*(1-p_arm))/n_arm
f_arm <- c(2, 2, 0.5, 0.5) #years
parameters_names<- c("alpha","beta","follow_up" ,"mean", "sd")

m_discontinuation <- matrix(c(r_arm, (n_arm-r_arm), f_arm, p_arm, sqrt(v_arm)), byrow=FALSE, ncol =5, 
            dimnames=list(arm_names,parameters_names) )
m_discontinuation
write.csv(m_discontinuation, file="data/m_discontinuation.csv")

## SPMS Cost (Public Prices)
df_c_tx <-t(read.csv("data/df_c_drugs.csv", header = T, row.names = 1))
colnames(df_c_tx)<-v_treatment_names
df_c_tx_admin <- t(read.csv("data/df_c_drugs_admin.csv", header = T, row.names = 1))
df_c_tx_admin[df_c_tx_admin==0] <-0.1 # set zero costs to positive number stops generating NaNs
colnames(df_c_tx_admin)<-v_treatment_names
df_c_tx_monitor <-t(read.csv("data/df_c_monitoring.csv", header = T, row.names = 1))
colnames(df_c_tx_monitor)<-v_treatment_names

df_c_spms <- as.data.frame(subset(df_c_tx, select = c("PegIFN-β-1 SC 125μg", "IFNβ-1a SC 22μg")))
colnames(df_c_spms)[2] <- "Siponimod"
df_c_spms[,"PegIFN-β-1 SC 125μg"] <- df_c_spms[,"PegIFN-β-1 SC 125μg"]  + df_c_tx_admin[,"PegIFN-β-1 SC 125μg"] + df_c_tx_monitor[,"PegIFN-β-1 SC 125μg"]
df_c_spms[,"Siponimod"] <- c(rep(7239.11+733+139, 3))
write.csv(df_c_spms, file="data/df_c_spms.csv") 


## Re treatment proportions for 2nd and 3rd line treatments HARRMS population (Source: MS Registry Analysis)
treatment_names <- c("Natalizumab-IV","Natalizumab-SC",
                     "Natalizumab biosimilar-IV", "Natalizumab biosimilar-SC",
                     "Cladribine", "Ublituximab", "Ofatumumab", "Ocrelizumab")
n_nat<- 4 # number of Natalizumab treatment variations
# Ublituximab assumed equivelent to Ocrelizumab
n_2ndline<- c(rep(17,n_nat), 17, 34, 23, 34) 
r_2ndline<- rep(91, length(n_2ndline)) 
n_3rdline<- c(rep(2,n_nat), 5, 9, 4, 9) 
r_3rdline<- rep(20, length(n_3rdline)) 
parameters_names<- c("alpha_2ndline","beta_2ndline","alpha_3rdline","beta_3rdline")

m_proportion_ontrtline <- matrix(c(n_2ndline, r_2ndline, n_3rdline, r_3rdline) , byrow=TRUE, ncol =8, 
                                 dimnames=list(parameters_names, treatment_names))
m_proportion_ontrtline

write.csv(m_proportion_ontrtline, file="data/m_proportion_ontrtline.csv")


