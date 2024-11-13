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
v_arm <- (p_arm*(1-p_arm))/(n_arm+r_arm)
f_arm <- c(3.846153846, 6.060606061)
parameters_names<- c("alpha","beta","follow_up" ,"mean", "sd")

m_saes <- matrix(c(r_arm, n_arm, f_arm, p_arm, sqrt(v_arm)), byrow=FALSE, ncol =5, 
                            dimnames=list(arm_names,parameters_names) )
m_saes
write.csv(m_saes, file="data/m_saes.csv")

#r_sae <- -log(1 - 0.19) /2 # two year follow up time
#write.csv(r_sae, file="data/r_sae.csv")

## Excess all cause mortality for MS patients HR 1.68 95% CI: 1.38-2.05 Jick 2014 (sd calculated below)
v_smr <- as.double(c(1.68, (2.05-1.38)/3.92))
names(v_smr) <- c("hr", "sd")
write.csv(v_smr, file="data/v_smr.csv")

## baseline discontinuation AFFIRM and Antilope
arm_names <- c("AFFIRM Placebo", "AFFIRM Natalizumab IV300", 
               "Antelope Natalizumab biosimiliar", "Antelope Natalizumab IV300" )
n_arm<- c(312,627,131,103) 
r_arm<- c(12,38,8,3) 
p_arm<-r_arm/n_arm
v_arm <- (p_arm*(1-p_arm))/(n_arm+r_arm)
f_arm <- c(3.846153846, 6.060606061, 6.106870229, 2.912621359)
parameters_names<- c("alpha","beta","follow_up" ,"mean", "sd")

m_discontinuation <- matrix(c(r_arm, n_arm, f_arm, p_arm, sqrt(v_arm)), byrow=FALSE, ncol =5, 
            dimnames=list(arm_names,parameters_names) )
m_discontinuation
write.csv(m_discontinuation, file="data/m_discontinuation.csv")

