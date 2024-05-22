### Costs
# Model inputs to extracted here from TA616 as dummy
library(readxl)


#Drug acquisition costs Table 84 P.245/594 of TA616
c_drugs_ta616 <- read_excel("data-raw/TA 616 Drug acquisition costs Table 84 .xlsx", 
                      range = "A3:I9")
c_drugs_ta616[,2] <-  as.numeric(c_drugs_ta616$unit_per_pack)
c_drugs_ta616[,4] <-  as.numeric(c_drugs_ta616$units_annual_year1)
c_drugs_ta616[,5] <-  as.numeric(c_drugs_ta616$units_annual_year2)
str(c_drugs_ta616)

df_c_drugs_ta616 <- data.frame(c_drugs_ta616)
row.names(df_c_drugs_ta616)<- df_c_drugs_ta616[,1]
df_c_drugs_ta616 <- df_c_drugs_ta616[,-1]

saveRDS(df_c_drugs_ta616, file="data/df_c_drugs_ta616.rds")


#Drug Administration costs Table 85 P.246/594 of TA 616
c_drugs_admin_ta616 <- read_excel("data-raw/TA 616 Drug Administration costs Table 85 .xlsx", 
                            range = "A2:E7")

str(c_drugs_admin_ta616)
c_drugs_admin_ta616 <- data.frame(c_drugs_admin_ta616)
rownames(c_drugs_admin_ta616)<-c_drugs_admin_ta616[,1]
c_drugs_admin_ta616 <- c_drugs_admin_ta616[,-1]

saveRDS(c_drugs_admin_ta616, file="data/df_c_drugs_admin_ta616.rds")


#Patient monitoring & relapse annual costs P.70/594
c_monitoring_ta616 <- read_excel("data-raw/TA 616 Treatment Admin and monitoring costs .xlsx", 
                                                          range = "A4:G10")

c_monitoring_ta616 <- data.frame(c_monitoring_ta616)
c_monitoring_ta616 <- c_monitoring_ta616[,-2:-5]
rownames(c_monitoring_ta616)<-c_monitoring_ta616[,1]
c_monitoring_ta616 <- c_monitoring_ta616[,-1]

str(c_monitoring_ta616)

saveRDS(c_monitoring_ta_616, file="data/df_c_monitoring_ta616.rds")

c_relapse_yr1 <- c(526, 3463)
c_relapse_yr2<-c(526, 3463)
c_relapse_ta_616 <- data.frame(c_relapse_yr1, c_relapse_yr2)
rownames(c_relapse_ta_616)<-c("relapse","relapse_w_hospital")

saveRDS(c_relapse_ta_616, file="data/df_c_relapse_ta616.rds")


#Direct medical and non and health state costs by edss P.68/594 TA616
#looks like the used Hawton etal for the medical costs and Karampampa etal for the non medical costs
edss_health_state_costs_ta_616 <- read_excel("data-raw/TA 616 EDSS health state costs .xlsx", 
                                             range = "a2:c12")
edss_health_state_costs_ta_616[,2] <- as.numeric(edss_health_state_costs_ta_616$c_medical_annual)
str(edss_health_state_costs_ta_616)

df_c_direct_edss_ta616 <- data.frame(edss_health_state_costs_ta_616)
rownames(df_c_direct_edss_ta616)<-paste("EDSS", df_c_direct_edss_ta616[,1], sep = " ")
df_c_direct_edss_ta616 <- df_c_direct_edss_ta616[,-1]
df_c_direct_edss_ta616[10,1] <- 0

str(df_c_direct_edss_ta616)

saveRDS(df_c_direct_edss_ta616, file="data/df_c_direct_edss_ta616.rds")


#Direct medical Costs by EDSS Tables 87&88 P.248/594 of TA616 (Hawton et al 2016a) CY2012 (Karampampa et al 2012c) CY2009 (Tyas et al 2007)

#Table 87: Annualised costs by EDSS state for medical direct costs/health care and social worker costs
names <- c("Hawton_mean", "Hawton_sd", "Karampampa_mean", "Karampampa_sd", "Tyas_mean", "Tyas_sd") 
EDSS0<- c(1020, 281, 1345,0,   250, 1975)
EDSS1<- c(910 , 168, 1345,0,    85,  899)
EDSS2<- c(716 ,  92, 1345,0,   213,  868)
EDSS3<- c(668 ,  81, 1345,0,   850, 1237)
EDSS4<- c(1002, 110, 2602,0,   806,  884)
EDSS5<- c(1006, 120, 2602,0,  1419,  823)
EDSS6<- c(1304,  94, 2602,0,  2162,  851)
EDSS7<- c(1316, 180, 2602,0,  6583,  995)
EDSS8<- c(3320, 395, 3961,0, 10761, 1069)
EDSS9<- c(0,0, 3961,0, 15121, 2656)
n_sample <-  c( 0, 289, 0, 119, 0, 2048)
Cost_year<- c(2012,0 , 2009,0 , 2005, 0)
df_c_directmedical_edss <- rbind(EDSS0, EDSS1, EDSS2, EDSS3, EDSS4, EDSS5, EDSS6 ,EDSS7, 
                                 EDSS8, EDSS9, n_sample, Cost_year)
df_c_directmedical_edss <-data.frame(df_c_directmedical_edss)
colnames(df_c_directmedical_edss) <- names
str(df_c_directmedical_edss)
saveRDS(df_c_directmedical_edss, file="data/df_c_directmedical_edss.rds")


#Table 88: Annualised costs by EDSS state for non-medical direct costs 
EDSS0<- c(0,0, 1913,    843,  2536, 2183) 
EDSS1<- c(0,0, 1913,    843,  3462, 1314) 
EDSS2<- c(0,0, 1913,    843,  4414, 1314) 
EDSS3<- c(0,0, 1913,    843,  6212, 1585) 
EDSS4<- c(0,0, 10299,  1161,  4028, 1320) 
EDSS5<- c(0,0, 10299,  1161,  6333, 1338) 
EDSS6<- c(0,0, 10299,  1161,  6580, 1338) 
EDSS7<- c(0,0, 41242, 11371, 10808, 1485) 
EDSS8<- c(0,0, 41242, 11371, 15339, 1514) 
EDSS9<- c(0,0, 41242, 11371, 10166, 2837) 

df_c_direct_nonmedical_edss <- rbind(EDSS0, EDSS1, EDSS2, EDSS3, EDSS4, EDSS5, EDSS6 ,EDSS7, 
                                 EDSS8, EDSS9, n_sample, Cost_year)
df_c_direct_nonmedical_edss <-data.frame(df_c_direct_nonmedical_edss)
colnames(df_c_direct_nonmedical_edss) <- names
str(df_c_direct_nonmedical_edss)
saveRDS(df_c_direct_nonmedical_edss, file="data/df_c_direct_nonmedical_edss.rds")

#Costs by relapse state  Table 90 P.252/594 , Table91 P.253/594 of TA616 (Hawton 2016b) (Karampampa 2012c)

#Adverse Events unit costs Table 92 P.253/594

#AE disutilities & costs P.71/594
AE_disutilities_costs_TA_616 <- read_excel("data-raw/TA 616 AE disutilities & costs.xlsx", 
                                           range = "A2:E13")
u_annual_medical_edss_ta616 <-
  
  saveRDS(u_annual_medical_edss_ta616, file="data/u_annual_medical_edss_ta616.rds")

#costs used in the Analysis Table 82 P. 242 -243 / 594 of TA616
c_ta616 <- read_excel("data-raw/TA 616 costs used in the Analysis Table 82 .xlsx", 
                      range = "A2:C14")
c_ta616 <- data.frame(c_ta616)
rownames(c_ta616)<-c_ta616[,1]
c_ta616 <- c_ta616[,-1]

saveRDS(advese_events_disutilities_ta616, file="data/df_u_advese_events.rds")

### References

#Hawton AJ, Green C. (2016b) Multiple sclerosis: relapses, resource use, and costs. Eur J Health Econ. 17(7): 875-884.

#Karampampa K, Gustavsson A, Miltenburger C, Eckert B. (2012a) Treatment experience, burden and
#unmet needs (TRIBUNE) in MS study: results from five European countries. Mult Scler. 18(2 Suppl): 7-  15.

#Karampampa K, Gustavsson A, Miltenburger C, Kindundu CM, Selchen DH. (2012b) Treatment
#experience, burden, and unmet needs (TRIBUNE) in multiple sclerosis: the costs and utilities of MS
#patients in Canada. J Popul Ther Clin Pharmacol. 19(1): e11-e25.

#Karampampa K, Gustavsson A, Miltenburger C, Tyas D. (2012c) Treatment experience, burden and
#unmet needs (TRIBUNE) in MS study: results from the United Kingdom. Mult Scler. 18(2 Suppl): 41-45.

#Karampampa K, Gustavsson A, van Munster ET, Hupperts RM, Sanders EA, et al. (2013) Treatment
#experience, burden, and unmet needs (TRIBUNE) in Multiple Sclerosis
#MS patients in The Netherlands. J Med Econ. 16(7): 939-950.

#Ruutiainen J, Viita AM, Hahl J, Sundell J, Nissinen H. (2016) Burden of illness in multiple sclerosis
#(DEFENSE) study: the costs and quality-of-life of Finnish patients with multiple sclerosis. J Med Econ. 19(1): 21-33

#Tyas D, Kerrigan J, Russell N, Nixon R. (2007) The distribution of the cost of multiple sclerosis in the
#UK: how do costs vary by illness severity? Value Health. 10(5): 386-389.



