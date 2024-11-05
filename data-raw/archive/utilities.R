### Utilities

# In TA616 the health effects of relapses were measured as QALY losses, and were calculated from the mean
# duration of each relapse event multiplied by the loss in utility associated with each relapse.

# Model inputs to extracted here from TA616 as dummy
library(readxl)


#Baseline utilities  Table 75 P.223/594 TA616, these were not used in the TA so did not extract them.

#Utilities reported from a variety of sources Table 76 P. 236/594 (CLARITY) (Hawton 2016a)(Orme 2007) (TA320)
# BUT for full regression formulas on would want to look at the actual publications for inclusion of additional co-variates

u_edss_ta616 <- read_excel("data-raw/TA 616 Utilities reported from a variety of population studies Table 76.xlsx", 
                                                                                          range = "A2:I13", col_types = "numeric")
str(u_edss_ta616 )
df_u_edss_ta616 <- data.frame(u_edss_ta616[-1,-1])
rownames(df_u_edss_ta616) <- paste("EDSS", 0:9, sep=" ")
colnames(df_u_edss_ta616)[3:4] <- c("Hawton_mean", "Hawton_se") 

write.csv(df_u_edss_ta616, file="data/df_u_edss_ta616.csv", sep= " ")
#saveRDS(df_u_edss_ta616, file="data/df_u_edss_ta616.rds")





# Carer disutilities Table 78 P.23/594 (Acaster 2013)
u_carer_ta616 <- read_excel("data-raw/TA 616 Carer disutilities Table 78.xlsx", 
                                                 range = "A2:E9")
str(u_carer_ta616)

df_u_carer_ta616 <- data.frame(u_carer_ta616)
rownames(df_u_carer_ta616)<-df_u_carer_ta616[,1]
df_u_carer_ta616 <- df_u_carer_ta616[,-1]

write.csv(df_u_carer_ta616, file = "data/df_u_carer_ta616.csv",sep="")
#saveRDS(df_u_carer_ta616, file = "data/df_u_carer_ta616.rds")





# Health state utilities by relapse state (Ruutiainen 2016) (Orme 2007) Table 79 P.239/ 594
u_mean_relapse_hospital <- c(-0.071, -0.066)
u_sd_relapse_hospital <- c(0.013, 0.013)
df_u_relapse <- cbind(u_mean_relapse_hospital, u_sd_relapse_hospital)
rownames(df_u_relapse)<- c("Orme etal", "Ruutiainen etal")

write.csv(df_u_relapse, file="data/df_u_relapse.csv")
#saveRDS(df_u_relapse, file="data/df_u_relapse.rds")




# AE dis-utilities Table 80 P. 240/594 TA616
u_advese_events_ta616 <- read_excel("data-raw/TA 616 AE disutilities Table 80.xlsx", 
                                       range = "A2:C14")
u_advese_events_ta616[,2] <- as.numeric(u_advese_events_ta616$duration_days)
u_advese_events_ta616[,3] <- as.numeric(u_advese_events_ta616$disutility)
str(u_advese_events_ta616)

df_u_advese_events_ta616 <- data.frame(u_advese_events_ta616)
rownames(df_u_advese_events_ta616)<-df_u_advese_events_ta616[,1]
df_u_advese_events_ta616 <- df_u_advese_events_ta616[,-1]

write.csv(df_u_advese_events_ta616, file="data/df_u_advese_events_ta616.csv")
#saveRDS(df_u_advese_events_ta616, file="data/df_u_advese_events_ta616.rds")


#Utilities used in TA616 Analysis Table 81 P.241/594
# these are same as the ones we have imported


### References
#Hawton A, Green C. (2016a) Health Utilities for Multiple Sclerosis. Value Health. 19(4): 460-468.

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

#Orme M, Kerrigan J, Tyas D, Russell N, Nixon R. (2007) The effect of disease, functional status, and
#relapses on the utility of people with multiple sclerosis in the UK. Value Health. 10(1): 54-60.

#Ruutiainen J, Viita AM, Hahl J, Sundell J, Nissinen H. (2016) Burden of illness in multiple sclerosis
#(DEFENSE) study: the costs and quality-of-life of Finnish patients with multiple sclerosis. J Med Econ. 19(1): 21-33.




