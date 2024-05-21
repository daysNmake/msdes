# Model inputs to extracted here from TA616 as dummy
library(readxl)
library(data.table)

#Patient baseline characteristics P.203/594#

### Natural history of acute relapse events
# Annualised relapse rate in the first year (ARR t=1) in placebo arm of CLARITY trial
arr_active_rrms<- 0.34 
arr_active_rrms_lci<-0.30 
arr_active_rrms_uci <-0.38
#The annualised relapse rate in the second and subsequent years were modelled by combining the rate
#in the first year with the annual change in relapse rate per additional year of disease, via the following
#equation ARR(t-1)*RR

# 5-year RR from Tremlett 2010 (search for source and verify)
rr_nh_5yr <- 0.229
rr_lci_nh_5yr <- 0.194
rr_uci_nh_5yr <- 0.262
# to be annualised with RR= exp(1/t * ln(RR)) this will be equivalent to 0.0507 per annum.

# duration of relapses p.217/ 594
days_relapse_hospital <- c(39.14, 29.64, 34.41)
sd_days_relapse_hospital <- c(7.59, 4.85, NA)
days_relapse <- c(42.94, 34.31, 38.64)
sd_days_relapse <- c(7.27, 4.89, NA)
n_group <- c(437, 433, NA)
df_duration_relapse <- cbind(days_relapse_hospital,sd_days_relapse_hospital,days_relapse,
                                  sd_days_relapse, n_group)
rownames(df_duration_relapse)<- c("Placebo-CLARITY", "Clarabidine", "TA616 Model")


saveRDS(df_duration_relapse, file="data/df_duration_relapse.rds")

# Hazard Ratio adjustment for the HA and RES subgroups using the the probability of 6 month progression at 96 weeks
# HR=ln(1-Pa)/ln(1-Pb), data from CLARITY is redacted but maybe we can find this somewhere else?

# Standardised Mortality Ratio for excess MS-related mortality (Manouchehrinia 2016) modeled on data from Jick 2014
smr <- 1.68
smr_lci <-1.38
smr_uci <- 2.05

# Relapse Rate - Treatment adjustment by multiplying the ARR in the Placebo group by the Rate Ratio (ARR Trt vs Pbo)
# Rate Ratios from NMA on P.
# CARES MS-11 p.225/594 TA616
rr_alemtuzumab <- 0.51
rr_lci_alemtuzumab <-0.39
rr_uci_alemtuzumab <-0.65


# treatment waning assumptions where HR waning calculated by adjusting the HR with a Weights.
# This in the absence of treatmet switching although the data was redacted from table 71 of the TA 616
# HR waning = (1-(1-HR)*W)
W_0TO2_YR <- 1
W_2TO5_YR <- 0.75
W_5PLUS_YR <- 0.5



# The probability of discontinuation (P) from the probability of all-cause discontinuation from the NMA (p)
# P=1-e(-((52/t)*ln(1-p))
# Treatment discontinuation annualised probabilities Table 74 P. 232/594






#base case inputs used in the model Table 93 P.254 - 256 /594 of TA616

### References
#Hawton A, Green C. (2016a) Health Utilities for Multiple Sclerosis. Value Health. 19(4): 460-468.

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

#Manouchehrinia A, Tanasescu R, Tench CR, Constantinescu CS. (2016) Mortality in multiple sclerosis:
#meta-analysis of standardised mortality ratios. J Neurol Neurosurg Psychiatry. 87(3): 324-331.

#Orme M, Kerrigan J, Tyas D, Russell N, Nixon R. (2007) The effect of disease, functional status, and
#relapses on the utility of people with multiple sclerosis in the UK. Value Health. 10(1): 54-60.

#Ruutiainen J, Viita AM, Hahl J, Sundell J, Nissinen H. (2016) Burden of illness in multiple sclerosis
#(DEFENSE) study: the costs and quality-of-life of Finnish patients with multiple sclerosis. J Med Econ. 19(1): 21-33.

#Tyas D, Kerrigan J, Russell N, Nixon R. (2007) The distribution of the cost of multiple sclerosis in the
#UK: how do costs vary by illness severity? Value Health. 10(5): 386-389.



