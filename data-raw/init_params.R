# Model inputs to extracted here from TA616 as dummy
library(readxl)

#EDSS health state costs P.68/594

EDSS_health_state_costs_TA_616 <- read_excel("TA 616 EDSS health state costs .xlsx", 
                                              range = "a2:e12")
saveRDS(EDSS_health_state_costs_TA_616, file="EDSS_health_state_costs_TA_616.rds")

#Treatment/ Admin and monitoring costs P.70/594
Treatment_Admin_and_monitoring_costs_TA_616 <- read_excel("TA 616 Treatment Admin and monitoring costs .xlsx", 
                                                           range = "A3:G10")
saveRDS(Treatment_Admin_and_monitoring_costs_TA_616, file="Treatment_Admin_and_monitoring_costs_TA_616.rds")

c_relapse <- 526
c_relapse_w_hospital<-3463

#AE disutilities & costs P.71/594


#Patient baseline characteristics P.203/594#

### Natural history of acute relapse events
# Annualised relapse rate in the first year (ARR t=1) in placebo arm of CLARITY trial
ARR_Active_RRMS<- 0.34 
ARR_Active_RRMS_lcl<-0.30 
ARR_Active_RRMS_ucl <-0.38
#The annualised relapse rate in the second and subsequent years were modelled by combining the rate
#in the first year with the annual change in relapse rate per additional year of disease, via the following
#equation ARR(t-1)*RR

# 5-year RR from Tremlett 2010 (search for source and verify)
RR_NH_5yr <- 0.229
RR_NH_5yr_lcl <- 0.194
RR_NH_5yr_ucl <- 0.262
# to be annualised with RR= exp(1/t * ln(RR)) this will be equivalent to 0.0507 per annum.

# duration of relapses p.217/ 594


# Hazard Ratio adjustment for the HA and RES subgroups using the the probability of 6 month progression at 96 weeks
# HR=ln(1-Pa)/ln(1-Pb), data from CLARITY is redacted but maybe we can find this somewhere else?

# Standardised Mortality Ratio for excess MS-related mortality (Manouchehrinia 2016) modeled on data from Jick 2014
SMR <- 1.68
SMR_lcl <-1.38
SMR_ucl <- 2.05

# Relapse Rate - Treatment adjustment by multiplying the ARR in the Placebo group by the Rate Ratio (ARR Trt vs Pbo)
# Rate Ratios from NMA on P.
# CARES MS-11 p.225/594 TA616
RR_ALEMTUZUMAB <- 0.51
RR_ALEMTUZUMAB_LCI <-0.39
RR_ALEMTUZUMAB_UCI <-0.65


# treatment waning assumptions where HR waning calculated by adjusting the HR with a Weights.
# This in the absence of treatmet switching although the data was redacted from table 71 of the TA 616
# HR waning = (1-(1-HR)*W)
W_0TO2_YR <- 1
W_2TO5_YR <- 0.75
W_5PLUS_YR <- 0.5

# Adverse Events probabilities Table 73 P.230/594

# The probability of discontinuation (P) from the probability of all-cause discontinuation from the NMA (p)
# P=1-e(-((52/t)*ln(1-p))
# Treatment discontinuation annualised probabilities Table 74 P. 232/594


### Utilities
# The health effects of relapses were measured as QALY losses, and were calculated from the mean
# duration of each relapse event multiplied by the loss in utility associated with each relapse.

#Baseline utilities (CLARITY) Table 75 P.223//594 TA616

#Utilities reported from a variety of sources Table 76 P. 236/594 (Hawton 2016a)(Orme 2007)

# Carer disutilities Table 78 P.23/594 (Acaster 2013)

# Health state utilities by relapse state (Ruutiainen 2016) (Orme 2007) Table 79 P.239/ 594

# AE disutilities Table 8- P. 240/594

#Utilities used in TA616 Analysis Table 81 P.241/594

### Costs

#costs used in the Analysis Table 82 P. 242 -243 / 594 of TA616

#Drug acquisition costs Table 84 P.245/594 of TA616

#Drug Administration costs Table 85 P.246/594 of TA 616

#Direct medical Costs by EDSS Table 87 P.248/594 of TA616 (Hawton et al 2016a) CY2012 (Karampampa et al 2012c) CY2009 (Tyas et al 2007)

#Costs by relapse state  Table 90 P.252/594 , Table91 P.253/594 of TA616 (Hawton 2016b) (Karampampa 2012c)

#Adverse Events unit costs Table 92 P.253/594

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

#Tyas D, Kerrigan J, Russell N, Nixon R. (2007) The distribution of the cost of multiple sclerosis in the
#UK: how do costs vary by illness severity? Value Health. 10(5): 386-389.



