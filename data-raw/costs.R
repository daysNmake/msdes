### Costs
# Model inputs to extracted here from TA616 as dummy
library(readxl)

#costs used in the Analysis Table 82 P. 242 -243 / 594 of TA616

#Drug acquisition costs Table 84 P.245/594 of TA616

#Drug Administration costs Table 85 P.246/594 of TA 616

#EDSS health state costs P.68/594

EDSS_health_state_costs_TA_616 <- read_excel("TA 616 EDSS health state costs .xlsx", 
                                             range = "a2:e12")
c_annual_medical_edss_ta616 <-
  
  saveRDS(c_annual_medical_edss_ta616, file="data/c_annual_medical_edss_ta616.rds")

#Treatment/ Admin and monitoring costs P.70/594
Treatment_Admin_and_monitoring_costs_TA_616 <- read_excel("TA 616 Treatment Admin and monitoring costs .xlsx", 
                                                          range = "A3:G10")
saveRDS(Treatment_Admin_and_monitoring_costs_TA_616, file="data/Treatment_Admin_and_monitoring_costs_TA_616.rds")

c_relapse <- 526
c_relapse_w_hospital<-3463

#Direct medical Costs by EDSS Table 87 P.248/594 of TA616 (Hawton et al 2016a) CY2012 (Karampampa et al 2012c) CY2009 (Tyas et al 2007)

#Costs by relapse state  Table 90 P.252/594 , Table91 P.253/594 of TA616 (Hawton 2016b) (Karampampa 2012c)

#Adverse Events unit costs Table 92 P.253/594

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



