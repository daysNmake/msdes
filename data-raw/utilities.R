### Utilities

library(readxl)
# rrms and spms health state utilities
v_row_names_edss <-as.matrix(read_excel("data-raw/health state utilities orme etal.xlsx", 
                                   sheet = "UK MS Survey 2005", range = "A2:A11", 
                                   col_names = FALSE))
  
m_u_edss <- as.matrix(read_excel("data-raw/health state utilities orme etal.xlsx", 
                                 sheet = "UK MS Survey 2005", range = "F1:I11", 
                                 col_names = TRUE,col_types = c("numeric")))
row.names(m_u_edss)<-v_row_names_edss

write.csv(m_u_edss, file="data/m_u_edss.csv")



# Health state utilities by relapse state (Ruutiainen 2016) (Orme 2007) Table 79 P.239/ 594
u_mean_relapse_hospital <- c(-0.071, -0.066)
u_sd_relapse_hospital <- c(0.013, 0.013)
df_u_relapse <- cbind(u_mean_relapse_hospital, u_sd_relapse_hospital)
rownames(df_u_relapse)<- c("Orme etal", "Ruutiainen etal")

write.csv(df_u_relapse, file="data/df_u_relapse.csv")
#saveRDS(df_u_relapse, file="data/df_u_relapse.rds")




# Carer disutilities (Acaster 2013)

v_row_names_edss <-as.matrix(read_excel("data-raw/Carer disutilities.xlsx", 
                                        sheet = "Acaster", range = "A2:A11", 
                                        col_names = FALSE))

m_u_carer <- as.matrix(read_excel("data-raw/Carer disutilities.xlsx", 
                                sheet = "Acaster", range = "B1:C11", 
                                col_names = TRUE,col_types = c("numeric")))

row.names(m_u_carer)<-v_row_names_edss

write.csv(m_u_carer, file="data/m_u_carer.csv")

