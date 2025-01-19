### Utilities

library(readxl)
# rrms and spms health state utilities orme et al
v_row_names_edss <-as.matrix(read_excel("data-raw/health state utilities orme etal.xlsx", 
                                   sheet = "UK MS Survey 2005", range = "A2:A11", 
                                   col_names = FALSE))
  
m_u_edss <- as.matrix(read_excel("data-raw/health state utilities orme etal.xlsx", 
                                 sheet = "UK MS Survey 2005", range = "F1:I11", 
                                 col_names = TRUE,col_types = c("numeric")))
row.names(m_u_edss)<-v_row_names_edss

write.csv(m_u_edss, file="data/m_u_edss.csv")

# rrms and spms health state utilities OPERA(EDSS0-7) + Orme(EDSS8-9)
m_u_edss_opera <- as.matrix(read_excel("data-raw/Opera_Clarity_utilities.xlsx", 
                                 sheet = "Opera", range = "B1:E11", 
                                 col_names = TRUE,col_types = c("numeric")))
row.names(m_u_edss_opera)<-v_row_names_edss

write.csv(m_u_edss_opera, file="data/m_u_edss_opera.csv")

# rrms and spms health state utilities CLARITY(EDSS0-6) + Orme(EDSS7-9)
m_u_edss_clarity <- as.matrix(read_excel("data-raw/Opera_Clarity_utilities.xlsx", 
                                       sheet = "Clarity", range = "B1:E11", 
                                       col_names = TRUE,col_types = c("numeric")))
row.names(m_u_edss_clarity)<-v_row_names_edss

write.csv(m_u_edss_clarity, file="data/m_u_edss_clarity.csv")

# Health state utilities by relapse state (Ruutiainen 2016) (Orme 2007) Table 79 P.239/ 594
u_mean_relapse_hospital <- c(-0.071, -0.066, -0.1006)
u_sd_relapse_hospital <- c(0.013, 0.013, 0.0201)
df_u_relapse <- cbind(u_mean_relapse_hospital, u_sd_relapse_hospital)
rownames(df_u_relapse)<- c("Orme etal", "Ruutiainen etal", "Opera")

write.csv(df_u_relapse, file="data/df_u_relapse.csv")
#saveRDS(df_u_relapse, file="data/df_u_relapse.rds")




# Carer disutilities (Acaster 2013), TA27
v_row_names_edss <-as.matrix(read_excel("data-raw/Carer disutilities.xlsx", 
                                        sheet = "Acaster", range = "A2:A11", 
                                        col_names = FALSE))

m_u_carer <- as.matrix(read_excel("data-raw/Carer disutilities.xlsx", 
                                sheet = "Acaster", range = "B1:C11", 
                                col_names = TRUE,col_types = c("numeric")))

row.names(m_u_carer)<-v_row_names_edss

write.csv(m_u_carer, file="data/m_u_carer.csv")

m_u_carer_ta127 <- as.matrix(read_excel("data-raw/Carer disutilities.xlsx", 
                                  sheet = "TA127", range = "B1:C11", 
                                  col_names = TRUE,col_types = c("numeric")))

row.names(m_u_carer_ta127)<-v_row_names_edss

write.csv(m_u_carer_ta127, file="data/m_u_carer_ta127.csv")
