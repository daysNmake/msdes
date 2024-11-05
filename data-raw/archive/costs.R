
library(readxl)


# Inflated health state direct medical costs from Tyas et al
v_row_names <- as.matrix(read_excel("data-raw/NHS inflation.xlsx", 
                               sheet = "Severity Costs", range = "B19:B29", 
                               col_names = FALSE))
v_col_names <-as.matrix(read_excel("data-raw/NHS inflation.xlsx", 
                                   sheet = "Severity Costs", range = "E18:H18", 
                                   col_names = FALSE))

m_c_edss <- as.matrix(read_excel("data-raw/NHS inflation.xlsx", 
                                             sheet = "Severity Costs", range = "E19:H29", 
                                             col_names = FALSE,col_types = c("numeric")))

row.names(m_c_edss)<-v_row_names
colnames(m_c_edss)<-v_col_names
m_c_edss <- m_c_edss[-8,] # drop edss6.5

write.csv(m_c_edss, file="data/m_c_edss.csv")


# Inflated relapse costs from Hawton etal
v_row_names_relapse <- as.matrix(read_excel("data-raw/NHS inflation.xlsx", 
                                    sheet = "Relapse Costs", range = "B19:B24", 
                                    col_names = FALSE))
v_col_names_relapse <-as.matrix(read_excel("data-raw/NHS inflation.xlsx", 
                                   sheet = "Relapse Costs", range = "E18:F18", 
                                   col_names = FALSE))

m_c_relapse <- as.matrix(read_excel("data-raw/NHS inflation.xlsx", 
                                 sheet = "Relapse Costs", range = "E19:F24", 
                                 col_names = FALSE,col_types = c("numeric")))

row.names(m_c_relapse)<-v_row_names_relapse
colnames(m_c_relapse)<-v_col_names_relapse

write.csv(m_c_relapse, file="data/m_c_relapse.csv")
