####################### MS registry data#####################################

# load libraries 
library(readxl)

##### Models excluding age and sex covariates used in the main analysis) ####
# Ignored time to EDSS Decrease (SPMS)

#covariate names
edss_model_names <- read_excel("data-raw/ExponentialSurvivalModels 21Oct2024.xlsx", 
                               range = "M6:M8", col_names = FALSE)

# Natalizumab time to edss increase Highly active RRMS
tto_edss_increase_harrms_mu <- as.matrix(read_excel("data-raw/ExponentialSurvivalModels 21Oct2024.xlsx", 
                                                    sheet = "Models", range = "N6:N8", 
                                                    col_names = FALSE,col_types = c("numeric")))

tto_edss_increase_harrms_sigma <- as.matrix(read_excel("data-raw/ExponentialSurvivalModels 21Oct2024.xlsx", 
                                                       sheet = "Models", range = "N18:P20", 
                                                       col_names = FALSE,col_types = c("numeric")))

dimnames(tto_edss_increase_harrms_mu) <- edss_model_names
dimnames(tto_edss_increase_harrms_sigma) <- c(edss_model_names, edss_model_names)

write.csv(tto_edss_increase_harrms_mu, file="data/v_tto_edss_increase_harrms_mu.csv")
write.csv(tto_edss_increase_harrms_sigma, file="data/m_tto_edss_increase_harrms_sigma.csv")



# Natalizumab time to edss increase ALL RRMS
tto_edss_increase_rrms_mu <- as.matrix(read_excel("data-raw/ExponentialSurvivalModels 21Oct2024.xlsx", 
                                                  sheet = "Models", range = "N34:N36", 
                                                  col_names = FALSE,col_types = c("numeric")))
tto_edss_increase_rrms_sigma <- as.matrix(read_excel("data-raw/ExponentialSurvivalModels 21Oct2024.xlsx", 
                                                     sheet = "Models", range = "N46:P48", 
                                                     col_names = FALSE,col_types = c("numeric")))

dimnames(tto_edss_increase_rrms_mu) <- edss_model_names
dimnames(tto_edss_increase_rrms_sigma) <- c(edss_model_names, edss_model_names)

write.csv(tto_edss_increase_rrms_mu, file="data/v_tto_edss_increase_rrms_mu.csv")
write.csv(tto_edss_increase_rrms_sigma, file="data/m_tto_edss_increase_rrms_sigma.csv")



# Natalizumab Time to EDSS Increase SPMS
tto_edss_increase_spms_mu <- as.matrix(read_excel("data-raw/ExponentialSurvivalModels 21Oct2024.xlsx", 
                                                  sheet = "Models", range = "N90:N92", 
                                                  col_names = FALSE,col_types = c("numeric")))
tto_edss_increase_spms_sigma <- as.matrix(read_excel("data-raw/ExponentialSurvivalModels 21Oct2024.xlsx", 
                                                     sheet = "Models", range = "N102:P104", 
                                                  col_names = FALSE,col_types = c("numeric")))
dimnames(tto_edss_increase_spms_mu) <- edss_model_names
dimnames(tto_edss_increase_spms_sigma) <- c(edss_model_names, edss_model_names)

write.csv(tto_edss_increase_spms_mu, file="data/v_tto_edss_increase_spms_mu.csv")
write.csv(tto_edss_increase_spms_sigma, file="data/m_tto_edss_increase_spms_sigma.csv")




# Time to edss decrease all RRMS all treatments?

# Natalizumab Time to edss decrease all RRMS
tto_edss_decrease_rrms_mu <- as.matrix(read_excel("data-raw/ExponentialSurvivalModels 21Oct2024.xlsx", 
                                                  sheet = "Models", range = "N62:N64", 
                                                  col_names = FALSE,col_types = c("numeric")))
tto_edss_decrease_rrms_sigma <- as.matrix(read_excel("data-raw/ExponentialSurvivalModels 21Oct2024.xlsx", 
                                                     sheet = "Models", range = "N74:P76", 
                                                     col_names = FALSE,col_types = c("numeric")))
dimnames(tto_edss_decrease_rrms_mu) <- edss_model_names
dimnames(tto_edss_decrease_rrms_sigma) <- c(edss_model_names, edss_model_names)

write.csv(tto_edss_decrease_rrms_mu, file="data/v_tto_edss_decrease_rrms_mu.csv")
write.csv(tto_edss_decrease_rrms_sigma, file="data/m_tto_edss_decrease_rrms_sigma.csv")



# Natalizumab Time to relapse HARRMS
tto_relapse_harrms_mu <- as.matrix(read_excel("data-raw/ExponentialSurvivalModels 21Oct2024.xlsx", 
                                            sheet = "Models", range = "N174:N176", 
                                            col_names = FALSE,col_types = c("numeric")))
tto_relapse_harrms_sigma <- as.matrix(read_excel("data-raw/ExponentialSurvivalModels 21Oct2024.xlsx", 
                                               sheet = "Models", range = "N186:P188", 
                                               col_names = FALSE,col_types = c("numeric")))
dimnames(tto_relapse_harrms_mu) <- edss_model_names
dimnames(tto_relapse_harrms_sigma) <- c(edss_model_names, edss_model_names)

write.csv(tto_relapse_harrms_mu, file="data/v_tto_relapse_harrms_mu.csv")
write.csv(tto_relapse_harrms_sigma, file="data/m_tto_relapse_harrms_sigma.csv")


# Natalizumab Time to relapse SPMS
tto_relapse_spms_mu <- as.matrix(read_excel("data-raw/ExponentialSurvivalModels 21Oct2024.xlsx", 
                                                  sheet = "Models", range = "N146:N148", 
                                                  col_names = FALSE,col_types = c("numeric")))
tto_relapse_spms_sigma <- as.matrix(read_excel("data-raw/ExponentialSurvivalModels 21Oct2024.xlsx", 
                                                     sheet = "Models", range = "N158:P160", 
                                                     col_names = FALSE,col_types = c("numeric")))
dimnames(tto_relapse_spms_mu) <- edss_model_names
dimnames(tto_relapse_spms_sigma) <- c(edss_model_names, edss_model_names)

write.csv(tto_relapse_spms_mu, file="data/v_tto_relapse_spms_mu.csv")
write.csv(tto_relapse_spms_sigma, file="data/m_tto_relapse_spms_sigma.csv")

# Natalizumab Time to SP conversion HARRMS
tto_sp_conversion_harrms_mu <- as.matrix(read_excel("data-raw/ExponentialSurvivalModels 21Oct2024.xlsx", 
                                              sheet = "Models", range = "N230:N232", 
                                              col_names = FALSE,col_types = c("numeric")))
tto_sp_conversion_harrms_sigma <- as.matrix(read_excel("data-raw/ExponentialSurvivalModels 21Oct2024.xlsx", 
                                                 sheet = "Models", range = "N242:P244", 
                                                 col_names = FALSE,col_types = c("numeric")))
dimnames(tto_sp_conversion_harrms_mu) <- edss_model_names
dimnames(tto_sp_conversion_harrms_sigma) <- c(edss_model_names, edss_model_names)

write.csv(tto_sp_conversion_harrms_mu, file="data/v_tto_sp_conversion_harrms_mu.csv")
write.csv(tto_sp_conversion_harrms_sigma, file="data/m_tto_sp_conversion_harrms_sigma.csv")


############################################################################

############################################################################
##### Models w/ age and sex covariates not in use in the main analysis) ####
#covariate names
edss_model_names <- read_excel("data-raw/ExponentialSurvivalModels 15Oct2024 (2).xlsx", 
                               range = "A23:A26", col_names = FALSE)


# Natalizumab time to edss increase Highly active RRMS
tto_edss_increase_harrms_mu <- as.matrix(read_excel("data-raw/ExponentialSurvivalModels 15Oct2024 (2).xlsx", 
                                                    sheet = "Natalizumab", range = "B6:B9", 
                                                    col_names = FALSE,col_types = c("numeric")))
dimnames(tto_edss_increase_harrms_mu) <- edss_model_names


tto_edss_increase_harrms_sigma <- as.matrix(read_excel("data-raw/ExponentialSurvivalModels 15Oct2024 (2).xlsx", 
                                                       sheet = "Natalizumab", range = "B13:E16", 
                                                       col_names = FALSE,col_types = c("numeric")))
dimnames(tto_edss_increase_harrms_sigma) <- c(edss_model_names, edss_model_names)

write.csv(tto_edss_increase_harrms_mu, file="data/v_tto_edss_increase_harrms_mu.csv")
write.csv(tto_edss_increase_harrms_sigma, file="data/m_tto_edss_increase_harrms_sigma.csv")



# Natalizumab time to edss increase ALL RRMS
tto_edss_increase_rrms_mu <- as.matrix(read_excel("data-raw/ExponentialSurvivalModels 15Oct2024 (2).xlsx", 
                                                    sheet = "Natalizumab", range = "B23:B26", 
                                                    col_names = FALSE,col_types = c("numeric")))
dimnames(tto_edss_increase_rrms_mu) <- edss_model_names


tto_edss_increase_rrms_sigma <- as.matrix(read_excel("data-raw/ExponentialSurvivalModels 15Oct2024 (2).xlsx", 
                                                       sheet = "Natalizumab", range = "B30:E33", 
                                                       col_names = FALSE,col_types = c("numeric")))
dimnames(tto_edss_increase_rrms_sigma) <- c(edss_model_names, edss_model_names)

write.csv(tto_edss_increase_rrms_mu, file="data/v_tto_edss_increase_rrms_mu.csv")
write.csv(tto_edss_increase_rrms_sigma, file="data/m_tto_edss_increase_rrms_sigma.csv")



# Natalizumab Time to EDSS Increase SPMS
tto_edss_increase_spms_mu <- as.matrix(read_excel("data-raw/ExponentialSurvivalModels 15Oct2024 (2).xlsx", 
                                                    sheet = "Natalizumab", range = "B40:B43", 
                                                    col_names = FALSE,col_types = c("numeric")))
dimnames(tto_edss_increase_spms_mu) <- edss_model_names

tto_edss_increase_spms_sigma <- as.matrix(read_excel("data-raw/ExponentialSurvivalModels 15Oct2024 (2).xlsx", 
                                                       sheet = "Natalizumab", range = "B47:E50", 
                                                       col_names = FALSE,col_types = c("numeric")))
dimnames(tto_edss_increase_spms_sigma) <- c(edss_model_names, edss_model_names)

write.csv(tto_edss_increase_spms_mu, file="data/v_tto_edss_increase_spms_mu.csv")
write.csv(tto_edss_increase_spms_sigma, file="data/m_tto_edss_increase_spms_sigma.csv")




# Time to edss decrease all RRMS all treatments
tto_edss_decrease_rrms_mu <- as.matrix(read_excel("data-raw/ExponentialSurvivalModels 15Oct2024 (2).xlsx", 
                                                    sheet = "Non-Stratified", range = "B6:B9", 
                                                    col_names = FALSE,col_types = c("numeric")))
dimnames(tto_edss_decrease_rrms_mu) <- edss_model_names

tto_edss_decrease_rrms_sigma <- as.matrix(read_excel("data-raw/ExponentialSurvivalModels 15Oct2024 (2).xlsx", 
                                                       sheet = "Non-Stratified", range = "B13:E16", 
                                                       col_names = FALSE,col_types = c("numeric")))
dimnames(tto_edss_decrease_rrms_sigma) <- c(edss_model_names, edss_model_names)

write.csv(tto_edss_decrease_rrms_mu, file="data/v_tto_edss_decrease_rrms_mu.csv")
write.csv(tto_edss_decrease_rrms_sigma, file="data/m_tto_edss_decrease_rrms_sigma.csv")




# Time to edss decrease SPMS all treatments
tto_edss_decrease_spms_mu <- as.matrix(read_excel("data-raw/ExponentialSurvivalModels 15Oct2024 (2).xlsx", 
                                                  sheet = "Non-Stratified", range = "B24:B27", 
                                                  col_names = FALSE,col_types = c("numeric")))
dimnames(tto_edss_decrease_spms_mu) <- edss_model_names

tto_edss_decrease_spms_sigma <- as.matrix(read_excel("data-raw/ExponentialSurvivalModels 15Oct2024 (2).xlsx", 
                                                     sheet = "Non-Stratified", range = "B31:E34", 
                                                     col_names = FALSE,col_types = c("numeric")))
dimnames(tto_edss_decrease_spms_sigma) <- c(edss_model_names, edss_model_names)

write.csv(tto_edss_decrease_spms_mu, file="data/v_tto_edss_decrease_spms_mu.csv")
write.csv(tto_edss_decrease_spms_sigma, file="data/m_tto_edss_decrease_spms_sigma.csv")
