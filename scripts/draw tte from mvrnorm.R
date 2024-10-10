#An example of drawing time to event using data generated from MVN distribution

# load libraries 
library(MASS) 
library(readxl)
library(descem)

#Read and save data
edss_model_names <- read_excel("data-raw/ExponentialSurvivalModels (2).xlsx", 
                               range = "A23:A26", col_names = FALSE)

tto_edss_increase_harrms_mu <- as.matrix(read_excel("data-raw/ExponentialSurvivalModels (2).xlsx", 
                                          sheet = "Natalizumab", range = "B6:B9", 
                                          col_names = FALSE,col_types = c("numeric")))
dimnames(tto_edss_increase_harrms_mu) <- edss_model_names

tto_edss_increase_harrms_sigma <- as.matrix(read_excel("data-raw/ExponentialSurvivalModels (2).xlsx", 
                                                       sheet = "Natalizumab", range = "B13:E16", 
                                                       col_names = FALSE,col_types = c("numeric")))
dimnames(tto_edss_increase_harrms_sigma) <- c(edss_model_names, edss_model_names)

# set seed and create data vectors 
set.seed(98989) 
# draw model coefficients for event edss increase from the MVN distribution
tto_edss_increase <- mvrnorm(n = 1, 
                               mu = tto_edss_increase_harrms_mu, 
                               Sigma = tto_edss_increase_harrms_sigma) 


#set some patient characteristics
patient_age=-10
patient_edss=3
patient_gender =1

# draw time to event edss increase from random model coefficients
# log(rate)=exp(intercept + AgeDuringEvent*patient_age + GenderMALE*patient_gender + BaselineWebEDSS*patient_edss)
set.seed(98989) 
draw_tte(1,
         dist="exp",
         coef1 =  tto_edss_increase["(Intercept)"] + 
                        tto_edss_increase["AgeDuringEvent"]*patient_age +
                          tto_edss_increase["GenderMALE"]*patient_gender +
                            tto_edss_increase["BaselineWebEDSS"]*patient_edss)

