# Relapse severity Hawton and Green 2016

t_follwup <- 0.5 # 6 months

severity_names <- c("No_relapse", "untreated", "limited_activities", "oral_steroids",
                    "intravenous_steroids", "hospital_admission" )

v_n_relapse_severity <- matrix(c(941, 625, 634, 196, 85, 90), ncol = , dimnames = list(c(NULL,severity_names ))) # no of patients classified by no relapse, relapse severity 

n_patients <- sum(v_n_relapse_severity) # total number of patients

v_p_relapse_severity <- v_n_relapse_severity/n_patients # annual rate of relpse by severity

v_r_relapse_severity <- -log(1-v_p_relapse_severity)/t_follwup # annual rate of relpse by severity

colnames(v_p_relapse_severity) <- "prortion"

colnames(v_r_relapse_severity) <- "annual_rate"

v_p_relapse_severity<- t(v_p_relapse_severity)

v_r_relapse_severity <- t(v_r_relapse_severity)

write.csv(v_p_relapse_severity, file="data/v_p_relapse_severity.csv")
write.csv(v_r_relapse_severity, file="data/v_r_relapse_severity.csv")
