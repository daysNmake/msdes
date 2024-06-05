##Baseline EDSS Distribution
#Redacted TA767, TA699, 312, 254
BL_TA616     <- c(0.029, 0.030, 0.314, 0.243, 0.235, 0.098, 0.051, 0, 0, 0) #EDSS0-6
BL_TA535_ITT <- c(0.031, 0.188, 0.304, 0.235, 0.147, 0.088, 0.006, 0, 0, 0)
BL_TA535_HA  <- c(0.018, 0.152, 0.304, 0.230, 0.173, 0.117, 0.007, 0, 0, 0)
BL_TA535_RES <- c(0.024, 0.193, 0.307, 0.231, 0.152, 0.086, 0.007, 0, 0, 0)
BL_UKRMSS_TA767  <- c(0.032, 0.163, 0.258, 0.230, 0.155, 0.105, 0.570, 0, 0, 0) 
BL_UKRMSS_TA312  <- c(0.030, 0.700, 0.200, 0.195, 0.205, 0.100, 0.155, 0.450, 0, 0)
baseline_severity <- rbind(BL_TA616,BL_TA535_ITT,BL_TA535_HA,BL_TA535_RES,BL_UKRMSS_TA767,BL_UKRMSS_TA312)
colnames(baseline_severity) <- c(paste("EDSS", 0:9, sep=" "))

# save the data as .csv file
write.csv(baseline_severity, file="data/df_baseline_severity.csv")
#saveRDS(baseline_severity, file="data/df_baseline_severity.rds")
