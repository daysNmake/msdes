# Confirmed Disability Progression Hazard Ratios reported on TAs.
hr_cdp3 <-     c(NA,0.91,1.21, 0.79, NA, 0.93, 0.75, 0.58)
hr_cdp3_lci <- c(NA,0.61,0.68, 0.51, NA, 0.59, 0.58, 0.40)
hr_cdp3_uci <- c(NA,1.33,2.16, 1.24, NA, 1.45, 0.96, 0.84)
df_cdp3_ta312 <- data.frame(hr_cdp3,hr_cdp3_lci, hr_cdp3_uci,
row.names = c("Alemtuzumab", "IM IFNβ-1a", "IFNβ-1b", "SC IFNβ-1a 44μg", "SC IFNβ-1a 22μg", 
              "Glatiramer acetate", "Fingolimod", "Natalizumab")
)

# save the data as .csv file
write.csv(df_cdp3_ta312, file="data/df_cdp3_ta312.csv")
#saveRDS(df_cdp3_ta312, file="data/df_cdp3_ta312.rds")
  
  
  
  
  
  