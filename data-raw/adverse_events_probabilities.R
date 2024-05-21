# Adverse Events probabilities Table 73 P.230/594

library(readxl)

adverse_events_probabilities_ta616 <- read_excel("data-raw/TA 616 Adverse Events probabilities Table 73 .xlsx", 
                                                 range = "A2:F15")

adverse_events_probabilities_ta616 <- adverse_events_probabilities_ta616[-1,]
adverse_events_probabilities_ta616 <- adverse_events_probabilities_ta616[-3,]

df_adverse_events_probabilities_ta616 <-data.frame(adverse_events_probabilities_ta616)
rownames(df_adverse_events_probabilities_ta616) <- df_adverse_events_probabilities_ta616[,1]

df_adverse_events_probabilities_ta616 <-df_adverse_events_probabilities_ta616[,-1]

saveRDS(df_adverse_events_probabilities_ta616, file="df_adverse_events_probabilities_ta616.rds")