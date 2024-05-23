# The British Columbia data set (Palace 2014) used in TA767, TA616, TA533

# TA616 EDSS states were not modeled as time dependent

#Natural History Age>=28 Table 66 CS P.104/170 (P.220/594) TA616,  table 23 in TA533 CS

library(readxl)
TA_616 <- read_excel("data-raw/TA 616.xlsx", 
          range = "A3:O13")

#View and reformat the data
View(TA_616)
TA_616 <- data.frame(TA_616)
row.names(TA_616)<-TA_616[,1]
Natural_History <- TA_616[,c(2,4,6,9:15)]
colnames(Natural_History) <- TA_616[,1]
Natural_History

# save the data as RDS file
saveRDS(Natural_History, file="data/df_nh_rrms_bc_ta616.rds")

## LOndon Ontario natural history in TA767, TA699
