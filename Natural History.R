# The British Columbia data set (Palace 2014) used in TA767, TA616, TA533

# TA616 EDSS states were not modeled as time dependent

#Natural History Age>=28 Table 66 CS P.104/170 (P.220/594) TA616,  table 23 in TA533 CS

library(readxl)
TA_616 <- read_excel("C:/Users/po22525/University of Bristol/grp-Bristol TAG - MTA-MS - MTA-MS/Working Folder_HE/Natural history/TA 616.xlsx", 
          range = "A3:O13")

View(TA_616)
TA_616 <- data.frame(TA_616)
row.names(TA_616)<-TA_616[,1]

Natural_History <- TA_616[,c(2,4,6,9:15)]
colnames(Natural_History) <- TA_616[,1]
Natural_History

#Plot the distribution in each EDSS state
plot(Natural_History[,1])
plot(Natural_History[,2])
plot(Natural_History[,3])
plot(Natural_History[,4])
plot(Natural_History[,5])
plot(Natural_History[,6])
plot(Natural_History[,7])
plot(Natural_History[,8])
plot(Natural_History[,9])
plot(Natural_History[,10])

#Plot the transitions from each EDSS state, some of these allow for backward transitions.
tNH<-transpose(Natural_History)
plot(tNH[,1])
plot(tNH[,2])
plot(tNH[,3])
plot(tNH[,4])
plot(tNH[,5])
plot(tNH[,6])
plot(tNH[,7])
plot(tNH[,8])
plot(tNH[,9])
plot(tNH[,10])

##Baseline EDSS Distribution
#Redacted TA767, TA699, 312, 254
BL_TA616     <- c(0.029, 0.030, 0.314, 0.243, 0.235, 0.098, 0.051, 0, 0, 0) #EDSS0-6
BL_TA535_ITT <- c(0.031, 0.188, 0.304, 0.235, 0.147, 0.088, 0.006, 0, 0, 0)
BL_TA535_HA  <- c(0.018, 0.152, 0.304, 0.230, 0.173, 0.117, 0.007, 0, 0, 0)
BL_TA535_RES <- c(0.024, 0.193, 0.307, 0.231, 0.152, 0.086, 0.007, 0, 0, 0)
BL_UKRMSS_1  <- c(0.032, 0.163, 0.258, 0.230, 0.155, 0.105, 0.570, 0, 0, 0) #from TA767
BL_UKRMSS_2  <- c(0.030, 0.700, 0.200, 0.195, 0.205, 0.100, 0.155, 0.450, 0, 0) #from TA312

## LOndon Ontario natural history in TA767, TA699
