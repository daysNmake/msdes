# load packages
library(descem)
library(dplyr)
library(flexsurv)
library(ggplot2)
library(kableExtra)
library(purrr)
library(tidyr)

###load data
init_params <- read.csv("data/df_init_params.csv")

r_nh <- read.csv("data/df_r_nh_rrms_bc_ta616.csv")
r_nh <- r_nh[,-1]
u_nh <- read.csv("data/df_u_edss_ta616.csv")
hr_cdp <- read.csv("data/df_cdp3_ta312.csv")
#u_relapse
#u_carer
#u_ae

c_m_nh <- read.csv("data/df_c_directmedical_edss.csv") #need to be inflated
c_nm_nh <- read.csv("data/df_c_direct_nonmedical_edss.csv") #need to be inflated
c_tx <- read.csv("data/df_c_drugs.csv")
c_tx <-c_tx[-7:-8,]
c_tx_admn <- read.csv("data/df_c_drugs_admin.csv") #need to be inflated
c_tx_admn <- c_tx_admn[-6:-7,]
#c_monitoring
#c_replase
#c_disutilities


# fixed hr used in place pending implementation of trt dependent hr
hr_int <-hr_cdp[8,2] # Natalizumab for the treatment group
hr_noint <- hr_cdp[3,2] # IF-1b for the reference group

###Utilities data: health states
u.data <- data.frame( name =  c(paste0("u.edss", 0:9, "")),
                         value = c(u_nh[,4]),
                         se=     c(u_nh[,5]),
                         stringsAsFactors = FALSE
)

u.data[10,2:3] <-0

#Costs : direct medical, direct non-medical, treatment
c.data <- data.frame( name = c(paste0("c.m_edss", 0:9, ""),
                                  paste0("c.nm_edss", 0:9, ""),
                                  "c.tx.clab", "c.tx.alem", "c.tx.fing", "c.tx.nat", "c.tx.dac",
                                  "c.tx.ib1a", "c.tx.ib1b", "c.tx.glat", 
                                  "c.tx_admn.clab", "c.tx_admn.alem", "c.tx_admn.fing", "c.tx_admn.nat", "c.tx_admn.dac",
                                  "c.tx_admn.ib1a", "c.tx_admn.ib1b", "c.tx_admn.glat"),
                         value = c(c_m_nh[1:9,2],c_m_nh[10,6], c_nm_nh[1:10,6],c_tx[2:9,6],c_tx_admn[,2]),
                         se=     c(c_m_nh[1:9,3],c_m_nh[10,7], c_nm_nh[1:10,7], c_tx[2:9,6]/5,c_tx_admn[,2]/5 ),
                         stringsAsFactors = FALSE
) 
