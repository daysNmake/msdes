# load packages
library(descem)
library(dplyr)
library(flexsurv)
library(ggplot2)
library(kableExtra)
library(purrr)
library(tidyr)

#load data
init_params <- read.csv("data/df_init_params.csv")

p_nh <- read.csv("data/df_nh_rrms_bc_ta616.csv")

u_nh <- read.csv("data/df_u_edss_ta616.csv")
#u_relapse
#u_carer
#u_ae

c_m_nh <- read.csv("data/df_c_directmedical_edss.csv")
c_nm_nh <- read.csv("data/df_c_direct_nonmedical_edss.csv")
c_tx <- read.csv("data/df_c_drugs_ta616.csv")
#c_monitoring
#c_replase
#c_disutilities


# Transform data

r_nh <- -log(1-p_nh[,2:11]) # natural history rates by taking -log(1-P)


#Utilities data: health states
util.data <- data.frame( name =  c(paste0("u.edss", 0:9, "")),
                         value = c(u_nh[,4]),
                         se=     c(u_nh[,5]),
                         stringsAsFactors = FALSE
)

util.data[10,2:3] <-0

#Costs : direct medical, direct non-medical, treatment
cost.data <- data.frame( name = c(paste0("c.m_edss", 0:9, ""),
                                  paste0("c.nm_edss", 0:9, ""),
                                  "c.tx.clab", "c.tx.alem", "c.tx.fing", "c.tx.nat", "c.tx.dac"),
                         value = c(c_m_nh[1:9,2],c_m_nh[10,6], c_nm_nh[1:10,6],c_tx[2:6,3]),
                         se=     c(c_m_nh[1:9,3],c_m_nh[10,7], c_nm_nh[1:10,7], value[21:25]/5 ),
                         stringsAsFactors = FALSE
) 
