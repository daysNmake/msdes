#All cause mortality from the national life tables for 2020-2022

library(readxl)
library(tidyverse)
nltuk198020203 <- read_excel("data-raw/nltuk198020203.xlsx", 
                         sheet = "2020-2022", range = "A6:M107", 
                           na = "G6:G107")

#View and reformat the data
View(nltuk198020203)
nltuk198020203 <- nltuk198020203[,-7]

# colums 1-6 are male and 7-12 are female
colnames(nltuk198020203) <- rep(c("age", "mx", "qx", "lx", "dx", "ex"), 2)
all_cause_mortality <- data.frame(nltuk198020203)
row.names(all_cause_mortality)<-all_cause_mortality[,1]
head(all_cause_mortality)

# save the data as csv file
write.csv(all_cause_mortality, file="data/df_all_cause_mortality.csv")
#saveRDS(all_cause_mortality, file="data/df_all_cause_mortality.rds")
