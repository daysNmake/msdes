
#MS-MTA SAEs
saes_data <- read_excel("data-raw/SAE inputs.xlsx", sheet = "model_input" ,
                             range = c("A1:E10"), 
                             col_names = TRUE,col_types = c("text",rep("numeric", 4)))

saes_data <- as.data.frame(saes_data)
row.names(saes_data) <-saes_data[,1]
saes_data<-saes_data[,-1]

write.csv(saes_utilities, file="data/df_saes_data.csv")
