#excess mortality multiplier used sourced from TA312, used in TA127 TA254
# adapted from Pokorski et al. 1997

excess_mortality <- c(1,1.432,1.600,1.637,1.674,1.842,2.273,3.097,4.447,6.454)
names(excess_mortality)<-c("edss0", "edss1","edss2","edss3","edss4", "edss5","edss6","edss7", "edss8","edss9")

write.csv(excess_mortality, file="data/excess_mortality.csv")
