library(dplyr)
setwd("C:/Users/espri/OneDrive/Dokumenter/Universitet-mat/Bachelor/Bachelor R")

whales<-read.csv("SwimSpeedFitleredWhaleData.txt",header=TRUE)


argos_whales<-whales[as.Date(whales$time)<as.Date("2012-01-01"),]

names(argos_whales)[names(argos_whales) == "time"] <- "date"
names(argos_whales)[names(argos_whales) == "ID"] <- "id"
argos_whales<-argos_whales[,c(1:4,7)]

argos_whales <- argos_whales %>%
  group_by(id) %>%
  filter(n() > 20)

bs_fit<-bsam::fit_ssm(argos_whales,span=1,tstep=1/24,model="DCRWS")

saveRDS(bs_fit,file="bs_test")
