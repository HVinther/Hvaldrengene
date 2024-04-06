library(aniMotum)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(plotly)
#Notes
#Very few tracks cannot be simulated from. These individualtracks are:
#i=160 ("22854_10-37"),i=163 ("24638_10-15"),i=220 ("37228_10-56"),
#i=234 ("37228_10-73"),i=238 ("37228_10-82"), i=353 ("7929_05-82"),
#i=359 ("7929_10-38"), i=369 ("93116_10-48"), 

getwd()

whales<-read.csv("SwimSpeedFitleredWhaleData.txt")

sub_whales<-whales

names(sub_whales)[names(sub_whales) == "time"] <- "date"
names(sub_whales)[names(sub_whales) == "ID"] <- "id"
sub_whales<-sub_whales[,c(1:4,7)]

sub_whales <- sub_whales %>%
  group_by(id) %>%
  filter(n() > 30)

ssm<-fit_ssm(sub_whales,vmax=5,model="crw",time.step=2)
ssm<-readRDS("aniMotum_ssm")
#saveRDS(ssm,"aniMotum_ssm")


#For finding the error-prone tracks
for(i in 370:nrow(ssm)){
  print(i)
  print(nrow(ssm[i,]$ssm[[1]]$predicted))
  sim<-sim_post(ssm[i,],reps=1)
}
error_index<-c(160,163,220,234,238,353,359,369)

sim<-sim_post(ssm[-error_index,],reps=1)

plot_data<-as.data.frame(sim$psims[[1]])
plot_data$id<-sim$id[1]
for(i in 2:length(sim$id)){
  new_data<-as.data.frame(sim$psims[[i]])
  new_data$id<-sim$id[i]
  plot_data<-rbind(plot_data,new_data)
}
plot_data$rep<-as.factor(plot_data$rep)

countries <- ne_countries(scale = "medium", type = "countries", returnclass = "sf")


gg<-ggplot()+
  geom_path(plot_data[which(plot_data$rep!=0),],mapping=aes(x=lon,y=lat,color=rep,group=id))+
  geom_path(plot_data[which(plot_data$rep==0),],mapping=aes(x=lon,y=lat,group=id),color="black")+
  geom_point(sub_whales[-which(sub_whales$id%in%ssm$id[error_index]),],mapping=aes(x=lon,y=lat),color="blue",size=0.05)+
  geom_sf(data=countries,fill="gray")+coord_sf(xlim=range(plot_data$lon),ylim=range(plot_data$lat))

p<-ggplotly(gg)  

p


