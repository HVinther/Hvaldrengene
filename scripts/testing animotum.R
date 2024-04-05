library(aniMotum)
library(tidyverse)

sub_whales<-whales

names(sub_whales)[names(sub_whales) == "time"] <- "date"
names(sub_whales)[names(sub_whales) == "ID"] <- "id"
sub_whales<-sub_whales[,c(1:4,7)]

sub_whales <- sub_whales %>%
  group_by(id) %>%
  filter(n() > 30)

ssm<-fit_ssm(sub_whales,vmax=5,model="crw",time.step=2)



sim<-sim_post(ssm,rep=10)

plot_data<-as.data.frame(sim$psims[[1]])
plot_data$id<-sim$id[1]
for(i in 2:length(sim$id)){
  new_data<-as.data.frame(sim$psims[[i]])
  new_data$id<-sim$id[i]
  plot_data<-rbind(plot_data,new_data)
}
plot_data$rep<-as.factor(plot_data$rep)


ggplot()+
  geom_path(plot_data[which(plot_data$rep!=0),],mapping=aes(x=lon,y=lat,color=rep))+
  geom_path(plot_data[which(plot_data$rep==0),],mapping=aes(x=lon,y=lat),color="black")



