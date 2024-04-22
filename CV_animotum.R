library(aniMotum)
library(tidyverse)

# Vi indlæser sub_whales på samme måde som i analysen

whales<-read.csv("SwimSpeedFitleredWhaleData.txt") #Might have been moved to the Data folder

sub_whales<-whales
names(sub_whales)[names(sub_whales) == "time"] <- "date"
names(sub_whales)[names(sub_whales) == "ID"] <- "id"
sub_whales<-sub_whales[,c(1:4,7)]

sub_whales <- sub_whales %>%
  group_by(id) %>%
  filter(n() > 30)
sub_whales$id_old<-extract_prefix(sub_whales$id)

#Her er de to tracks der anvendes. Analysen bør nok deles op på en lidt smartere måde
#Hvor de begge kan inkluderes

#ID<-"37280FB_03-2"
ID<-"170753-35"
k<-20
test_set<-subset(sub_whales,id==ID)

set.seed(1)
group<-sample(rep(1:k,length.out=nrow(test_set))) #So the groups are of equal size

for(i in 1:k){
  #Needed to make predictions at specific time stamps
  time_stamps<-data.frame("id"=test_set[group==i,]$id,"date"=test_set[group==i,]$date)
  time_stamps$date<-as.POSIXct(time_stamps$date)
  
  #We fit the model without the omitted group and ask for predictions at the time-stamps from this group
  model<-fit_ssm(test_set[group!=i,],time.step=time_stamps,model="crw")
  
  #Then grab the predictions
  predicted_loc<-grab(model,what="predicted")
}

#De prædikterede lokationer i hver loop bør nok gemmes så vi kan udregne cv-score (mse).
#Desuden bør crawl tilføjes til dette loop
#(og jeg tror at den kun kan prædiktere hvis start-koordinatet er med, så group skal måske ændres lidt)
#Desuden skal afstande jo regnes, så vi skal lige have samme projektioner
