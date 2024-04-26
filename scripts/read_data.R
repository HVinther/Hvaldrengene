old_whale_loc<-read.csv("Data/data_argos")
new_whale_loc<-read.csv("Data/data_gps")

whales<-create_whale_data(old_whale_loc,new_whale_loc,what="estimated",filter=FALSE)



sub_whales<-whales
names(sub_whales)[names(sub_whales) == "time"] <- "date"
names(sub_whales)[names(sub_whales) == "ID"] <- "id"
sub_whales<-sub_whales[,c(1:4,7)]

sub_whales <- sub_whales %>%
  group_by(id) %>%
  filter(n() > 30)
sub_whales$id_old<-extract_prefix(sub_whales$id)