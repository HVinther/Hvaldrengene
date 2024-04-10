library(aniMotum)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(plotly)
library(R.utils)
#Notes
#Very few tracks cannot be simulated from. These individualtracks are:
#i=160 ("22854_10-37"),i=163 ("24638_10-15"),i=220 ("37228_10-56"),
#i=234 ("37228_10-73"),i=238 ("37228_10-82"), i=353 ("7929_05-82"),
#i=359 ("7929_10-38"), i=369 ("93116_10-48"), 

##If this all fails, I might try with fitting an mp model directly


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
ssm<-readRDS("rds/ssm_animotum")
#saveRDS(ssm,"aniMotum_ssm")



#For finding the error-prone tracks

# Initialize an empty vector to store indices where sim_post() fails due to errors (excluding timeout)
error_index <- c()

# Loop through each row of ssm
for(i in 1:nrow(ssm)) {
  print(i)
  
  tryCatch({
    withTimeout({
      sim <- sim_post(ssm[i,], reps=1)
      # This line might need adjustment based on your specific data structure
      print(nrow(ssm[i,]$ssm[[1]]$predicted))
    }, timeout = 10) # Set timeout limit to 10 seconds
  }, error = function(e) {
    # Check if the error is due to a timeout
    if (!inherits(e, "TimeoutException")) {
      # If the error is not a timeout, append to error_index outside the function scope
      error_index <<- c(error_index, i)
      print(paste("Error at index:", i, "- Error message:", e$message))
    } else {
      print(paste("Execution exceeded 10 seconds at index:", i))
    }
  })
}

# error_index<-c(160,163,220,234,238,353,359,369)

# Making simulation
sim<-sim_post(ssm[-error_index,],reps=10)
sim<-readRDS("rds/sims_animotum")

## Turning it into a sim_fit object for the rerouting
sim_fit<-sim_fit(ssm[-error_index,])
sim_fit$sims<-sim$psims

sim_route<-route_path(sim_fit,append=FALSE,dist=200)
# id=7618_10-23 is strange

saveRDS(sim_route,"sims_animotum_reroute")

extract_prefix<-function(values) {
  # Use a regular expression to remove the last dash and everything following it
  sapply(values, function(x) sub("-[^-]*$", "", x))
}


sim_to_data<-function(sim){ #Might need to adjust the sim$sims
  plot_data<-as.data.frame(sim$sims[[1]])
  plot_data$id<-sim$id[1]
  for(i in 2:length(sim$id)){
    new_data<-as.data.frame(sim$sims[[i]])
    new_data$id<-sim$id[i]
    plot_data<-rbind(plot_data,new_data)
  }
  plot_data$rep<-as.factor(plot_data$rep)
  plot_data$id_old<-extract_prefix(plot_data$id)
  return(plot_data)
}

plot_reroute<-sim_to_data(sim_route)
plot_straight<-sim_to_data(sim_fit)





# Plotting them
countries <- ne_countries(scale = 10, type = "countries", returnclass = "sf")


gg<-ggplot()+
  geom_path(plot_data[which(plot_data$rep!=0),],mapping=aes(x=lon,y=lat,color=rep,group=id))+
  geom_path(plot_data[which(plot_data$rep==0),],mapping=aes(x=lon,y=lat,group=id),color="black")+
  geom_point(sub_whales[-which(sub_whales$id%in%ssm$id[error_index]),],mapping=aes(x=lon,y=lat),color="blue",size=0.1)+
  geom_sf(data=countries,fill="gray")+coord_sf(xlim=range(plot_data$lon),ylim=range(plot_data$lat))

p<-ggplotly(gg)  

p

# Adds covariates to data

plot_reroute$ZOOC<-addCov(plot_reroute,ZOOC,type="extract",printFlag=FALSE)
plot_reroute$SST<-addCov(plot_reroute,SST,type="extract",printFlag=FALSE)
## For gradients I am not sure that the method is made for the case where we have every rep in the same dataframe



# Turns data into list

data_to_list<-function(plot_data){
  imputed_paths<-list()
  for(i in unique(as.integer(plot_data$rep)-1)){
    print(i)
    imputed_paths[[i+1]]<-plot_data[which(plot_data$rep==i),]
  }
  return(imputed_paths)
}


# Fits mpm

route_list<-data_to_list(plot_reroute)

mpm0<-fit_mpm(route_list[[1]])


