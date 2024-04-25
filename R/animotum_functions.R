#This is a function for plotting a whole whale, with an option to include a sub-plot
#It needs a data.frame of the observations (whale)
#and one of predictions (predict, which can be extracted using grab(ssm,what="predicted)).
#The scale variables can be used to make sure that the plot is somewhat square
plot_ssm<-function(id_old="170753",
                   whale=sub_whales,
                   predict=ssm_data,
                   sub_plot=TRUE,
                   from=c(xmin=-54.5,xmax=-52.5,ymin=69.1,ymax=69.4),
                   to=c(xmin=-61,xmax=-55,ymin=68,ymax=69),
                   conuntry=countries,
                   scale_x=c(0,0),
                   scale_y=c(0,0)){
  dat1<-whale[which(whale$id_old==id_old),]
  dat2<-predict[which(predict$id_old==id_old),]
  if(sub_plot==FALSE){
    return(ggplot(dat1)+
             geom_sf(data=countries,fill="gray")+
             coord_sf(xlim=range(dat1$lon)+scale_x,ylim=range(dat1$lat)+scale_y,expand=TRUE)+
             geom_point(dat1,mapping=aes(x=lon,y=lat,group=id))+
             geom_path(dat2,mapping=aes(x=lon,y=lat,group=id),color="turquoise"))
  }
  if(sub_plot==TRUE){
    return(ggplot(dat1)+
             geom_sf(data=countries,fill="gray")+
             coord_sf(xlim=range(dat1$lon)+scale_x,ylim=range(dat1$lat)+scale_y,expand=TRUE)+
             geom_point(dat1,mapping=aes(x=lon,y=lat,group=id))+
             geom_path(dat2,mapping=aes(x=lon,y=lat,group=id),color="turquoise")+
             geom_magnify(data=dat1,from=from,to=to,shadow=TRUE)
    )
  }
}

#Plots a simulation. We use the simulation directly since there is no grab method for it
#It is deemed most interesting to only look at a single split and not the whole whale

plot_sim<-function(simu=sim,
                   whale=sub_whales,
                   id="170753-13",
                   conuntry=countries,
                   scale_x=c(0,0),scale_y=c(0,0)){
  for_plot_gps<-as.data.frame(simu[[3]][which(simu$id==id)]) #The simulation is converted to the relevant data.frame
  for_plot_gps$rep<-as.factor(for_plot_gps$rep)
  points<-whale[which(whale$id==id),]
  ggplot()+
    geom_sf(data=country,fill="gray")+coord_sf(xlim=range(for_plot_gps$lon)+scale_x,ylim=range(for_plot_gps$lat)+scale_y)+
    geom_path(data=for_plot_gps[which(for_plot_gps$rep!=0),],mapping=aes(x=lon,y=lat,color=rep))+
    geom_path(data=for_plot_gps[which(for_plot_gps$rep==0),],mapping=aes(x=lon,y=lat),color="black")+
    geom_point(data=points,mapping=aes(x=lon,y=lat),color="blue",size=1)+theme(legend.position = "none")
}

#Make a function for getting id_old (i.e before splits)
extract_prefix<-function(values) {
  # Use a regular expression to remove the last dash and everything following it
  sapply(values, function(x) sub("-[^-]*$", "", x))
}

#Method for converting all simulations to a single dataframe
sim_to_data<-function(sim){
  data<-as.data.frame(sim[[3]][[1]])
  data$id<-sim$id[1]
  for(i in 2:length(sim$id)){
    new_data<-as.data.frame(sim[[3]][[i]])
    new_data$id<-sim$id[i]
    data<-rbind(data,new_data)
  }
  data$rep<-as.factor(data$rep)
  data$id_old<-extract_prefix(data$id)
  return(data)
}

#Method for converting the above to a list
#(used for making analysis of a single simulation) and because it fits with the output of crawl
data_to_list<-function(plot_data){
  imputed_paths<-list()
  for(i in unique(as.integer(plot_data$rep)-1)){
    print(i)
    imputed_paths[[i+1]]<-plot_data[which(plot_data$rep==i),]
  }
  return(imputed_paths)
}

#Function for making the interactive plots

#We need to make sure that the error-prone tracks are not included in the whale data
gen_html<-function(sim_data=plot_straight, 
                   whale=sub_whales,
                   country=countries){
  if (!("date" %in% names(whale)) || !("id" %in% names(whale))) {
    stop("The 'whale' dataframe must contain 'date' and 'id' columns.")
  } #To ensure that the text hover actually works
  
  gg<-ggplot(whale)+
    geom_sf(data=country,fill="gray")+coord_sf(xlim=range(sim_data$lon),ylim=range(sim_data$lat))+
    geom_path(sim_data[which(plot_data$rep!=0),],mapping=aes(x=lon,y=lat,color=rep,group=id))+
    geom_path(sim_data[which(plot_data$rep==0),],mapping=aes(x=lon,y=lat,group=id),color="black")+
    geom_point(whale,mapping=aes(x=lon,y=lat,text=paste("ID:", id, "<br>Date:", date)),color="blue",size=1)
  p<-ggplotly(gg,tooltip = "text")
  
  return(p)
}

## Analysis steps

##### Vi fitter SSM på pre processed data.
ani_analysis_fit_ssm <- function(whale_data){
  sub_whales<-whale_data
  names(sub_whales)[names(sub_whales) == "time"] <- "date"
  names(sub_whales)[names(sub_whales) == "ID"] <- "id"
  sub_whales<-sub_whales[,c(1:4,7)]
  
  sub_whales <- sub_whales %>%
    group_by(id) %>%
    filter(n() > 30)
  sub_whales$id_old<-extract_prefix(sub_whales$id)
  
  ssm<-fit_ssm(sub_whales,vmax=5,model="crw",time.step=2)
  
  ssm
}

#We take the ssm and find out which ones we can simulate from and save the index in the ssm
ani_analysis_find_error_index<-function(state_space_model){
  error_index<-c()
  for(i in 1:nrow(state_space_model)) {
    print(i)
    tryCatch({
      withTimeout({
        sim <- sim_post(state_space_model[i,], reps=1)
        print(nrow(state_space_model[i,]$state_space_model[[1]]$predicted))
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
  return(error_index)
}

ani_analysis_sim_post_to_sim_fit<-function(state_space_model,sim_post,error_index){
  sim_fit<-aniMotum::sim_fit(state_space_model[-error_index,],reps=1)
  sim_fit$sims<-sim_post$psims
  return(sim_fit)
}

ani_sim<-function(ssm,reps){
  error_index = ani_analysis_find_error_index(ssm) # identify tracks for which simulations cannot be made
  sim = sim_post(ssm[-error_index,],reps = reps) # simulating tracks from state space model for track for which simulation is possible
  sim_fit = ani_analysis_sim_post_to_sim_fit(
    state_space_model = ssm,
    sim_post = sim,
    error_index = error_index) # preparation for rerouting
  return(sim_fit)
}
