#For at finde steder hvor data ryger i analysen kan søges på "lose some data"


##### Vi fitter SSM på pre processed data.

whales<-read.csv("SwimSpeedFitleredWhaleData.txt") #Might have been moved to the Data folder

sub_whales<-whales
names(sub_whales)[names(sub_whales) == "time"] <- "date"
names(sub_whales)[names(sub_whales) == "ID"] <- "id"
sub_whales<-sub_whales[,c(1:4,7)]

sub_whales <- sub_whales %>%
  group_by(id) %>%
  filter(n() > 30)
sub_whales$id_old<-extract_prefix(sub_whales$id)

ssm<-fit_ssm(sub_whales,vmax=5,model="crw",time.step=2)

ssm_data<-grab(ssm,what="predicted")
ssm_data$id_old<-extract_prefix(ssm_data$id)

#### Vi plotter det
country<-ne_countries(scale = 50, type = "countries", returnclass = "sf")

p1<-plot_ssm(scale_x=c(-3,3))+ggtitle("170735",subtitle="2018")
p2<-plot_ssm(id_old="37230FB_03",scale_y=c(-0.5,0.5),from=c(xmin=-91.7,xmax=-90.9,ymin=71.55,ymax=71.8),to=c(xmin=-90,xmax=-87.8,ymin=69.3,ymax=70),sub_plot=TRUE)+
  ggtitle("37230FB_03",subtitle="2003")#This is interesting with the rerouting
grid.arrange(p1,p2,nrow=1)

  
##### Vi simulerer


#We take the ssm and find out which ones we can simulate from and save the index in the ssm
error_index<-c()
for(i in 1:nrow(ssm)) {
  print(i)
  tryCatch({
    withTimeout({
      sim <- sim_post(ssm[i,], reps=1)
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
#error_index<-c(160,163,220,234,238,353,359,369)
error_id<-ssm[error_index,]$id

#We lose some data here and should report that accurately
sim<-sim_post(ssm[-error_index,],reps=10)

#### Vi plotter simulationen
p3<-plot_sim(scale_y=c(-0.1,0.1))+ggtitle("170753-13",subtitle="simulated")
p4<-plot_sim(id="37230FB_03-7",scale_x=c(-0.5,0.5))+ggtitle("37230FB_03-7",subtitle="simulated")


df <- data.frame(
  x = 1:10,
  y = runif(10),
  rep = as.factor(1:10))
plot_for_legend <- ggplot(df, aes(x, y, color = rep)) + geom_line()
legend<-get_legend(plot_for_legend)

grid.arrange(p3,p4,legend,widths = c(2, 2, 0.5),nrow=1)

#### Og vi rerouter

#In order to be able to use the reroute function, we need a sim_fit object
#So I convert a the sim_post object like this
sim_fit<-sim_fit(ssm[-error_index],reps=1)
sim_fit$sims<-sim$psims

sim_route<-route_path(sim_fit,append=FALSE,dist=200)

#### Her skal vi indsætte noget om id=7618_10-23 da det åbenbart er rent på land

p5<-plot_sim(sim_reroute,sub_whales,scale_y=c(-0.1,0.1))+ggtitle("170753-13",subtitle="rerouted")
grid.arrange(p5,legend,widths=c(6,0.5),nrow=1) #Since we don't include the legend in plot_sim

p6<-plot_ssm(id_old="37230FB_03",predict=reroute_data,whale=sub_whales,sub_plot=FALSE)
p6


#### Vi laver de to html filer

#We lose some data here, namely a single split and 545 predictions across all simulations 
#These are predictions which were on land, and which is mostly due to an observed
#split on land
reroute_data<-sim_to_data(sim_route)
sim_data<-sim_to_data(sim_fit)

html1<-gen_html()
layout(html1,title="simulation")
html2<-gen_html(plot_data=plot_reroute)
layout(html2,title="reroute")

##### Fitter mp til reroutede predikationer
#This is not the most obvious way to extract the rep=0 data, but it works.
route_list<-data_to_list(reroute_data)
data_for_mpm<-as.data.frame(route_list[1])
data_for_mpm<-data_for_mpm[,c("id","date","lon","lat")]

mpm0<-fit_mpm(for_mpm,model="mpm",coords=3:4)

#We lose some data here, and should report how much that is
mpm0$track_length<-numeric(nrow(mpm0))
for(i in 1:nrow(mpm0)){
  mpm0$track_length[i]<-nrow(sim_reroute$sims[i][[1]])/11
}

#### Vi tilføjer de nødvendige covariater
mpm0_data<-grab(mpm0[which(mpm0$converged==TRUE),])
mpm0_data$id_old<-extract_prefix(mpm0_data$id)
mpm0_data<-merge(mpm0_data,data_for_mpm,by=c("id","date"),all.x=TRUE)
#Disregard data_for_mpm from now on

#We decide to remove some overly persistent data, namely that with logit_g.se>1000
#lose some data
length(mpm0_data[which(mpm0_data$logit_g.se>1000),]$id)

mpm0_data<-filter(mpm0_data,logit_g.se<1000)
mpm0_data$SST<-addCov(mpm0_data,SST,type="extract")
mpm0_data$ZOOC<-addCov(mpm0_data,ZOOC,type="extract")
mpm0_data$SST_grad<-addCov(mpm0_data,SST,type="gradient")
mpm0_data$SST_dist<-addCov(mpm0_data,SST,type="contourDist")
mpm0_data$SSTgrad<-sqrt(mpm0_data$SST_grad[,1]^2+mpm0_data$SST_grad[,2]^2)
mpm0_data[which(is.na(mpm0_data$SST)),]$SST<-getCovAlt(mpm0_data[which(is.na(mpm0_data$SST)),],SST)
mpm0_data[which(is.na(mpm0_data$ZOOC)),]$ZOOC<-getCovAlt(mpm0_data[which(is.na(mpm0_data$ZOOC)),],ZOOC)