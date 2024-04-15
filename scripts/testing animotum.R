library(aniMotum)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(plotly)
library(R.utils)
library(bernr)
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

ssm_plot_data<-grab(ssm,what="predicted")

ssm_plot_data$id_old<-extract_prefix(ssm_plot_data$id)
sub_whales$id_old<-extract_prefix(sub_whales$id)

countries <- ne_countries(scale = 50, type = "countries", returnclass = "sf")

plot_ssm<-function(id_old="170753",
                   whale=sub_whales,
                   predict=ssm_plot_data,
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
p1<-plot_ssm(scale_x=c(-3,3),)+ggtitle("170735",subtitle="2018")

p2<-plot_ssm(id_old="37230FB_03",scale_y=c(-0.5,0.5),from=c(xmin=-91.7,xmax=-90.9,ymin=71.55,ymax=71.8),to=c(xmin=-90,xmax=-87.8,ymin=69.3,ymax=70),sub_plot=TRUE)+
  ggtitle("37230FB_03",subtitle="2003")#This is interesting with the rerouting
p2
grid.arrange(p1,p2,nrow=1)

temp<-ssm_plot_data[which(ssm_plot_data$id_old=="37230FB_03"),]
ggplot(temp[which(temp$id=="37230FB_03-7"),])+geom_point(mapping=aes(x=lon,y=lat))

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



plot_sim<-function(simu=sim,whale=sub_whales,id="170753-13",scale_x=c(0,0),scale_y=c(0,0)){
  for_plot_gps<-as.data.frame(simu[[3]][which(simu$id==id)])
  for_plot_gps$rep<-as.factor(for_plot_gps$rep)
  points<-whale[which(whale$id==id),]
  ggplot()+
    geom_sf(data=countries,fill="gray")+coord_sf(xlim=range(for_plot_gps$lon)+scale_x,ylim=range(for_plot_gps$lat)+scale_y)+
    geom_path(data=for_plot_gps[which(for_plot_gps$rep!=0),],mapping=aes(x=lon,y=lat,color=rep))+
    geom_path(data=for_plot_gps[which(for_plot_gps$rep==0),],mapping=aes(x=lon,y=lat),color="black")+
    geom_point(data=points,mapping=aes(x=lon,y=lat),color="blue",size=1)+theme(legend.position = "none")
}


p1<-plot_sim(scale_y=c(-0.1,0.1))+ggtitle("170753-13",subtitle="simulated")
p1
p2<-plot_sim(id="37230FB_03-7",scale_x=c(-0.5,0.5))+ggtitle("37230FB_03-7",subtitle="simulated")
p2

df <- data.frame(
  x = 1:10,
  y = runif(10),
  rep = as.factor(1:10))
plot_for_legend <- ggplot(df, aes(x, y, color = rep)) + geom_line()
legend<-get_legend(plot_for_legend)

grid.arrange(p1,p2,legend,widths = c(2, 2, 0.5),nrow=1)

## Turning it into a sim_fit object for the rerouting
sim_fit<-sim_fit(ssm[-error_index,])
sim_fit$sims<-sim$psims

sim_route<-route_path(sim_fit,append=FALSE,dist=200)
# id=7618_10-23 is strange since it is claimed to be on land, but it is not really?

saveRDS(sim_route,"sims_animotum_reroute")


## We also plot this 
reroute_data<-sim_to_data(sim_reroute)

p3<-plot_sim(sim_reroute,sub_whales,scale_y=c(-0.1,0.1))+ggtitle("170753-13",subtitle="rerouted")
grid.arrange(p3,legend,widths=c(6,0.5),nrow=1)

p4<-plot_ssm(id_old="37230FB_03",predict=reroute_data,whale=sub_whales,sub_plot=FALSE)

grid.arrange(p3,p4+ggtitle("37230FB_03",subtitle="rerouted"),nrow=1)


layout_matrix <- matrix(
  c(1, 2, 5,
    3, 4, 5),
  nrow = 2, byrow = TRUE
)

grid.arrange(p1,p2,p3,p4,legend,widths = c(2,2, 0.5),layout_matrix=layout_matrix,top=textGrob("Illustration of simulation and rerouting"))

extract_prefix<-function(values) {
  # Use a regular expression to remove the last dash and everything following it
  sapply(values, function(x) sub("-[^-]*$", "", x))
}


sim_to_data<-function(sim){ #Might need to adjust the sim$sims
  plot_data<-as.data.frame(sim[[3]][[1]])
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
countries <- ne_countries(scale = 50, type = "countries", returnclass = "sf")


gen_html<-function(plot_data=plot_straight,whale=sub_whales){
  if (!("date" %in% names(whale)) || !("id" %in% names(whale))) {
    stop("The 'whale' dataframe must contain 'date' and 'id' columns.")
  }
  
  whale<-whale[-which(whale$id%in%ssm$id[error_index]),]
  gg<-ggplot(whale)+
    geom_sf(data=countries,fill="gray")+coord_sf(xlim=range(plot_data$lon),ylim=range(plot_data$lat))+
    geom_path(plot_data[which(plot_data$rep!=0),],mapping=aes(x=lon,y=lat,color=rep,group=id))+
    geom_path(plot_data[which(plot_data$rep==0),],mapping=aes(x=lon,y=lat,group=id),color="black")+
    geom_point(whale,mapping=aes(x=lon,y=lat,text=paste("ID:", id, "<br>Date:", date)),color="blue",size=1)
  p<-ggplotly(gg,tooltip = "text")  
}

html1<-gen_html()

layout(html1,title="simulation")

html2<-gen_html(plot_data=plot_reroute)

layout(html2,title="reroute")

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

## Is the most appropriate model to simply combine all 10 simulations to the same dataset?

sim_reroute<-readRDS("rds/sims_animotum_reroute")

plot_reroute<-sim_to_data(sim_reroute)

route_list<-data_to_list(plot_reroute)

for_mpm<-data.frame("id"=route_list[[1]]$id,"date"=route_list[[1]]$date,
                    "lon"=route_list[[1]]$lon,
                    "lat"=route_list[[1]]$lat)

mpm0<-fit_mpm(for_mpm,model="mpm",coords=3:4)

## Add track length just to see where we have lack of convergence
### Divide by 11 because there are 10 sims and 1 prediction for each track.
mpm0$track_length<-numeric(nrow(mpm0))
for(i in 1:nrow(mpm0)){
  mpm0$track_length[i]<-nrow(sim_reroute$sims[i][[1]])/11
}



# Makes analysis with it (gam perhaps?)

mpm0_data<-grab(mpm0[which(mpm0$converged==TRUE),])
mpm0_data$id_old<-extract_prefix(mpm_data$id)

positions0<-filter(plot_reroute,rep==0)

positions0 <- select(positions0, id, date, lon, lat)

mpm0_data<-merge(mpm0_data,positions0,by=c("id","date"),all.x=TRUE)

## Some of these have very high standard errors on the logit scale, and so far I dont consider these
## It is 47 tracks with a total length of 553 so on average 11 data points each.
length(mpm0_data[which(mpm0_data$logit_g.se>1000),]$id)


mpm0_data<-filter(mpm0_data,logit_g.se<1000)
mpm0_data$SST<-addCov(mpm0_data,SST,type="extract")
mpm0_data$ZOOC<-addCov(mpm0_data,ZOOC,type="extract")
mpm0_data$SST_grad<-addCov(mpm0_data,SST,type="gradient")
mpm0_data$SST_dist<-addCov(mpm0_data,SST,type="contourDist")
mpm0_data$SSTgrad<-sqrt(mpm0_data$SST_grad[,1]^2+mpm0_data$SST_grad[,2]^2)
mpm0_data[which(is.na(mpm0_data$SST)),]$SST<-getCovAlt(mpm0_data[which(is.na(mpm0_data$SST)),],SST)
mpm0_data[which(is.na(mpm0_data$ZOOC)),]$ZOOC<-getCovAlt(mpm0_data[which(is.na(mpm0_data$ZOOC)),],ZOOC)




saveRDS(mpm0_data,file="mpm_data_test")

ggplot(mpm0_data)+geom_point(mapping=aes(x=SSTgrad,y=logit_g))

#linear model

mean_logit_g <- mean(mpm0_data$logit_g)
sd_logit_g <- sd(mpm0_data$logit_g)

p <- ggplot(mpm0_data, aes(x = logit_g)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, color = "black", fill = "gray") +
  stat_function(fun = dnorm, args = list(mean = mean_logit_g, sd = sd_logit_g), color = "red", size = 1) +
  labs(title = "Histogram with Normal Curve", x = "logit_g", y = "Density")

print(p)

## So it is not completely unreasonable to use a normal linear model


mpm_mod<-lm(logit_g~ns(SST,df=3)+ns(ZOOC,df=3)+ns(SST_dist,df=2),data=mpm0_data)
summary(mpm_mod)

mpm_mod_mixed<-lmer(logit_g~ns(SST,df=3)+ns(ZOOC,df=4)+ns(SST_dist,df=2),data=mpm0_data,REML=TRUE)
mpm_mod_mixed_nlme <- lme(fixed = logit_g ~ ns(SST, df=3) + ns(ZOOC, df=3) + ns(SST_dist, df=2), 
                          random = ~ 1 | id_old,
                          #correlation=corAR1(form=~date,fixed=TRUE),
                          data = mpm0_data, 
                          method = "REML")

summary(mpm_mod)

# SSTgrad is not really significant
# Do we want to include distance to coast?
# I feel like salinity would be fun as well

model<-mpm_mod_mixed
var

plot_model <- function(model=mpm_mod, data=mpm0_data, var = "SST") {
  if (!var %in% names(data)) {
    stop("The variable specified does not exist in the data frame.")
  }
  
  plot_linear_dat_list <- list()
  
  # Loop through each variable in the data
  
    for (col_name in names(data)) {
      if (col_name == var) {
        # If it's the variable of interest, create a sequence
        plot_linear_dat_list[[col_name]] <- seq(min(na.omit(data[[col_name]])), max(na.omit(data[[col_name]])), length = 100)
      } else if (class(data[[col_name]])[1] != "numeric")
        {next} 
      else {
        # Otherwise, calculate the mean of the column, omitting NA values
        plot_linear_dat_list[[col_name]] <- mean(na.omit(data[[col_name]]))
      }
    }
  

  
  # Convert the list to a data frame
  plot_linear_dat <- as.data.frame(plot_linear_dat_list)
  
  # preds <- predict(model, newdata = plot_linear_dat, se.fit = TRUE,re.form=NA)
  # plot_linear_dat$logit_g <- preds$fit
  # plot_linear_dat$lower_ci <- preds$fit - 1.96 * preds$se.fit #This ought to be a prediction interval tho?
  # plot_linear_dat$upper_ci <- preds$fit + 1.96 * preds$se.fit
  
  preds<-bolker_ci(mpm_mod_mixed_nlme,plot_linear_dat) #Using design matrix is not sensible
  # since we are just taking the average and this somehow messes with the design matrix

  
  
  # Convert string to symbol for ggplot
  var_sym <- sym(var)
  
  ggplot(plot_linear_dat, aes(x = !!var_sym, y = logit_g)) +
    geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), fill = "gray50") +
    geom_line(color = "black")
}

model<-mpm_mod_mixed_nlme

plot_model(model=mpm_mod_mixed,var="SST_dist")


## Temporary conclusion is that the linear model gives the nicest interpretation,
## while the mixed effects is obviously better fitted. In nlme we can adjust
## for autocorrelation, but cannot easily make confidence intervals. lmer4 easily makes
## confidence intervals but autocorrelation is harder


