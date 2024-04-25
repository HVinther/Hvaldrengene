---
title: "Untitled"
author: "Hans, Emil, Hjalte"
date: "2023-05-03"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABIAAAASCAYAAABWzo5XAAAAWElEQVR42mNgGPTAxsZmJsVqQApgmGw1yApwKcQiT7phRBuCzzCSDSHGMKINIeDNmWQlA2IigKJwIssQkHdINgxfmBBtGDEBS3KCxBc7pMQgMYE5c/AXPwAwSX4lV3pTWwAAAABJRU5ErkJggg==
output: html_document
---
Loading the data
```{r,results='hide',message=FALSE, warning=FALSE}
Sys.setenv(LANG = "en")
Sys.setlocale("LC_TIME", "English")
library(tidyverse)
library(lubridate)
library(sf)
library(sp)
library(proj4)
setwd("C:/Users/hjalt/OneDrive/Skrivebord/Whale data")
old_whale_loc<-read_csv("Bowhead estimated_20160901.csv")
new_whale_loc<-read_csv("2017_170750-1-FastGPS.csv",skip=3)
new_whale_loc<-rbind(new_whale_loc,read_csv("2017_170751-1-FastGPS.csv",skip=3))
new_whale_loc<-rbind(new_whale_loc,read_csv("2017_170752-1-FastGPS.csv",skip=3))
new_whale_loc<-rbind(new_whale_loc,read_csv("2017_170753-1-FastGPS.csv",skip=3))
new_whale_loc<-rbind(new_whale_loc,read_csv("2017_170754-1-FastGPS.csv",skip=3))
new_whale_loc<-rbind(new_whale_loc,read_csv("2018_170750-2-FastGPS.csv",skip=3))
new_whale_loc<-rbind(new_whale_loc,read_csv("2018_170751-1-FastGPS.csv",skip=3))
new_whale_loc<-rbind(new_whale_loc,read_csv("2018_170752-2-FastGPS.csv",skip=3))
new_whale_loc<-rbind(new_whale_loc,read_csv("2018_170753-1-FastGPS.csv",skip=3))
new_whale_loc<-rbind(new_whale_loc,read_csv("2018_170754-1-FastGPS.csv",skip=3))
new_whale_loc<-rbind(new_whale_loc,read_csv("2018_170833-2-FastGPS.csv",skip=3))
new_whale_loc<-rbind(new_whale_loc,read_csv("2018_170834-1-FastGPS.csv",skip=3))
new_whale_loc<-rbind(new_whale_loc,read_csv("2018_170835-1-FastGPS.csv",skip=3))
new_whale_loc<-rbind(new_whale_loc,read_csv("2018_170837-1-FastGPS.csv",skip=3))
new_whale_loc<-rbind(new_whale_loc,read_csv("2019_170750-1-FastGPS.csv",skip=3))
new_whale_loc<-rbind(new_whale_loc,read_csv("2019_170751-1-FastGPS.csv",skip=3))
new_whale_loc<-rbind(new_whale_loc,read_csv("2019_170752-1-FastGPS.csv",skip=3))
new_whale_loc<-rbind(new_whale_loc,read_csv("2019_170754-1-FastGPS.csv",skip=3))
```

Only using the relevant columns and renaming them
```{r,message=FALSE}
new_whale_loc<-subset(new_whale_loc,select=c(Name,Day,Time,Latitude,Longitude))
new_whale_loc<-na.omit(new_whale_loc)
new_whale_loc<-new_whale_loc[order(new_whale_loc$Name),]
names(new_whale_loc) <- c("ID", "Day", "Time", "lat", "lon")
new_whale_loc$lc <- "G"

old_whale_loc<-subset(old_whale_loc,select = c(id,date,estimatedLat,estimatedLon,lc))
names(old_whale_loc) <- c("ID", "time", "lat", "lon", "lc")
```

Change to Cartesian coordianates instead of lon lat
```{r}
llcoord <- st_as_sf(new_whale_loc[, c("lon", "lat")], coords = c("lon", "lat"), 
                    crs = CRS("+proj=longlat +datum=WGS84"))
laeacoords <- st_transform(llcoord, 3574)

# Add Easting-Northing to data (in km)
new_whale_loc[, c("x", "y")] <- st_coordinates(laeacoords)/1000


llcoord <- st_as_sf(old_whale_loc[, c("lon", "lat")], coords = c("lon", "lat"), 
                    crs = CRS("+proj=longlat +datum=WGS84"))
laeacoords <- st_transform(llcoord, 3574)

# Add Easting-Northing to data (in km)
old_whale_loc[, c("x", "y")] <- st_coordinates(laeacoords)/1000

### adding date+time as POSIXct to the new whales
new_whale_loc$Day <- as.Date(new_whale_loc$Day, format = "%d-%B-%Y")
new_whale_loc$Time <- as.character(new_whale_loc$Time)
new_whale_loc$time <- as.POSIXct(paste(new_whale_loc$Day, new_whale_loc$Time), format="%Y-%m-%d %H:%M:%S")
new_whale_loc <- subset(new_whale_loc, select = -c(Time,Day))

rm(llcoord,laeacoords)
```

Creating the swimspeed filter
```{r}
#creating the swimspeed filter function where S is the dataframe, s is the maximum speed for all points (used to remove obvious outliers), s2 is the maximum speed for points with a travel time above h (where h is in hours)  
Swimspeedfilter <- function(S, s, s2, h){
  S$date_hours <- as.numeric(S$time)/3600
  L <- nrow(S)
  l <- 1
  for (N in 1:500){
    if (l != L){
        S$dist <- NA
        for (i in 1:(nrow(S)-1)){
          if (S$ID[i] == S$ID[i+1]){
            S$dist[i+1] <- sqrt(  (S$x[i+1]-S$x[i])^2  +  (S$y[i+1]-S$y[i])^2)
          }
        }
        S$speed <- NA
        for (t in 1:(nrow(S)-1)){
          if (S$ID[t] == S$ID[t+1]){
            S$speed[t+1] <- S$dist[t+1]/(S$date_hours[t+1]-S$date_hours[t])
          }
        }
        k <- 1
        for (n in 1:nrow(S)){
          k <- k+1
          if ((is.na(S$speed[k]) == FALSE & S$date_hours[k]-S$date_hours[k-1] > h & S$speed[k] > s2)){
            S <- S[-c(k),]
            k <- k+10
            }
          else if (is.na(S$speed[k]) == FALSE & S$speed[k] > s){
            S <- S[-c(k),]
            k <- k+10
          }
        }
      l <- L
      L <- nrow(S)
      cat("Itteration", N, ":", "Removed", l-L,  "points", fill = TRUE)
    }
    else {break}
  }
  S = subset(S, select = -c(date_hours) )
  return(S)
  
}




new_whale_loc <- Swimspeedfilter(new_whale_loc, 30, 10, 0.5)
old_whale_loc <- Swimspeedfilter(old_whale_loc, 30, 10, 0.5)

whaledata <- rbind(new_whale_loc,old_whale_loc)

rm(new_whale_loc,old_whale_loc,Swimspeedfilter)

```

adding divetimes to the data
```{r}
whaledata$date_hours <- as.numeric(whaledata$time)/3600
whaledata$divetime <- NA
for (i in 1:(nrow(whaledata)-1)){
          if (whaledata$ID[i] == whaledata$ID[i+1]){
            whaledata$divetime[i+1] <- whaledata$date_hours[i+1]-whaledata$date_hours[i]
          }
        }
whaledata <- subset(whaledata, select = -c(date_hours))


#removing (almost)duplicate points
x <- rep(NA, nrow(whaledata))
for (i in 1:(nrow(whaledata))){
  if (is.na(whaledata$divetime[i]) == FALSE & whaledata$divetime[i]==0){
    x[i] <- i
  }
}

x <- x[!is.na(x)]
whaledata <- whaledata[-x,]
```


Splitting tracks
```{r}
split_at_gap <- function(data, max_gap = 60, shortest_track = 0) {
    # Number of tracks
    n_tracks <- length(unique(data$ID))
    
    # Save old ID and reinitialise ID column
    data$ID_old <- data$ID
    data$ID <- character(nrow(data))
    
    # Loop over tracks (i.e., over IDs)
    for(i_track in 1:n_tracks) {
        # Indices for this track
        ind_this_track <- which(data$ID_old == unique(data$ID_old)[i_track])
        track_length <- length(ind_this_track)
        
        # Time intervals in min
        dtimes <- difftime(data$time[ind_this_track[-1]], 
                           data$time[ind_this_track[-track_length]],
                           units = "mins")
        
        # Indices of gaps longer than max_gap
        ind_gap <- c(0, which(dtimes > max_gap), track_length)
        
        # Create new ID based on split track
        subtrack_ID <- rep(1:(length(ind_gap) - 1), diff(ind_gap))
        data$ID[ind_this_track] <- paste0(data$ID_old[ind_this_track], "-", subtrack_ID)
    }
    
    # Only keep sub-tracks longer than some duration
    track_lengths <- sapply(unique(data$ID), function(id) {
        ind <- which(data$ID == id)
        difftime(data$time[ind[length(ind)]], data$time[ind[1]], units = "min")
    })
    ID_keep <- names(track_lengths)[which(track_lengths >= shortest_track)]
    data <- subset(data, ID %in% ID_keep)
    
    return(data)
}
whaledata <- split_at_gap(data = whaledata, max_gap =6*60)
rm(x,split_at_gap)
```




### multiple imputations
```{r, message=FALSE,warning=FALSE}
library(foieGras)
library(tidyverse)
library(lubridate)
library(crawl)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthhires)
library(sf)
```


```{r}
whale_data <- whaledata
#whale_data<-read.table("dataWithEMF.txt",sep=",",header=TRUE)
whale_data<-subset(whale_data,month(whale_data$time)>=5)
whale_data<-subset(whale_data,month(whale_data$time)<=9)
whale_data<-whale_data[!(whale_data$lc=="Z"),]
rm(whaledata)
```

```{r}
unique_IDs<-data.frame("ID"=unique(whale_data$ID),"length"=rep(NA,length(unique(whale_data$ID))))
for (i in 1:nrow(unique_IDs)){
  unique_IDs$length[i]<-nrow(subset(whale_data,ID==unique_IDs$ID[i]))
}
```
In order to have a chance of getting good fits to all the tracks, we remove tracks of length 30 or less.
```{r}
index_set<-subset(unique_IDs,length<=30)$ID
whale_data<-subset(whale_data,!ID%in%index_set)
```


```{r}
whale_data<-data.frame("id"=whale_data$ID,"date"=as.POSIXlt(whale_data$time),"lc"=whale_data$lc,"x"=whale_data$x*1000,"y"=whale_data$y*1000, "id_old"=whale_data$ID_old) #Dataframe is in kilometers under projection, so is converted to meters
whale_data$lc<-factor(whale_data$lc,levels=c("G","3","2","1","0","A","B"))
```



```{r}
IDs<- c(unique(whale_data$id)) # unique id's
seeds2 <- rep(NA,length(IDs)) # place seeds that works for the specific whale
fixPar = c(log(50),log(250),log(500),log(1500),rep(log(2500),3),NA,NA)
```


```{r warning=FALSE, message=FALSE}
for (t in (1):(1)){
for (i in 1:length(IDs)){
if (is.na(seeds2[i])){
  try(
  {seed <- t
  set.seed(seed)
  test_set <- subset(whale_data,id==IDs[i])
  
  fit <- crwMLE(data=test_set,err.model=list(x=~lc-1),coords=c("x","y"),theta = c(14,-14),fixPar = fixPar,Time.name="date", attempts = 500)
  predTime <- seq((min(test_set$date)), 
                (max(test_set$date)), 3600)

  predObj <- crwPredict(object.crwFit=fit, 
                      predTime = predTime)

  simObj <- crwSimulator(fit, 
                       predTime, 
                       method="IS",
                       parIS = 100)
  seeds2[i] <- seed
  }, silent = T)
}
}
}
```


```{r}
seeds <- seeds2
x <- rep(NA,length(seeds))
for (i in 1:length(IDs)){
  if (is.na(seeds2[i])==FALSE){
    x[i] <- i
  }
}
x<- na.omit(x)
X <- as.character(IDs[x])
seeds <- na.omit(seeds)
seeds <- as.numeric(seeds)


k <- 0
for (i in 1:length(x)){
  k <- k + nrow(subset(whale_data,id==X[i]))
}
```


```{r warning=FALSE, message=FALSE}
iter <- 10
id <- NA
date <- NA
mux <- NA
muy <- NA
mux1 <- NA
mux2 <- NA
mux3 <- NA
mux4 <- NA
mux5 <- NA
mux6 <- NA
mux7 <- NA
mux8 <- NA
mux9 <- NA
mux10 <- NA
muy1 <- NA
muy2 <- NA
muy3 <- NA
muy4 <- NA
muy5 <- NA
muy6 <- NA
muy7 <- NA
muy8 <- NA
muy9 <- NA
muy10 <- NA
id_old<- NA
locType<-NA

df <- data.frame(id,mux1,muy1,mux2,muy2,mux3,muy3,mux4,muy4,mux5,muy5,mux6,muy6,mux7,muy7,mux8,muy8,mux9,muy9,mux10,muy10,date,mux,muy,id_old,locType)
n <- 0
for (i in 1:length(X)){
  try({
  seed <- seeds[i]
  set.seed(seed)
  test_set <- subset(whale_data,id==X[i])
  
  fit <- crwMLE(data=test_set,err.model=list(x=~lc-1),coords=c("x","y"),fixPar = fixPar,Time.name="date", attempts = 500)
  predTime <- seq.POSIXt(min(test_set$date), 
                (max(test_set$date)), '1 hour')
  
  predObj <- crwPredict(object.crwFit=fit, 
                      predTime = predTime)

  simObj <- crwSimulator(fit, 
                       predTime, 
                       method="IS",
                       parIS = 100)
  for(t in 1:iter){
  samp <- crwPostIS(simObj)
  k <- length(samp$alpha.sim[,'mu.x'])
  df[(n+1):(k+n),(t*2)] <- samp$alpha.sim[,'mu.x']
  df[(n+1):(k+n),(t*2+1)] <- samp$alpha.sim[,'mu.y']
  }
  
  
  df[(n+1):(k+n),1] <- X[i]
  
  df[(n+1):(k+n),22] <- predObj$date
  df[(n+1):(k+n),23] <- predObj$mu.x
  df[(n+1):(k+n),24] <- predObj$mu.y
  df[(n+1):(k+n),25] <- test_set$id_old[1]
  df[(n+1):(k+n),26] <- predObj$locType
  
  
  
  n <- n+k} ,silent = T)
}

df[,22] <- as.POSIXct(df[,22], origin='1970-01-01')
```


```{r}

df_create <- function(ux,uy,id=df$id,date=df$date,id_old=df$id_old,locType=df$locType){
  x <- ux
  y <- uy
  return(data.frame(id,id_old,date,locType,x,y))
}

df1 <- df_create(ux = df$mux1,uy = df$muy1)
df2 <- df_create(ux = df$mux2,uy = df$muy2)
df3 <- df_create(ux = df$mux3,uy = df$muy3)
df4 <- df_create(ux = df$mux4,uy = df$muy4)
df5 <- df_create(ux = df$mux5,uy = df$muy5)
df6 <- df_create(ux = df$mux6,uy = df$muy6)
df7 <- df_create(ux = df$mux7,uy = df$muy7)
df8 <- df_create(ux = df$mux8,uy = df$muy8)
df9 <- df_create(ux = df$mux9,uy = df$muy9)
df10 <- df_create(ux = df$mux10,uy = df$muy10)

imp_list <- list(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10)
imp_list <- lapply(imp_list,function(df){filter(df, locType == "p") })

rm(predObj,samp,simObj,test_set,unique_IDs,df,df1,df10,df2,df3,df4,df5,df6,df7,df8,df9,fit,whale_data,date,fixPar,id,id_old,IDs,index_set,iter,k,locType,mux1,mux,mux2,mux3,mux4,mux5,mux6,mux7,mux8,mux9,mux10,muy1,muy,muy2,muy3,muy4,muy5,muy6,muy7,muy8,muy9,muy10,n,predTime,seed,seeds,seeds2,t,X,x,df_create)
```


##testing






### Rerouting and adding covariates:
```{r}
library(raster)
library(tidyverse)
library(sf)
library(ncdf4)
library(ctmcmove)
library(sfheaders)
library(sfnetworks)
library(pathroutr)
library(reshape2)

SST<-stack('Data/SST_stack.nc')
ZOOC<-stack("Data/ZOOC_stack.nc")
gebco<-raster("Data/GEBCO/gebco_2022_n80.0_s50.0_w-110.0_e-45.0.nc",varname = "elevation")
Z<-readRDS("Data/Z.rds")


getcoord<-function(imp){
  coord = imp %>% 
    sf::st_as_sf(coords = c("x","y"), crs = 3574) %>% 
    sf::st_transform(crs = 4326) %>% 
    sf::st_coordinates() %>%
    dplyr::as_tibble() %>%
    dplyr::select(X,Y)
  names(coord) <- c("lon","lat")
  out <- imp  %>%
    dplyr::bind_cols(coord)
  return(out)
}

imputed_paths<-lapply(imp_list,getcoord)

##subfunction for adding covariate from raster at each obs
getCov<-function(Data,raster, printFlag = TRUE){
  nlayers<-nlayers(raster)
  Ncol<-raster::ncol(raster)
  Nrow<-raster::nrow(raster)
  if(nlayers == 1){
    cov<-raster::extract(raster, cbind(Data$lon,Data$lat))
    return(cov)
  }
  midday <-paste(as.character(trunc(Data$date, "day")), "12:00:00", sep = " ")
  cov<-numeric(nrow(Data))
  for(i in 1:length(Z)){
    if(printFlag){print(i)}
    index<-which(midday == Z[i])
    if(length(index) == 0){next}
    value<-raster::extract(raster, cbind(Data$lon[index],Data$lat[index]), layer = i, nl = 1)[,1]
    cov[index]<-value
  }
  return(cov)
}

##subfunction for calculating distance to contour of a given level at each obs
distanceToContour<-function(Data,raster, Level, printFlag = TRUE){
  nlayers<-nlayers(raster)
  if(nlayers == 1){
    spl<-rasterToContour(raster, levels = Level)
    point<-Data %>% st_as_sf(coords = c("lon","lat"), crs = 4326)
    spl_sf<-fortify(spl)%>% st_as_sf(coords = c("long","lat"), crs = 4326)
    dist<-st_distance(spl_sf,point)
    cov<-apply(dist,2,min)
    return(cov)
  }
  midday <-paste(as.character(trunc(Data$date, "day")), "12:00:00", sep = " ")
  cov<-numeric(nrow(Data))
  for(i in 1:length(Z)){
    if(printFlag){print(i)}
    index<-which(midday == Z[i])
    if(length(index) == 0){next}
    spl<-rasterToContour(raster[[i]], levels = Level)
    point<-Data[index,] %>% st_as_sf(coords = c("lon","lat"), crs = 4326)
    spl_sf<-fortify(spl)%>% st_as_sf(coords = c("long","lat"), crs = 4326)
    dist<-st_distance(spl_sf,point)
    cov[index]<-apply(dist,2,min)
    na.index<-which(is.na(cov))
  }
  return(cov)
}

##subfunction for calculating covariate gradients at each obs
getGrad<-function(Data,raster, printFlag = TRUE){
  nlayers<-nlayers(raster)
  if(nlayers == 1){
    grad<-ctmcmove::rast.grad(raster)
    grad.x<-grad$rast.grad.x
    grad.y<-grad$rast.grad.y
    cov.x<-raster::extract(grad.x, cbind(Data$lon,Data$lat))
    cov.y<-raster::extract(grad.y, cbind(Data$lon,Data$lat))
    return(cbind("lon"=cov.x,"lat"=cov.y))
  }
  midday <-paste(as.character(trunc(Data$date, "day")), "12:00:00", sep = " ")
  cov<-cbind("lon"=numeric(nrow(Data)),"lat"=numeric(nrow(Data)))
  for(i in 1:length(Z)){
    if(printFlag){print(i)}
    index<-which(midday == Z[i])
    if(length(index) == 0){next}
    grad<-ctmcmove::rast.grad(raster[[i]])
    grad.x<-grad$rast.grad.x
    grad.y<-grad$rast.grad.y
    cov.x<-raster::extract(grad.x, cbind(Data$lon[index],Data$lat[index]))
    cov.y<-raster::extract(grad.y, cbind(Data$lon[index],Data$lat[index]))
    cov[index,]<-cbind(cov.x,cov.y)
  }
  return(cov)
}

##main function for adding covariate of desired type
addCov<-function(Data, raster, type = "extract", Level = 0, printFlag = TRUE){
  if(type == "extract"){
    return(getCov(Data,raster, printFlag))
  }
  if(type == "contourDist"){
    return(distanceToContour(Data,raster,Level, printFlag))
  }
  if(type == "gradient"){
    return(getGrad(Data,raster, printFlag))
  }
  print(paste("no method for",type, "since", type, "not extract, contourDist nor gradient."))
}

##try_reroute function
try_reroute<-function(Data,barrier,vis_graph){
  names<-colnames(Data)
  sf_data<-Data %>%  st_as_sf(coords = c("lon","lat"))%>% sf::st_set_crs(4326) %>%st_transform(3574)
  id<-Data$id%>%unique()%>%as.character()
  updated_data<- data.frame()
  for(i in id){
    print(i)
    try(rm(trimmed))
    try(rm(updated))
    try(assign("trimmed",prt_trim(filter(sf_data, id == i),sfworld)))
    try(assign("updated",prt_update_points(prt_reroute(trimmed,barrier,vis_graph),trimmed)))
    try(assign("updated_data",rbind(updated_data,updated)))
  }
  updated_data[,c("lon","lat")]<-updated_data%>%st_transform(4326)%>%st_coordinates()
  return(data.frame(updated_data)[,names])
}

##creation of barrier and vis_graph for pathrouting
world<-map_data("world", region = c("Canada","Greenland"))
temp<-st_as_sf(world, coords = c("long","lat"))%>%sf::st_set_crs(4326)%>%sf::st_transform(crs = 3574) %>% st_coordinates()
world[,c("x","y")]= temp
sfworld<-sfc_polygon(world, x=7, y=8, polygon_id = 3)
sfworld<-sfworld%>%sf::st_set_crs(3574)
vis_graph<-pathroutr::prt_visgraph(sfworld)

## function for estimating covariates in NA-cells
getCovAlt<-function(Data,raster, printFlag = TRUE){
  nlayers<-nlayers(raster)
  Ncol<-raster::ncol(raster)
  Nrow<-raster::nrow(raster)
  midday <-paste(as.character(trunc(Data$date, "day")), "12:00:00", sep = " ")
  cov<-numeric(nrow(Data))
  for(i in 1:length(Z)){
    if(printFlag){print(i)}
    index<-which(midday == Z[i])
    if(length(index) == 0){next}
    value<-raster::extract(raster, cbind(Data$lon[index],Data$lat[index]), layer = i, nl = 1)[,1]
    cov[index]<-value
    na.index<-which(is.na(value))
    if(length(na.index)>0)
    {
      for(j in na.index){
        if(printFlag){print(paste("there is no covariate value at obs", j,"- calculating mean of adjacent cell values."))}
        row<-raster::rowFromY(raster, Data$lat[index[j]])
        col<-raster::colFromX(raster,Data$lon[index[j]])
        for(l in 1:20){
          print(l)
          row_start<-max(row-l,0)
          row_end<-min(row+l,Nrow)
          nrows <- row_end-row_start+1
          col_start<-max(col-l,0)
          col_end<-min(col+l, Ncol)
          ncols<-col_end-col_start+1
          values<-raster::getValuesBlock(raster, row = row_start, nrows = nrows, col = col_start, ncols = ncols, lyrs = i)
          mean <- mean(na.omit(values))
          if(!is.na(mean)){break}
        }
        cov[index[j]] <- mean
      }
    }
  }
  return(cov)
}

## rerouting paths
rr_list<-list()

# for(i in 1:length(imputed_paths)){
#   message(paste("beginning",i))
#   rr_list[[i]]<-try_reroute(Data = imputed_paths[[i]], barrier = sfworld, vis_graph = vis_graph)
# }



rr_list<-parallel::mclapply(imputed_paths, 
                    function(imp){try_reroute(Data = imp, barrier = sfworld,vis_graph = vis_graph)},
                    mc.cores = max(parallel::detectCores()-1,1))


## listing covariates to add
CovariateList<-list("SST"=list(SST,"extract",0),
                    "SSTgrad"=list(SST,"gradient",0),
                    "ZOOC"=list(ZOOC,"extract",0))

## adding covariates
data_list<-list()
for(j in 1:length(rr_list)){
  data_temp<-rr_list[[j]]
  for(i in 1:length(CovariateList)){
    print(names(CovariateList)[i])
    list<-CovariateList[[i]]
    data_temp[,names(CovariateList)[i]]=addCov(Data = data_temp,raster = list[[1]],type =list[[2]],Level = list[[3]])
  }
  data_list[[j]]<-data_temp
}


## fixing gradient vector naming/structure
temp_list<-data_list

temp_list<-lapply(temp_list,
       function(x){
         cols<-data.frame("SSTgrad.lon" = x$SSTgrad[,1],"SSTgrad.lat" = x$SSTgrad[,2])
         df<-dplyr::select(x,-SSTgrad)
         out<-dplyr::bind_cols(df,cols)
         return(out)
})

## calculating angle and norm
temp_list<-lapply(temp_list, function(x){
    df<-data.frame("SSTangle" = atan2(x$SSTgrad.lat,x$SSTgrad.lon),
    "SSTnorm "= sqrt(x$SSTgrad.lat^2+x$SSTgrad.lon^2))
    dplyr::bind_cols(x,df)
  })


## Estimating values for NA-observations
for(i in 1:length(temp_list)){
    message(i)
    index_SST<-which(is.na(temp_list[[i]]$SST))
    index_ZOOC<-which(is.na(temp_list[[i]]$ZOOC))
    temp_list[[i]]$SST[index_SST]<-getCovAlt(temp_list[[i]][index_SST,],SST)
    temp_list[[i]]$ZOOC[index_ZOOC]<-getCovAlt(temp_list[[i]][index_ZOOC,],ZOOC)
}

imp_list<-temp_list
save(imp_list,file = "imp_list.Rdata")
```



Some names contain tracks from two different whales. This function fixes that
```{r}
name_func <- function(data){
data$id_old <- as.character(data$id_old)
x <- unique(data$id_old)
for (i in 1:length(x)){
test_set <- data[which(data$id_old==x[i]),]
 for (t in 1:nrow(test_set)){
   if (abs(as.numeric(test_set$date[1]) - as.numeric(test_set$date[t]) ) > 20000000){
      test_set$id_old[t:nrow(test_set)] <- paste0("A",test_set$id_old[1])
      data[which(data$id_old==x[i]),] <- test_set
      break
      }
    }
}
data$id_old <- as.factor(data$id_old)
return(data)
}
imp_list <- lapply(imp_list , name_func)
```





Splits SSTnorm into 3 categories and makes it a factor
```{r}
factor_func <- function(df){
  df$SSTnorm.[which(df$SSTnorm. < 1e-10)] <- 0
  x <- df$SSTnorm.[which(df$SSTnorm. != 0)]
  df$SSTnorm <- cut(df$SSTnorm.,
              breaks=c(-1,0,quantile(x,probs = c(0.5)),1),
              labels=c('0', '50', '100'))
  return(df)
}
imp_list <- lapply(imp_list, factor_func)
```





splitting the datasets in two based on if the whale is in the diskobay or have left it 
###Tidsskridt er ikke regulære efter split
```{r}
# use something like this instead
split_data_func1 <- function(df){
  df <- subset(df, lon > -55)

  unique_IDs<-data.frame("ID"=unique(df$id),"length"=rep(NA,length(unique(df$id))))
  for (i in 1:nrow(unique_IDs)){
    unique_IDs$length[i]<-nrow(subset(df,id==unique_IDs$ID[i]))
  }
  index_set<-subset(unique_IDs,length<3)$ID
  df<-subset(df,!id%in%index_set)
  df$id <- droplevels.factor(df$id)
return(df)
}

split_data_func2 <- function(df){
df <- subset(df, lon < -55)

unique_IDs<-data.frame("ID"=unique(df$id),"length"=rep(NA,length(unique(df$id))))
for (i in 1:nrow(unique_IDs)){
  unique_IDs$length[i]<-nrow(subset(df,id==unique_IDs$ID[i]))
}
index_set<-subset(unique_IDs,length<3)$ID
df<-subset(df,!id%in%index_set)
df$id <- droplevels.factor(df$id)
return(df)
}

imp_list1 <- lapply(imp_list, split_data_func1)
imp_list2 <- lapply(imp_list, split_data_func2)


#example of how to make regular timesteps
# checkregtime <- function(df){
#   IDDs <- NA
#   IDs <- names(table(df$ID))
#   for (i in 1:length(IDs)) {
#     times <- df$time[which(df$ID==IDs[i])]
#     for (t in 1:(length(times)-1)){
#       if((as.numeric(times[t]) - as.numeric(times[t+1]) ) != -3600 ){
#         IDDs[i] <- IDs[i]
#       }
#     }
#   }
#   IDDs <- names(table(IDDs))
#   df <- subset(df, ! ID %in% IDDs)
#   df$id_old <- droplevels.factor(df$id_old)
#   df$ID <- droplevels.factor(df$ID)
#   return(df)
# }

imp_list1 <- lapply(imp_list1, checkregtime)
imp_list2 <- lapply(imp_list2, checkregtime)



coordfunc <- function(df){
  names(df)[names(df) == "x"] <- "UTMx"
  names(df)[names(df) == "y"] <- "UTMy"
  return(df)
}

imp_list1 <- lapply(imp_list1, coordfunc)
imp_list2 <- lapply(imp_list2, coordfunc)


```



adds DOY to dataframe and preps it using momentuHMM
```{r}
library(momentuHMM)
prepfunc <- function(df){
  colnames(df)[1] <- "ID"
  df$ID <- factor(df$ID)
  colnames(df)[3] <- "time"
unique_IDs<-data.frame("ID"=unique(df$ID),"length"=rep(NA,length(unique(df$ID))))
for (i in 1:nrow(unique_IDs)){
  unique_IDs$length[i]<-nrow(subset(df,ID==unique_IDs$ID[i]))
}
index_set<-subset(unique_IDs,length<3)$ID
df<-subset(df,!ID%in%index_set)
df$ID <- droplevels.factor(df$ID)

df$SSTangle[which(df$SSTangle >= pi)] <- pi
df$SSTangle[which(df$SSTangle <= -pi)] <- -pi+0.000001 

df <- momentuHMM::prepData(df, type = "LL", coordNames = c("lon", "lat"), angleCovs = "SSTangle")
df$SSTangle <- abs(df$SSTangle)
df$DOY <- yday(df$time)

#removes all tracks containing points wiith step lengths over 20
IDs <- df$ID[which(df$step > 20)]
IDs <- unique(IDs)
IDs <- droplevels.factor(IDs)
df <- subset(df, ! ID %in% IDs)
df$id_old <- droplevels.factor(df$id_old)
df$ID <- droplevels.factor(df$ID)
return(df)
}

imp_list1 <- lapply(imp_list1, prepfunc)
imp_list2 <- lapply(imp_list2, prepfunc)
```


