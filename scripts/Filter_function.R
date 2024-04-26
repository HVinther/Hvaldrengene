#creating the swimspeed filter function where S is the dataframe,
#s is the maximum speed for all points (used to remove obvious outliers),
#s2 is the maximum speed for points with a travel time above h (where h is in hours)

Swimspeedfilter <- function(S, s, s2, h){
  S$date_hours <- as.numeric(S$time)/3600
  L <- nrow(S)
  l <- 1
  for (N in 1:500){ #Hvorfor 500 her?
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

create_whale_data<-function(old_whale_loc,new_whale_loc,what="estimated",filter=TRUE,min_data=30){
  #Use only the relevant data
  new_whale_loc<-subset(new_whale_loc,select=c(Name,Day,Time,Latitude,Longitude))
  new_whale_loc<-na.omit(new_whale_loc)
  new_whale_loc<-new_whale_loc[order(new_whale_loc$Name),]
  names(new_whale_loc) <- c("ID", "Day", "Time", "lat", "lon")
  new_whale_loc$lc <- "G"
  
  if(what=="estimated"){
    old_whale_loc<-subset(old_whale_loc,select = c(id,date,estimatedLat,estimatedLon,lc))
  }
  else if (what=="observed"){
    old_whale_loc<-subset(old_whale_loc,select = c(id,date,observedLat,observedLon,lc))
  }
  names(old_whale_loc) <- c("ID", "time", "lat", "lon", "lc")
  
  #Add the cartesian coordinates
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
  
  #Fixing the time format for new_whale_loc
  Sys.setlocale("LC_TIME", "C") #To make sure the 
  new_whale_loc$time <- as.POSIXct(paste(new_whale_loc$Day, new_whale_loc$Time), format="%d-%b-%Y %H:%M:%S")
  new_whale_loc <- subset(new_whale_loc, select = -c(Time,Day))
  
  if(filter){
    new_whale_loc <- Swimspeedfilter(new_whale_loc, 30, 10, 0.5)
    old_whale_loc <- Swimspeedfilter(old_whale_loc, 30, 10, 0.5)
  }
  
  whaledata <- rbind(new_whale_loc,old_whale_loc)
  return(whaledata)
}


split_at_gap <- function(data, max_gap = 60, shortest_track = 5) {
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
