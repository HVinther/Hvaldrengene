## 

make_barrier <- function(){
  world<-map_data("world", region = c("Canada","Greenland"))
  temp<-st_as_sf(world, coords = c("long","lat"))%>%sf::st_set_crs(4326)%>%sf::st_transform(crs = 3574) %>% st_coordinates()
  world[,c("x","y")]= temp
  sfworld<-sfc_polygon(world, x=7, y=8, polygon_id = 3)
  sfworld<-sfworld%>%sf::st_set_crs(3574)
  sfworld
}

##
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


##subfunction for adding covariate from raster at each obs
getCov<-function(Data,raster,Z, printFlag = TRUE){
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
distanceToContour<-function(Data,raster,Z, Level, printFlag = TRUE){
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
getGrad<-function(Data,raster,Z, printFlag = TRUE){
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
addCov<-function(Data, raster, type = "extract",Z, Level = 0, printFlag = TRUE){
  if(type == "extract"){
    return(getCov(Data,raster,Z, printFlag))
  }
  if(type == "contourDist"){
    return(distanceToContour(Data,raster,Z,Level, printFlag))
  }
  if(type == "gradient"){
    return(getGrad(Data,raster,Z,printFlag))
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

## function for estimating covariates in NA-cells
getCovAlt<-function(Data,raster,Z, printFlag = TRUE){
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