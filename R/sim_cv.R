library(aniMotum)
library(tidyverse)

# Vi indlæser sub_whales på samme måde som i analysen

whales<-read.csv("SwimSpeedFitleredWhaleData.txt") #Might have been moved to the Data folder

sub_whales<-function(whale_data){
  out<-whale_data
  names(out)[names(out) == "time"] <- "date"
  names(out)[names(out) == "ID"] <- "id"
  out<-out[,c(1:4,7)]
  out$id_old = extract_prefix(out$id)
  out
}

sim_cv<-function(data_set, ids, k = "loo"){
  ## Test if id's are valid and reports non-valid ids by erroring out
  id_test <- ids %in% unique(data_set$id)
  if(!all(id_test)){
    stop(paste("ids:", paste(ids[!id_test],collapse = ","), "not in data set"))
  }
  out <- purrr::map(
    ids,\(ID){
      test_set<-subset(data_set,id==ID)
      test_set<-test_set[order(test_set$date),]
      ## First observation is always needed, thus the provided number of groups is
      ## compared against number of rows minus 1.
      N_rows <- nrow(test_set)-1
      if(k == "loo"){ # standard setup leave-one-out cv
        k <- N_rows
      } else if(is.numeric(k)){
        k <- as.integer(k)
        if(k > N_rows){
          warning("k larger than number of rows -1. Defaulting to leave one out")
          k <- N_rows
        }
        if(k < 2){ # needs at least two groups for cv
          stop("k smaller than 2.")
        }
      } else {
        stop("k neither loo (leave one out) nor numeric")
      }
      
      group <- c(k+1,sample(rep(1:k,length.out=N_rows))) # cv groups are created
      pred_list<-purrr::map(1:k,\(i){
        #Needed to make predictions at specific time stamps
        time_stamps<-data.frame("id"=test_set[group==i,]$id,"date"=test_set[group==i,]$date)
        time_stamps$date<-as.POSIXct(time_stamps$date)
        
        #We fit the model without the omitted group and ask for predictions at the time-stamps from this group
        model<-aniMotum::fit_ssm(test_set[group!=i,],time.step=time_stamps,model="crw")
        
        #Then grab the predictions
        pred<-aniMotum::grab(model,what="predicted")
        tibble(date = pred$date,
               pred_lon = pred$lon,
               pred_lat = pred$lat)
      })
      pred_loc<-do.call(rbind,pred_list) # bind predictions
      pred_loc<-pred_loc[order(pred_loc$date),] # order by date
      
      ## Bind predictions to observed
      dplyr::mutate(test_set[-1,], # no prediction for the first observation
                    pred_lat = pred_loc$pred_lat,
                    pred_lon = pred_loc$pred_lon) 
    })
  do.call(rbind,out) # bind tibbles from different ids
}

out<-sim_cv(sub_whales(whales),ids = c("37280FB_03-2","170753-35"),k = 10)
out

## Calculates mean square distance between two dataframes of coordinate pairs
msdistance<-function(obs,pred){
  if(any(c(ncol(obs),ncol(pred))!= 2)){
   stop("either the number of rows of  obs or pred does equal 2.") 
  }
  if(nrow(obs)!=nrow(pred)){
    stop("obs and pred does not have same number of rows")
  }
  mean(
    sf::st_distance(
      sf::st_as_sf(obs, coords = names(obs)),
      sf::st_as_sf(pred, coords = names(pred)),
      by_element = TRUE)^2
  )
}
