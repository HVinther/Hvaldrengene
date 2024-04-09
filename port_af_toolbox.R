library(reticulate)

cms_init<-function(){
  if(! "copernicus_data_retrieval" %in% reticulate::conda_list()$name){
    reticulate::conda_create(envname = "copernicus_data_retrieval",
                             packages = "copernicusmarine")
  }
  cat("You'll have to login to retrieve data from Copernicus Marine.\nYou can allways login in later or change you login by calling cms_login().")
  answer <- readline("Do you wish to login now?  [Y/n]")
  if(answer %in% c("Y","y","")){
    cms_login()
  }
}

cms_login<-function(){
  clist <- reticulate::conda_list()
  path<-clist$python[clist$name == "copernicus_data_retrieval"]
  path<-stringr::str_replace(path,".python","/copernicusmarine")
  system(paste(path,"login"))
}

cms_subset<-function(
    serviceId,
    productId,
    variabe,
    lon = c(-110,-45),
    lat = c(55,80),
    depth,
    out_dir = getwd())
{
  clist <- reticulate::conda_list()
  if(! "copernicus_data_retrieval" %in% reticulate::conda_list()$name){
    answer <- readline("cms not initialized. Do you wish to initialize it? [Y/n]")
    if(answer %in% c("Y","y","")){
      cms_init()
    } else {
      return()
    }
  }
  path<-clist$python[clist$name == "copernicus_data_retrieval"]
  path<-stringr::str_replace(path,".python","/copernicusmarine")
  system(path)
  
  command <- paste (path," subset -i", productId,                    
                    "-x", lon[1], "-X", lon[2],                  
                    "-y", lat[1], "-Y", lat[2],
                    "-t", date_min, "-T", delta,
                    "-z", depth[1], "-Z", depth[2],                    
                    variable, "-o", out_dir, "-f", out_name, 
                    "--force-download")
  
  cat(paste("======== Download starting on",date_min,"========"))
  
  cat(command)
  
  system(command, intern = TRUE)
}

cms_deinit<-function(){
  reticulate::conda_remove("copernicus_data_retrieval")
}