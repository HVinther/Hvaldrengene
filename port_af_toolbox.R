library(reticulate)

cmw_init<-function(){
  if(! "copernicus_data_retrieval" %in% reticulate::conda_list()$name){
    reticulate::conda_create(envname = "copernicus_data_retrieval",
                             packages = "copernicusmarine")
  }
  message("Note:You'll have to login to retrieve data from Copernicus Marine.")
}

cmw_login<-function(username,password){
  clist <- reticulate::conda_list()
  if(! "copernicus_data_retrieval" %in% reticulate::conda_list()$name){
    stop("cmw not initialized. Initialize it with cmw_init().")
  }
  path<-clist$python[clist$name == "copernicus_data_retrieval"]
  path<-stringr::str_replace(path,".python","/copernicusmarine")
  system2(path,args = "login", input = c(username,password,"y"))
}

cmw_subset<-function(
    serviceId,
    productId,
    variable,
    dates,
    lon = c(-110,-45),
    lat = c(55,80),
    depth,
    out_name = paste(c("cmw_file_",sample(c(letters,1:10),20,TRUE),".nc"),collapse =""),
    out_dir = getwd())
{
  clist <- reticulate::conda_list()
  if(! "copernicus_data_retrieval" %in% reticulate::conda_list()$name){
    stop("cmw not initialized. Initialize it with cmw_init().")
  }
  path<-clist$python[clist$name == "copernicus_data_retrieval"]
  path<-stringr::str_replace(path,".python","/copernicusmarine")
  
  if(substr(out_name,1,1) == "/"){
    out_name <- substr(out_name,2,nchar(out_name))
  }
  
  dir_len <- nchar(out_dir)
  if(substr(out_dir,dir_len,dir_len) == "/"){
    out_dir <- substr(out_dir,dir_len -1,dir_len -1)
  }
  
  tmp_name <- paste(c("cmw_tmp_",sample(c(letters,1:10),20,TRUE),".nc"),collapse ="")
  
  command <- paste (path," subset -i", productId,                    
                    "-x", lon[1], "-X", lon[2],                  
                    "-y", lat[1], "-Y", lat[2],
                    "-t", dates[1], "-T", dates[2],
                    "-z", depth[1], "-Z", depth[2],                    
                    "--variable", variable, "-o", out_dir, "-f", tmp_name, 
                    "--force-download")
  
  cat("======== Download starting ========\n")
  
  cat(paste(command,"\n"))
  
  system(command, intern = TRUE)
  
  cat("Cleaning up \n")
  
  full_file_name <- paste(out_dir,out_name,sep ="/")
  
  if(file.exists(full_file_name)){
    file.remove(full_file_name)
  }
  
  file.rename(paste(out_dir,tmp_name,sep = "/"),full_file_name)
  
  return(full_file_name)
  
}

cmw <- function(...){
  clist <- reticulate::conda_list()
  if(! "copernicus_data_retrieval" %in% reticulate::conda_list()$name){
    stop("cmw not initialized. Initialize it with cmw_init().")
  }
  path<-clist$python[clist$name == "copernicus_data_retrieval"]
  path<-stringr::str_replace(path,".python","/copernicusmarine")
  system(paste(path,...))
}

cmw()

cmw_deinit<-function(){
  reticulate::conda_remove("copernicus_data_retrieval")
}