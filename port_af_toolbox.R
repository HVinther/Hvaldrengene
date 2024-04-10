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

cms_login<-function(username,password){
  clist <- reticulate::conda_list()
  path<-clist$python[clist$name == "copernicus_data_retrieval"]
  path<-stringr::str_replace(path,".python","/copernicusmarine")
  system2(path,args = "login", input = c(username,password,"y"))
}

cms_subset<-function(
    serviceId,
    productId,
    variable,
    dates,
    lon = c(-110,-45),
    lat = c(55,80),
    depth,
    out_name = tempfile(tmpdir = "",fileext = ".nc"),
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
  
  if(substr(out_name,1,1) == "/"){
    out_name <- substr(out_name,2,nchar(out_name))
  }
  
  dir_len <- nchar(out_dir)
  if(substr(out_dir,dir_len,dir_len) == "/"){
    out_dir <- substr(out_dir,dir_len -1,dir_len -1)
  }
  
  command <- paste (path," subset -i", productId,                    
                    "-x", lon[1], "-X", lon[2],                  
                    "-y", lat[1], "-Y", lat[2],
                    "-t", dates[1], "-T", dates[2],
                    "-z", depth[1], "-Z", depth[2],                    
                    "--variable", variable, "-o", out_dir, "-f", out_name, 
                    "--force-download")
  
  cat("======== Download starting ========\n")
  
  cat(paste(command,"\n"))
  
  system(command, intern = TRUE)
  
  paste(out_dir,out_name,sep ="/")
}

cms_deinit<-function(){
  reticulate::conda_remove("copernicus_data_retrieval")
}