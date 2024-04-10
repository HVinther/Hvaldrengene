library(reticulate)

## Call cmw_init to set up the conda enviroment and install the Copernicus Marine Toolbox inside
cmw_init<-function(){
  if(! "copernicus_data_retrieval" %in% reticulate::conda_list()$name){
    reticulate::conda_create(envname = "copernicus_data_retrieval",
                             packages = "copernicusmarine")
  }
  message("Note:You'll have to login to retrieve data from Copernicus Marine.")
}

## To retrieve data from the Copernicus Marine Service you'll need to provide a log-in
## The account information is saved in a local file using the methods of the toolbox
## Thus a login is required only once
cmw_login<-function(username,password){
  clist <- reticulate::conda_list()
  if(! "copernicus_data_retrieval" %in% reticulate::conda_list()$name){
    stop("cmw not initialized. Initialize it with cmw_init().")
  }
  path<-clist$python[clist$name == "copernicus_data_retrieval"]
  path<-stringr::str_replace(path,".python","/copernicusmarine")
  system2(path,args = "login", input = c(username,password,"y"))
}

## Fetch data from the Copernicus Marine Service. By default written to a file with
## a randomly generated name inside the current working directory.
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
  
  tmp_name <- substr(tempfile(pattern = "tmp",fileext = ".nc",tmpdir = ""),2,20L)
  
  command <- paste (path," subset -i", productId,                    
                    "-x", lon[1], "-X", lon[2],                  
                    "-y", lat[1], "-Y", lat[2],
                    "-t", dates[1], "-T", dates[2])
  
  if(!missing(depth)){
    command<-paste(command,"-z", depth[1], "-Z", depth[2])
  }

  command <- paste(command ,        
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

## For advanced use, commands can be issued directly to the Toolbox cli.
## Commands should be in a character vector of length one, and be in
## shell/cli style.
cmw <- function(...){
  clist <- reticulate::conda_list()
  if(! "copernicus_data_retrieval" %in% reticulate::conda_list()$name){
    stop("cmw not initialized. Initialize it with cmw_init().")
  }
  path<-clist$python[clist$name == "copernicus_data_retrieval"]
  path<-stringr::str_replace(path,".python","/copernicusmarine")
  system(paste(path,...))
}

## Currently just removes the conda enviroment. 
cmw_deinit<-function(){
  reticulate::conda_remove("copernicus_data_retrieval")
}