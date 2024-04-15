makeZ<-function(file_list){
  file_list %>%
    purrr::map(raster::brick) %>%
    purrr::map(raster::getZ) %>%
    do.call(c,.)
}
 
make_stack <- function(
    file_list,
    out_name = paste(c("covariate_stack_",sample(c(letters,1:10),20,TRUE),".nc"),collapse =""),
    out_dir = getwd()
    )
  {

  dir_len <- nchar(out_dir)
  if(substr(out_dir,dir_len,dir_len) == "/"){
    out_dir <- substr(out_dir,dir_len -1,dir_len -1)
  }

  out_path <- file.path(out_dir,out_name)

  brick_list <- purrr::map(file_list,raster::brick)

  raster::writeRaster(
    raster::stack(brick_list),
    filename = out_path,
    format = "CDF",
    overwrite = T)

  return(out_path)
}