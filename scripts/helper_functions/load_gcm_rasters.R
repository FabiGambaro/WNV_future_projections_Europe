load_gcm_rasters <- function(climatic_var, model_name, scenario){
  
  files_scenario <- sort(list.files(path = paste0("Rasters/Environmental_rasters/ISIMIP3b/", scenario, "/", model_name),
                                    pattern = paste0(climatic_var, ".*\\.nc$"), full.names = TRUE)) #chronological order!
  
  files_hist <- sort(list.files(path = paste0("Rasters/Environmental_rasters/ISIMIP3b/historical/", model_name),
                                pattern = paste0(climatic_var, ".*\\.nc$"), full.names = TRUE)) #chronological order
  
  files_hist <- files_hist[grepl("_(190[1-9]|19[1-9][0-9]|200[0-9]|201[0-4])_", files_hist)] #until 2014
  
  rast(c(files_hist, files_scenario)) #concatenate in one raster(SpatRaster)
}