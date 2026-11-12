#this function will load rasters, add them to a list and mas and crop according to Europe
#population density is trimmed to have 1901 - 2024 data
#total pop is calculated
require(terra)

load_rasters <- function(raster_paths_list, area_raster_path = NULL) {

  #contour_vect <- vect(contour_path)
  
  covariates <- list()
  for (name in names(raster_paths_list)) {
    paths <- raster_paths_list[[name]]
    rasters <- lapply(paths, rast)
    
    if (length(rasters) > 1) {
      cov <- rasters[[1]]
      for (i in 2:length(rasters)) {
        cov <- c(cov, rasters[[i]])
        }
      }else {
        cov <- rasters[[1]]
      }
    
    if (name == "populationDensity") {
      cov <- cov[[2:125]]
    }
    
    covariates[[name]] <- cov
    
    if (!is.null(area_raster_path) && name == "populationDensity") {
      areas <- rast(area_raster_path);areas <- areas / (1000*1000)  # convert m² → km²
      totalPopulation <- cov * areas
      covariates[["totalPopulation"]] <- totalPopulation
    }
    # crop and mask to Europe
    #cov <- crop(cov, contour_sp, snap = "out")
    #cov <- mask(cov, mask = contour_sp)

  }
  #covariates <- lapply(covariates, crop, contour_vect, snap="out")
  #covariates <- lapply(covariates, mask, contour_vect) 
  return(covariates)
}
