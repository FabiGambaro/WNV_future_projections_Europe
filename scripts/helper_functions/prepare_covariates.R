#this function will load rasters, add them to a list and mas and crop according to Europe
#population density is trimmed to have 1901 - 2024 data
#total pop is calculated

prepare_covariates <- function(contour_path, raster_paths_list, area_raster_path = NULL) {
  
  require(raster)
  require(rgdal)
  require(sf)
  
  contour_sf <- readRDS(contour_path) # load Europe contour and NUTS boundaries
  contour_sp <- as(contour_sf, "Spatial")
  
  covariates <- list()
  for (name in names(raster_paths_list)) {
    paths <- raster_paths_list[[name]]
    rasters <- lapply(paths, brick)
    if (length(rasters) > 1) {
      cov <- rasters[[1]]
      for (i in 2:length(rasters)) {
        cov <- addLayer(cov, rasters[[i]])
      }
    } else {
      cov <- rasters[[1]]
    }
    
    if (name == "populationDensity") {
      cov <- cov[[2:125]]
    }
    
    covariates[[name]] <- cov
    
    if (!is.null(area_raster_path) && name == "populationDensity") {
      areas <- brick(area_raster_path);areas <- areas / (1000*1000)  # convert m² → km²
      totalPopulation <- cov * areas
      covariates[["totalPopulation"]] <- totalPopulation
    }
    # crop and mask to Europe
    #cov <- crop(cov, contour_sp, snap = "out")
    #cov <- mask(cov, mask = contour_sp)

  }
  covariates <- lapply(covariates, crop, y = contour_sp, snap = "out")
  covariates <- lapply(covariates, mask, mask = contour_sp) 
  return(covariates)
}