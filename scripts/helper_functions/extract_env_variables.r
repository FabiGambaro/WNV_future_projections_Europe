extract_env_variables <- function(data){
  data$year <- as.integer(unlist(data$year))
  index <- match(data$nutsM_code, nutsM_sf$LocationCo)
  if (any(is.na(index))){
    warning("Some regions not matched!")
  }
  
  data_geo <- st_sf(data, geometry = nutsM_sf$geometry[index])
  data_geo$area <- nutsM_sf$area[index]
  
  results <- lapply(1:nrow(data_geo), function(k){
    # get seasonal indices
    if (!is.na(data_geo$year[k])) {
      seasonal_indices <- get_seasonal_indices(
        year = as.numeric(data_geo$year[k]),
        start_year_raster = start_year_raster
      )
      year_index <- as.numeric(data_geo$year[k]) - start_year_raster + 1
    } else {
      seasonal_indices <- get_seasonal_indices(
        year_start = as.numeric(data_geo$year_start[k]),
        year_end = as.numeric(data_geo$year_end[k]),
        start_year_raster = start_year_raster
      )
      year_index <- as.numeric(data_geo$year_start[k]) - start_year_raster + 1
    }
    
    covariates <- list()
    
    for (variable in climatic_variables) {
      mat <- obsclim.data[[variable]]
      for (season in seasons) {
        layers <- mat[[seasonal_indices[[season]]]]
        if (length(layers) == 0) {
          covariates[[paste0(variable, "_", season)]] <- NA
        } else {
          covariates[[paste0(variable, "_", season)]] <-
            units_convertion(variable, terra::mean(layers, na.rm = TRUE))
        }
      }
      if (variable == "temp") {
        covariates[[paste0(variable, "_var")]] <- covariates[[paste0(variable, "_summer")]] - 
          covariates[[paste0(variable, "_winter")]]
      }
    }
    
    for (var in landuse_variables) { #for landuse we expect not a lot of variation in landuse so we use the first year
      mat <- obsclim.data[[var]]
      covariates[[var]] <- mat[[year_index]]
    }
    
    if ("totalPopulation" %in% names(covariates)) {
      covariates_no_pop <- covariates[names(covariates) != "totalPopulation"]
      extracted_vals <- sapply(covariates_no_pop, function(x) {
        if (!terra::same.crs(x, data_geo[k,])) {
          warning("Check CRS projections rasters vs data")
        }
        exact_extract(x, data_geo[k,], fun = "mean") #value averged at NUTS3 level
      })
      
      pop_vals <- exact_extract(covariates[["totalPopulation"]],data_geo[k,],fun = "sum")
      names(pop_vals) <- "totalPopulation"
      extracted_vals <- c(extracted_vals, pop_vals)
      
    }else{
      extracted_vals <- sapply(covariates, function(x) {
        if (!terra::same.crs(x, data_geo[k,])) {
          warning("Check CRS projections rasters vs data")
        }
        exact_extract(x, data_geo[k,], fun = "mean") #value averged at NUTS3 level
      })
    }
    return(extracted_vals)
  })
  env_df <- do.call(rbind, results)
  cbind(st_drop_geometry(data_geo), env_df)
  
}