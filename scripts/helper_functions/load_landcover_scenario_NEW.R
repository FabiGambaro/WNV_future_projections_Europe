load_landcover_scenario <- function(scenario, scenario2, variable_codes, variable_names, years, nutsM, totalPop, areas){
  
  print(paste0("Loading and merging data - scenario: ", scenario))
  
  raster_path <- paste0("Rasters/Environmental_rasters/Land-use/Future_landuse_annual/", scenario, "/landcover_", scenario, "_2015-2100.nc")
  
  #matching name of the rasters to full names variables
  code_to_name <- c(c3ann="C3 annual crops",c3nfx="C3 nitrogen-fixing crops",c3per="C3 perennial crops",
                    c4ann="C4 annual crops",c4per="C4 perennial crops",pastr="managed pasture",primf ="forested primary land",
                    primn="non-forested primary land",range="rangeland",secdf = "potentially forested secondary land",
                    secdn = "potentially non-forested secondary land",urban = "urban land")
  
  # load each layer by matching content (long name -> short code)
  land_covers1 <- list()
  for(i in 1:length(code_to_name)){
    varname <- code_to_name[[i]]
    raster_varname <- names(code_to_name)[i]
    layer <-rast(raster_path,subds = raster_varname)
    land_covers1[[varname]] <- layer
  }

  # aggregate data 
  land_covers2 <- lapply(seq_along(variable_names), function(j) {
    indices <- which(code_to_name == variable_names[j])
    
    if (length(indices) == 0) {
      indices <- which(grepl(variable_names[j], code_to_name))
    }
    
    if (length(indices) == 0) {
      stop("variable_names[", j, "] = '", variable_names[j], "' has no match")
    }
    
    lc <- land_covers1[[indices[1]]]
    names(lc) <- paste0(variable_codes[j], "_", 2015:2100)
    
    if (length(indices) > 1) {
      for (k in 2:length(indices)) {
        lc <- lc + land_covers1[[indices[k]]]
      }
    }
    lc
  })
  
  names(land_covers2) <- variable_codes
  #aggregate to match climate resolution 0.5 X 0.5
  land_covers2 <- lapply(land_covers2, function(x) aggregate(x, fact = 2, fun = "mean"))
  
  #merge hist +ssp
  landcover_hist_ssp <- list()
  for (j in 1:length(variable_codes)){
    var <- variable_codes[j]
    hist_path   <- paste0("Rasters/Environmental_rasters/Land-use/", var, "_LUH2-GCB2024_1901_2024.nc")
    hist_raster <- rast(hist_path)  # 1901-2024, 124 layers
    names(hist_raster) <- paste0(variable_codes[j], "_", 1901:2024) 
    ssp_raster  <- land_covers2[[var]]
    names(ssp_raster) <- paste0(variable_codes[j], "_", 2015:2100)  # always named 2015:2100 before subsetting
    
    if (min(years) == 2015) {
      #hist_raster <- hist_raster[[1:114]]  # cut to 1901-2014
      hist_raster <- hist_raster[[grep("_190[1-9]$|_19[1-9][0-9]$|_200[0-9]$|_201[0-4]$", names(hist_raster))]]
      ssp_raster  <- ssp_raster[[grep("_201[5-9]|_20[2-9][0-9]|_2100$", names(ssp_raster))]]
    } else if (min(years) == 2025) {  # keep full hist 1901-2024
      ssp_raster  <- ssp_raster[[grep("_202[5-9]|_20[3-9][0-9]|_2100$", names(ssp_raster))]]
    }
    
    hist_ssp <- c(hist_raster, ssp_raster)
    landcover_hist_ssp[[variable_codes[j]]] <- hist_ssp
  }
  
  pop_historical <- rast("Rasters/Environmental_rasters/Population/Population_density_1900_2100/Population_density_05deg_1900_2025_historical.nc")
  pop_historical <- pop_historical[[2:126]]  # 1901-2025
  
  pop_ssp <- rast(paste0("Rasters/Environmental_rasters/Population/Population_density_1900_2100/Population_density_05deg_2026_2100_", scenario2, ".nc"))
  
  pop_hist_ssp <- c(pop_historical, pop_ssp) 
  landcover_hist_ssp[["populationDensity"]] <- pop_hist_ssp
  
  if(totalPop == TRUE){
    landcover_hist_ssp[["totalPopulation"]]   <- pop_hist_ssp * areas
  }
  return(landcover_hist_ssp)
}