######################
## Data extraction ##
#####################

require(terra)
require(exactextractr)
require(sf)
require(readr)
require(future.apply)

#load Europe geo data
contour_sf<- readRDS("WNV/Processed_data/europe_mask.rds")
contour_sf<- st_as_sf(contour_sf)
nutsM_sf<-readRDS("WNV/Processed_data/nutsM_sf.rds")
nutsM<-readRDS("WNV/Processed_data/nutsM_EU.rds")
nutsM_IDs <- read_tsv("WNV/Processed_data/nutsM_IDs.tsv")

source("WNV/helper_functions/load_rasters.R") #no cropping or masking before extarction of the data
source("WNV/helper_functions/get_seasonal_indices.r")
source("WNV/helper_functions/units_convertion.R")

obsclim_rasters <- list(temp=c("Rasters/Environmental_rasters/ISIMIP3a/obsclim/20CRv3-ERA5/20crv3-era5_obsclim_tas_1901_2021_monmean.nc","Rasters/Environmental_rasters/ISIMIP3a/obsclim/20CRv3-ERA5/20crv3-era5_obsclim_tas_2022_2024_monmean.nc"),
                        pr=c("Rasters/Environmental_rasters/ISIMIP3a/obsclim/20CRv3-ERA5/20crv3-era5_obsclim_pr_1901_2021_monmean.nc","Rasters/Environmental_rasters/ISIMIP3a/obsclim/20CRv3-ERA5/20crv3-era5_obsclim_pr_2022_2024_monmean.nc"),
                        rh=c("Rasters/Environmental_rasters/ISIMIP3a/obsclim/20CRv3-ERA5/20crv3-era5_obsclim_hurs_1901_2021_monmean.nc","Rasters/Environmental_rasters/ISIMIP3a/obsclim/20CRv3-ERA5/20crv3-era5_obsclim_hurs_2022_2024_monmean.nc"),
                        croplands="Rasters/Environmental_rasters/Land-use/croplands_LUH2-GCB2024_1901_2024.nc",
                        pastures="Rasters/Environmental_rasters/Land-use/pastures_LUH2-GCB2024_1901_2024.nc",
                        rangelands="Rasters/Environmental_rasters/Land-use/rangeland_LUH2-GCB2024_1901_2024.nc",
                        urbanAreas="Rasters/Environmental_rasters/Land-use/urbanAreas_LUH2-GCB2024_1901_2024.nc",
                        primaryForest="Rasters/Environmental_rasters/Land-use/primaryForest_LUH2-GCB2024_1901_2024.nc",
                        primaryNonForest="Rasters/Environmental_rasters/Land-use/primaryNonF_LUH2-GCB2024_1901_2024.nc",
                        secondaryForest="Rasters/Environmental_rasters/Land-use/secondaryForest_LUH2-GCB2024_1901_2024.nc",
                        secondaryNonForest="Rasters/Environmental_rasters/Land-use/secondaryNonF_LUH2-GCB2024_1901_2024.nc",
                        populationDensity="Rasters/Environmental_rasters/Population/Population_density_1900_2100/Population_density_05deg_1900_2025_historical.nc")

obsclim.data <- load_rasters(raster_paths_list = obsclim_rasters, area_raster_path = NULL)
sapply(obsclim.data, terra::nlyr) # temp pr rh = 1488 (124 y *12), land use + pop = 124
start_year_raster = 1901

climatic_variables <- c("temp", "pr", "rh")
seasons <- c("winter", "spring", "summer", "fall")
landuse_variables <- c("croplands", "pastures", "rangelands", "urbanAreas", "primaryForest",
                       "primaryNonForest", "secondaryForest", "secondaryNonForest", "populationDensity")


strategies <- c("STG2")
out_dir <- "WNV/Output/data_extraction_2026-10-22/"
dir.create(out_dir, showWarnings = FALSE)

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

#extract presences 
presences_data <- read_csv("WNV/Occurrence_data/wnv_presences_nutsM_year.csv")
source("WNV/helper_functions/years_by_NUTS3.r")
presences_agg<-years_by_nuts(presences_data) #261
presences_agg$response = 1 

presences_env <- extract_env_variables(presences_agg)
saveRDS(presences_env, "WNV/Output/2026-10-22_presences_env.rds")

#load and combine unique NUTS3 x year combination pseudo-absences across 5 strategies and 100 replicates.
all_pseudoabsences <- do.call(rbind, lapply(strategies, function(s){
  dt <- readRDS(paste0("WNV/Output/presence_pseudoabsences_", s, ".rds"))
  do.call(rbind, lapply(dt, function(d) d[d$response == 0, ]))
}))

all_pseudoabsences$key <- paste(all_pseudoabsences$nutsM_code, all_pseudoabsences$year,
                                all_pseudoabsences$year_start, all_pseudoabsences$year_end, sep = "_")
pseudoabsences_unique <- all_pseudoabsences[!duplicated(all_pseudoabsences$key), ] #remove those non unique NUTS3 x year combination pseudo-absences

pseudoabsences_env <- extract_env_variables(pseudoabsences_unique)
saveRDS(pseudoabsences_env, "WNV/Output/2026-10-22_pseudoabsences_env.rds")


#reconstruct the dataframes
out_dir <- "WNV/Output/data_extraction_2026-10-22/"
dir.create(out_dir, showWarnings = FALSE)

for (s in strategies){
  data_train <- readRDS(paste0("WNV/Output/presence_pseudoabsences_", s, ".rds"))
  data_env_train <- list()
  for (i in 1:length(data_train)){
    pa <- data_train[[i]][data_train[[i]]$response == 0, ]
    pa_rows <- pseudoabsences_env[match(paste(pa$nutsM_code, pa$year, pa$year_start, pa$year_end, sep = "_"), pseudoabsences_env$key), ] # CHANGED: same key as above
    stopifnot(!any(is.na(pa_rows$nutsM_code)))
    pa_rows$key <- NULL
    data_env_train[[i]] <- rbind(presences_env, pa_rows)
    write.csv(data_env_train[[i]], paste0(out_dir, s, "_20crv3-era5_env_data_replicate_", i, ".csv"), row.names = F)
  }
  saveRDS(data_env_train, paste0("WNV/Output/2026-10-22_data_env_train_", s, ".rds"))
}

#check
for (s in strategies){
  d <- readRDS(paste0("WNV/Output/2026-10-22_data_env_train_", s, ".rds"))
  print(c(s, unique(sapply(d, nrow)),
          unique(sapply(d, function(x) sum(x$response == 1))),
          max(sapply(d, function(x) max(c(x$year, x$year_end), na.rm = TRUE)))))
}

colSums(is.na(presences_env))
colSums(is.na(pseudoabsences_env))
