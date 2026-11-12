########################################################
### TEMPORAL VALIDATION HOLD-OUT: 2023-2024 DATA  ######
########################################################

require(dismo)
require(gbm)
require(gmodels)
require(blockCV)
require(sf)
require(terra)
require(exactextractr)
require(readr)
require(ecospat)
require(future.apply)


nutsM <- readRDS("WNV/Processed_data/nutsM_EU.rds")
nutsM_IDs<- read_tsv("WNV/Processed_data/nutsM_IDs.tsv")
nutsM_sf <-readRDS("WNV/Processed_data/nutsM_sf.rds")
nutsM_centroids<-st_coordinates(st_centroid(nutsM_sf))
strategies <- c("STG2")

source("WNV/helper_functions/years_by_NUTS3.r")
source("WNV/helper_functions/pseudo_absences_shp.r")
source("WNV/helper_functions/load_rasters.R")
source("WNV/helper_functions/get_seasonal_indices.r")
source("WNV/helper_functions/units_convertion.R")
source("WNV/helper_functions/extract_env_variables.r")
source("WNV/helper_functions/brt_training.r")

#1.presence data split into train and testing before aggregating
presences_data <- read_csv("WNV/Occurrence_data/wnv_presences_nutsM_year.csv")
last_train_year <- 2022
presences_train <- years_by_nuts(presences_data[presences_data$year <= last_train_year, ]) #207
presences_test  <- years_by_nuts(presences_data[presences_data$year >  last_train_year, ]) #188
presences_train$response = 1
presences_test$response = 1

#pseudo-absences STG2: surveillance capacity + exclusion of 2025 + 2026 locations NUTS3
# same pool as the full model: exclude ALL presence NUTS3 (2008-2024), the 2025-26 areas, RS/BA/ME/CH and their neighbours
presences_agg<-years_by_nuts(presences_data)
to_exclude <- read_tsv("WNV/Occurrence_data/excluded_2025-26_data.tsv")
to_exclude <- to_exclude[,2]
to_exclude$year <- rep(NA, nrow(to_exclude))
to_exclude$year_start <- rep(NA, nrow(to_exclude))
to_exclude$year_end <- rep(NA, nrow(to_exclude))
presences_excluded_areas <- rbind(presences_agg, to_exclude)
pseudo_absences_excluded_nutsM <- pseudo_absences_shp(presences_excluded_areas, nutsM_sf)

rast_surv <- terra::rast("Rasters/Lim_et_al_2025_data/outputs/Rasters/Surveillance_map_wmean.tif")
rast_surv_cropped <- crop(rast_surv, nutsM) # crop to Europe
surv_nutsM <- exact_extract(rast_surv_cropped, nutsM_sf, fun="mean")
surv.nutsM_data<-nutsM_IDs
surv.nutsM_data$surv=surv_nutsM
surv.nutsM_data$surv[is.na(surv.nutsM_data$surv)] = 0

pseudo_absences_surv <- surv.nutsM_data[surv.nutsM_data$nutsM_code %in% pseudo_absences_excluded_nutsM$LocationCo,]
pseudo_absences_surv$prob <- pseudo_absences_surv$surv/sum(pseudo_absences_surv$surv) #weighted prob

#generating pseudo-absenseces
#a different set for different period and set (training and testing)
sample_pseudo_absences <- function(presences_period){
  n_pseudo <- nrow(presences_period); ratio = 1
  indices <- pseudo_absences_surv$nutsM_code
  pseudo_absences_codes <- sample(indices, ratio*n_pseudo, replace = F, prob = pseudo_absences_surv$prob) #sample NUTS3 codes without replacement, weighted by surveillance
  pseudo_absences_sampled <- pseudo_absences_surv[pseudo_absences_surv$nutsM_code %in% pseudo_absences_codes, ]
  pseudo_absences <- pseudo_absences_sampled[, "nutsM_code"]
  w <- sample(nrow(presences_period), n_pseudo, replace = TRUE)
  pseudo_absences$year_start <- presences_period$year_start[w]
  pseudo_absences$year_end<- presences_period$year_end[w]
  pseudo_absences$year<- presences_period$year[w]
  pseudo_absences$response = 0
  rbind(presences_period, pseudo_absences[, colnames(presences_period)]) #merge data
}

data_train <- list(); data_test <- list()
n.iterations = 100 ; set.seed(113)
for(i in 1:n.iterations){
  data_train[[i]] <- sample_pseudo_absences(presences_train)
  data_test[[i]]  <- sample_pseudo_absences(presences_test)
}
nrow(data_test[[i]]) #376 presence and pseudo-absence
nrow(data_train[[i]]) #414 207+207


presences_train #188
presences_test #207

all_test <- ldply(data_test, rbind)
all_pseudo_test <-  all_test[all_test$response ==0,]
all_pseudo_test$key <- paste(all_pseudo_test$nutsM_code, all_pseudo_test$year,
                             all_pseudo_test$year_start, all_pseudo_test$year_end, sep = "_")
pseudo_test <- all_pseudo_test[!duplicated(all_pseudo_test$key), ]

all_train <- ldply(data_train, rbind)
all_pseudo_train <-  all_train[all_train$response ==0,]
all_pseudo_train$key <- paste(all_pseudo_train$nutsM_code, all_pseudo_train$year,
                              all_pseudo_train$year_start, all_pseudo_train$year_end, sep = "_")
pseudo_train <- all_pseudo_train[!duplicated(all_pseudo_train$key), ]


#2. extarct data
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

#brt parametrs
envVariables <- c("temp_winter","temp_spring","temp_summer" ,"temp_fall" ,"temp_var","pr_spring","pr_summer",
                  "rh_winter","rh_summer","rh_fall","pastures","rangelands","urbanAreas","primaryForest",
                  "secondaryNonForest", "populationDensity") 

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
    
    covariates <- covariates[names(covariates) %in% envVariables]
    
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

presences_train_env <- extract_env_variables(presences_train)
pseudo_train_env <- extract_env_variables(pseudo_train)
presences_test_env <- extract_env_variables(presences_test)
pseudo_test_env <- extract_env_variables(pseudo_test)

#3. reconstruct the dataframes
out_dir <- "WNV/Output/data_extraction_temporal_validation/training_data/"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

data_env_train <- list()
for (i in 1:length(data_train)){
  pa <- data_train[[i]][data_train[[i]]$response == 0, ]
  pa_rows <- pseudo_train_env[match(paste(pa$nutsM_code, pa$year, pa$year_start, 
                                          pa$year_end, sep = "_"), pseudo_train_env$key), ]
  stopifnot(!any(is.na(pa_rows$nutsM_code)))
  pa_rows$key <- NULL
  data_env_train[[i]] <- rbind(presences_train_env, pa_rows)
  write.csv(data_env_train[[i]], paste0(out_dir, s, "_20crv3-era5_env_data_replicate_", i, ".csv"), row.names = F)
}

out_dir2 <- "WNV/Output/data_extraction_temporal_validation/testing_data/"
dir.create(out_dir2, showWarnings = FALSE, recursive = TRUE)

data_env_test <- list()
for (i in 1:length(data_test)){
  pa <- data_test[[i]][data_test[[i]]$response == 0, ]
  pa_rows <- pseudo_test_env[match(paste(pa$nutsM_code, pa$year, pa$year_start, 
                                         pa$year_end, sep = "_"), pseudo_test_env$key), ]
  stopifnot(!any(is.na(pa_rows$nutsM_code)))
  pa_rows$key <- NULL
  data_env_test[[i]] <- rbind(presences_test_env, pa_rows)
  write.csv(data_env_test[[i]], paste0(out_dir2, s, "_20crv3-era5_env_data_replicate_", i, ".csv"), row.names = F)
}

#add centroids!
add_xy <- function(df){
  index <- match(df$nutsM_code, nutsM_sf$LocationCo)
  df$x <- nutsM_centroids[index, 1]
  df$y <- nutsM_centroids[index, 2]
  return(df)
}
data_env_train <- lapply(data_env_train, add_xy)
data_env_test  <- lapply(data_env_test, add_xy)


temporal_splits <- list()
temporal_splits[["STG2"]] <- lapply(1:n.iterations, function(i) list(train_df = data_env_train[[i]], test_df = data_env_test[[i]]))

#4. re-run BRTs

plan(multisession, workers = parallel::detectCores() - 1)
temporal_validation_splits_BRTs <- list()
for (s in strategies){
  cat("BRT training on ", s, "\n")
  
  BRTs <-list()
  for (i in 1:length(models)){
    model<- names(models)[i]
    print(paste0("Testing model ", i))
    
    tree.complexity=models[[model]][["tree.complexity"]]
    learning.rate=models[[model]][["learning.rate"]]
    bag.fraction=models[[model]][["bag.fraction"]]
    
    training_data <- lapply(temporal_splits[[s]], function(df) df$train_df) #take only yhe data for training. 
    
    #brt_model_list <- lapply(training_data, function(df){
    brt_model_list <- future_lapply(training_data, function(df){
      brt_training(df, gbm.y, gbm.x, theRange, tree.complexity, learning.rate, bag.fraction, n.trees)
    }, future.seed = TRUE, future.packages = c("dismo", "gbm", "blockCV", "sf"))
    
    BRTs[[model]] <- brt_model_list
  }
  temporal_validation_splits_BRTs[[s]] <- BRTs
}
plan(sequential)

#5. Calculate AUC, Boyce index and SI. 

env_all_years <- readRDS("WNV/Output/2026-10-22_env_all_years.rds")
nustM_2023_24_env <- env_all_years[env_all_years$year %in% 2023:2024, ]
stopifnot(!any(substr(nustM_2023_24_env$nutsM_code, 1, 2) %in% c("RS", "BA", "ME", "CH")))

results_list <- list()

for (s in strategies){
  ##evaluate the behaviour on the on the 2023–2024 hold-out validation data##
  for (i in 1:length(models)){
    model <- names(models)[[i]]
    brt_model_list <- temporal_validation_splits_BRTs[[s]][[model]]
    test_data <- lapply(temporal_splits[[s]], function(df) df$test_df) #take only yhe data for valiudation
  
    n.iterations <- length(brt_model_list)
    metrics <- data.frame(Boyce = rep(NA, n.iterations), AUC = rep(NA, n.iterations), SI = rep(NA, n.iterations), opt_thresholds = rep(NA, n.iterations))
    
    for (j in 1:n.iterations){
      brt_model <- brt_model_list[[j]]
      pred <- predict.gbm(brt_model, test_data[[j]][, gbm.x], 
                          n.trees = brt_model$gbm.call$best.trees, type = "response")
      obs <- test_data[[j]]$response
      e <- evaluate(p = pred[obs == 1], a = pred[obs == 0])
      metrics$AUC[j] <- e@auc
      SI_test <-  get_sorensen_index_pred(pred, obs, thresholds = seq(0, 1, 0.01)) # threshold optimised on the held-out 2023-24 predictions
      metrics$SI[j] <- SI_test$max_sorensen
      metrics$opt_thresholds[j] <- SI_test$optimised_threshold
      pred_all <- predict.gbm(brt_model, nustM_2023_24_env[, gbm.x],
                              n.trees = brt_model$gbm.call$best.trees, type = "response")
      metrics$Boyce[j] <- ecospat.boyce(fit = pred_all, obs = pred[obs == 1],
                                        nclass = 0, window.w = "default", PEplot = FALSE)$cor
    }
    
    res <- data.frame(strategy = s, model = model, tree.complexity = models[[model]][["tree.complexity"]],
                      learning.rate = models[[model]][["learning.rate"]], bag.fraction = models[[model]][["bag.fraction"]])
    for (metric in colnames(metrics)){
      x <- metrics[[metric]]
      median <- median(x)
      q <- quantile(x, c(0.025, 0.975), na.rm = TRUE)
      res[[metric]] <- paste0(round(median, 3), " (", round(q[1], 3), "-", round(q[2], 3), ")")
    }
    results_list[[paste(s, model, sep = "_")]] <- res
  }
  
}
results <- do.call(rbind, results_list)
write.csv(results, "WNV/Output/temporal_validation_splits_BRTs.csv", row.names = FALSE)
