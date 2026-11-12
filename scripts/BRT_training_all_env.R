###########################################
##    BRT training all env variables     ##
###########################################

require(ggplot2)
require(ncf)
require(ncdf4)
require(exactextractr)
require(terra)
require(sf)
require(sp)
require(gmodels)
require(devtools)
require(dismo)
require(gbm)
require(blockCV)
require(ecospat)
require(readr)

envVariables <- c("temp_winter","temp_spring","temp_summer","temp_fall", "temp_var","pr_winter" ,"pr_spring","pr_summer","pr_fall",
                  "rh_winter","rh_spring","rh_summer","rh_fall","croplands", "pastures", "rangelands", "urbanAreas","primaryForest",
                  "primaryNonForest", "secondaryForest", "secondaryNonForest", "populationDensity")

n.iterations <-100
source("WNV/helper_functions/get_sorensen_index.r")
source("WNV/helper_functions/brt_training.r")

nutsM_sf <-readRDS("WNV/Processed_data/nutsM_sf.rds")
nutsM_centroids<-st_coordinates(st_centroid(nutsM_sf))

## Calculate the block size for spatial cross-validation
strategies <- c("STG2")
ranges_STGs <- list()
for (s in strategies){
  cat("Working on ", s, "\n")
  dataframes_list <- readRDS(paste0("WNV/Output/2026-10-22_data_env_train_", s, ".rds"))
  
  dataframes <- lapply(dataframes_list, function(df){
    index <- match(df$nutsM_code, nutsM_sf$LocationCo)
    df$x <- nutsM_centroids[index, 1]
    df$y <- nutsM_centroids[index, 2]
    return(df)
  })
  
  block_sizes<- numeric(length(dataframes))
  for (i in 1:length(dataframes)){
    current_data <- dataframes[[i]]

    correlogram <- ncf::correlog(x = current_data$x,y = current_data$y, z = current_data$response,increment = 10,resamp = 0,na.rm = TRUE,latlon = TRUE)
    index <- which(correlogram$correlation <= 0)[1]
    block_sizes[i] <- correlogram$mean.of.class[index]
  }
  median_block <- round(median(block_sizes, na.rm = TRUE), digits = 0); print(median_block) #1345
  ranges_STGs[[s]] <- median_block
}
#STG2: 1345

#plot last correlogram as example
df_correlogram <- data.frame(distance = correlogram$mean.of.class[-1], correlation = correlogram$correlation[-1])
ggplot(df_correlogram, aes(x = distance, y = correlation)) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 0.4) +
  geom_vline(xintercept = median_block, color = "violet", linetype = "dashed", linewidth = 0.4) +
  geom_line(color = "gray30", linewidth = 0.4) +
  geom_point(color = "gray30", size = 0.5) +
  #geom_point(color = "gray30", fill = NA, size = 0.5, shape = 21, stroke = 0.1) +
  scale_x_continuous(name = "Distance (km)") +
  scale_y_continuous(name = "Correlation", limits = c(-1, 1.0)) +
  theme_minimal(base_size = 10)


#brt parametrs
gbm.y <- "response"
gbm.x <- envVariables
#theRange one per different STG
tree.complexity <- 5
learning.rate <- 0.001
bag.fraction <- 0.85
n.trees <- 100

STGs_BRTs <- list()
results <- data.frame(row.names = strategies)

for (s in strategies){
  cat("BRT training on ", s, "\n")
  dataframes_list <- readRDS(paste0("WNV/Output/2026-10-22_data_env_train_", s, ".rds"))
  
  dataframes <- lapply(dataframes_list, function(df){
    index <- match(df$nutsM_code, nutsM_sf$LocationCo)
    df$x <- nutsM_centroids[index, 1]
    df$y <- nutsM_centroids[index, 2]
    return(df)
  })
  
  theRange <- ranges_STGs[[s]]
  
  brt_model_list <- lapply(dataframes, function(x){
    brt_training(x, gbm.y, gbm.x, theRange, tree.complexity, learning.rate, bag.fraction, n.trees)
  })
  if (any(sapply(brt_model_list, is.null))) warning(paste(s, ": some gbm.step runs returned NULL"))
  
  STGs_BRTs[[s]] <- brt_model_list
}

##Estimate AUC, SI and Boyce index for model performance

#extract data for Boyce index estimation
source("WNV/helper_functions/load_rasters.R")
source("WNV/helper_functions/get_seasonal_indices.r")
source("WNV/helper_functions/units_convertion.R")
source("WNV/helper_functions/extract_env_variables.r")

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
start_year_raster = 1901

climatic_variables <- c("temp", "pr", "rh")
seasons <- c("winter", "spring", "summer", "fall")
landuse_variables <- c("croplands", "pastures", "rangelands", "urbanAreas", "primaryForest",
                       "primaryNonForest", "secondaryForest", "secondaryNonForest", "populationDensity")

nutsM_codes <- nutsM_sf$LocationCo
nutsM_codes <- nutsM_codes[!substr(nutsM_codes, 1, 2) %in% c("RS", "BA", "ME", "CH")]# excluding this areas for the evalution background! 
grid<- expand.grid(nutsM_code = nutsM_codes, year = c(2008:2024),stringsAsFactors = FALSE)
env_all_years <- extract_env_variables(grid)

saveRDS(env_all_years, "WNV/Output/2026-10-22_env_all_years.rds")

results <- data.frame(row.names = strategies)
for (s in strategies){
  brt_model_list <- STGs_BRTs[[s]]
  
  n.iterations <- length(brt_model_list)
  metrics <- data.frame(Boyce = rep(NA, n.iterations), AUC = rep(NA, n.iterations), SI = rep(NA, n.iterations), opt_thresholds = rep(NA, n.iterations))
  
  for (j in 1:n.iterations){
    brt_model <- brt_model_list[[j]]
    
    metrics$AUC[j] <- brt_model$cv.statistics$discrimination.mean
    sorensen_results <- get_sorensen_index(brt_model, thresholds = seq(0, 1, 0.01))
    metrics$SI[j] <-sorensen_results$max_sorensen
    metrics$opt_thresholds[j] <- sorensen_results$optimised_threshold
  
    pred_all <- rowMeans(sapply(brt_model$fold.models, function(m)
      predict.gbm(m, env_all_years[, gbm.x], n.trees = brt_model$gbm.call$best.trees, type = "response")))
    resp <- brt_model$gbm.call$dataframe$response
    metrics$Boyce[j] <- ecospat.boyce(fit = pred_all, obs = plogis(brt_model$fold.fit)[resp == 1],
                                      nclass = 0, window.w = "default", PEplot = FALSE)$cor
  }
  
  write_tsv(metrics, paste0("WNV/Output/2026-10-22_brt_performances_", s, "_replicates.tsv"))
  
  for (metric in colnames(metrics)){
    x <- metrics[[metric]]
    q <- quantile(x, c(0.25, 0.5, 0.75), na.rm = TRUE)
    results[s, metric] <- paste0(round(q[2], 3), " (", round(q[1], 3), "-", round(q[3], 3), ")")
  }
}


saveRDS(STGs_BRTs, "WNV/Output/2026-10-22_BRTs_STGs.rds")
write_tsv(cbind(strategy = rownames(results), results), "WNV/Output/2026-10-22_brt_performances_STGs.tsv")
