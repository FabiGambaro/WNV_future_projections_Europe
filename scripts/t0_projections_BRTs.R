##################################
##   t0 maps model A and B     ##
##################################
setwd("~/Dropbox/Postdoc_SpELL/GRP_Future_projections_Arbo")
require(readr)
require(plyr)
require(dplyr)
require(tidyr)
require(terra)
require(dismo)
require(gbm)
require(exactextractr)
require(ggplot2)
require(cowplot)
require(RColorBrewer)
require(scales)
require(viridis)
require(sf)
require(blockCV)
require(future.apply)

#load helper function
source("WNV/helper_functions/brt_training.r")
source("WNV/helper_functions/get_sorensen_index.r")

options(scipen=999)
MODELS_isimip3b <- c("CESM2-WACCM","CNRM-CM6-1","CNRM-ESM2-1","CanESM5",
                     "EC-Earth3","GFDL-ESM4","IITM-ESM","IPSL-CM6A-LR",
                     "KACE-1-0-G","MIROC6","MPI-ESM1-2-HR","MRI-ESM2-0",
                     "TaiESM1","UKESM1-0-LL")
scenarios <- c("ssp126", "ssp370", "ssp585")
thresholds <- 1.3

nutsM<-readRDS("WNV/Processed_data/nutsM_EU.rds")
nutsM_sf<-readRDS("WNV/Processed_data/nutsM_sf.rds")
nutsM_centroids<-st_coordinates(st_centroid(nutsM_sf))
nutsM_IDs <- read_tsv("WNV/Processed_data/nutsM_IDs.tsv")
contour_sf <- readRDS("WNV/Processed_data/europe_mask.rds")

#run BRTs
strategies <- c("STG2")
models <- list("model_A"= c(tree.complexity = 3, learning.rate = 0.001, bag.fraction = 0.85), 
               "model_B"= c(tree.complexity = 5, learning.rate = 0.001, bag.fraction = 0.80))
envVariables <- c("temp_winter","temp_spring","temp_summer" ,"temp_fall" ,"temp_var","pr_spring","pr_summer",
                  "rh_winter","rh_summer","rh_fall","pastures","rangelands","urbanAreas","primaryForest",
                  "secondaryNonForest", "populationDensity") #shadow_testing


n.iterations <-100
gbm.y <- "response"
gbm.x <- envVariables
theRange <- 1345 #from BRT_analysis
n.trees<-100

dataframes_list <- readRDS("WNV/Output/2026-10-22_data_env_train_STG2.rds")
dataframes <- lapply(dataframes_list, function(df){
  index <- match(df$nutsM_code, nutsM_sf$LocationCo)
  df$x <- nutsM_centroids[index, 1]
  df$y <- nutsM_centroids[index, 2]
  return(df)
})


plan(multisession, workers = parallel::detectCores() - 1)
BRTs <- list()
for (model in names(models)){
  cat("BRT", model, "\n")
  tree.complexity=models[[model]][["tree.complexity"]]
  learning.rate=models[[model]][["learning.rate"]]
  bag.fraction=models[[model]][["bag.fraction"]]
  
  BRTs[[model]] <- future_lapply(dataframes, function(df){
    brt_training(df, gbm.y, gbm.x, theRange, tree.complexity, learning.rate, bag.fraction, n.trees)
  }, future.seed = TRUE, future.packages = c("dismo", "gbm", "blockCV", "sf"))
  
  BRTs[[model]] <- BRTs[[model]][!sapply(BRTs[[model]], is.null)] #drop non-converged models
  
  cat(model, ":", length(BRTs[[model]]), "BRTs converged\n")
}
plan(sequential)

saveRDS(BRTs, "WNV/Output/2026-10-30_BRTs_A_B.rds")



#load climate warming level windows
landuse_21window <- read_tsv("WNV/Output/All_models_thresholds_66%chance_markerScenario.tsv")
landuse_21window_t0 <- landuse_21window[which(landuse_21window$threshold == 1.3),]

GCMs_21window <- read_tsv("WNV/Output/All_models_thresholds_66%chance.tsv")
global_warming_windows <- function(df,thresh){
  tab <- df[,c("model", "ssp", paste0("thresh_",thresh))]
  colnames(tab) <- c("model","ssp","central_year")
  tab$start_year <- tab$central_year -10
  tab$end_year <- pmin(tab$central_year + 10, 2100)
  return(tab)
}

global_warming_windows_list <- lapply(thresholds, function(t) {
  global_warming_windows(GCMs_21window, thresh = t)
})
names(global_warming_windows_list) <- thresholds

# load data
source("WNV/helper_functions/get_seasonal_indices.r")
source("WNV/helper_functions/units_convertion.R")
source("WNV/helper_functions/load_gcm_rasters.R")
source("WNV/helper_functions/load_landcover_scenario_NEW.R")

## load climatic data
climatic_data <- lapply(MODELS_isimip3b, function(model_name){
  climatic_hist_ssp <- list() #list with all scenarios for one model
  for(scenario in scenarios){
    key <- paste0(model_name, "_", scenario)
    temp<- load_gcm_rasters("tas", model_name, scenario)
    pr<- load_gcm_rasters("pr", model_name, scenario)
    rh<- load_gcm_rasters("hurs", model_name, scenario)
    climatic_hist_ssp[[key]] <- list(temp=temp, pr=pr, rh=rh)
    cat("Processed:", key, "\n")
  }
  return(climatic_hist_ssp)
})

climatic_data <- do.call(c, climatic_data)

#load data from land-use marker scenario 
marker_scenario<- "ssp370"
marker_scenario2 <- "SSP3"

#selected set of landuse variables
variable_codes = c("pastures","rangeland","urbanAreas","primaryForest","secondaryNonF")
variable_names = c("pasture","rangeland","urban land","forested primary land",
                   "potentially non-forested secondary land") #how land-cover types appear in the NetCDF

#need to provide areas even if not used as we dont include total pop as variable
areas <- rast("Rasters/Environmental_rasters/clm45_area.nc4") / (1000 * 1000)  # km2
totalPop = FALSE #not including total pop

years<- 2015:2100 #using 'future landuse data' from 2015 on
#load_landcover_scenario load pop density by default (historical + SSP scenario)
landcover_data <- lapply(1:length(marker_scenario), function(i) {
  load_landcover_scenario(marker_scenario[i], marker_scenario2[i],variable_codes, 
                          variable_names, years, nutsM, totalPop,areas)
})
names(landcover_data) <- marker_scenario

# rename variable_codes to variable_codes2 to match with brt training data. Adding popDensity as before was not there
variable_codes = c("pastures","rangeland","urbanAreas","primaryForest","secondaryNonF","populationDensity")
variable_codes2 = c("pastures","rangelands","urbanAreas","primaryForest","secondaryNonForest","populationDensity")

landcover_data <- lapply(landcover_data, function(x) {
  names(x)[match(variable_codes, names(x))] <- variable_codes2
  return(x)
})

# data extraction independen of each STG 
if(!dir.exists("WNV/Output/data_extraction_t0_BRTs_A_B")){
  dir.create("WNV/Output/data_extraction_t0_BRTs_A_B")
}
climatic_variables <- c("temp", "pr", "rh")
seasons <- c("winter", "spring", "summer", "fall")
landuse_variables <- envVariables[which(envVariables %in% variable_codes2)]
start_year_raster <- 1901

for (threshold in thresholds){
  print(paste0("Extraction for threshold ", threshold))
  df_threshold <- global_warming_windows_list[[as.character(threshold)]]
  #for certain thresholds, some models (the least warm ones) don't cross -> remove those lines
  df_threshold <- df_threshold[!is.na(df_threshold$central_year),]
  
  #land-use window from the marker scenario
  start_year_marker <- landuse_21window$start_year[which(landuse_21window$threshold == threshold)]
  end_year_marker <- landuse_21window$end_year[which(landuse_21window$threshold == threshold)]
  start_layer_marker<- as.numeric(start_year_marker) - start_year_raster + 1 #land use = historical LUH2 1901-2014 + SSP3 2015-2100, so layer 1 = 1901
  end_layer_marker<- as.numeric(end_year_marker) - start_year_raster + 1
  
  for (i in 1:nrow(df_threshold)){
    model_name <- df_threshold$model[i]
    scenario <- df_threshold$ssp[i]
    key <- paste0(model_name, "_", scenario)
    
    out_env <- paste0("WNV/Output/data_extraction_t0_BRTs_A_B/", threshold, "/", key, "_markerSSP3/")
    if (file.exists(paste0(out_env, "extracted_env_values.tsv"))) next #already extracted
    dir.create(out_env, recursive = TRUE, showWarnings = FALSE)
    
    seasonal_indices <- get_seasonal_indices(year_start = df_threshold$start_year[i],
                                             year_end = df_threshold$end_year[i],
                                             start_year_raster = start_year_raster)
    covariates <- list()
    for (variable in climatic_variables){
      mat <- climatic_data[[key]][[variable]]
      for (season in seasons) {
        layers <- mat[[seasonal_indices[[season]]]]
        if (length(layers) == 0) {
          covariates[[paste0(variable, "_", season)]] <- NA
        } else {
          covariates[[paste0(variable, "_", season)]] <-
            units_convertion(variable, terra::mean(layers, na.rm = TRUE)) #average per season and years
        }
      }
      if (variable == "temp"){
        covariates[[paste0(variable, "_var")]] <- covariates[[paste0(variable, "_summer")]] -
          covariates[[paste0(variable, "_winter")]]
      }
    }
    
    #for land-use we use the marker scenario
    for (var in landuse_variables){
      mat <- landcover_data[[marker_scenario]][[var]]
      covariates[[var]] <- terra::mean(mat[[start_layer_marker:end_layer_marker]], na.rm = TRUE)
    }
    
    covariates <- covariates[names(covariates) %in% envVariables]
    
    extracted_list <- lapply(1:length(covariates), function(j) {
      fun <- ifelse(names(covariates)[j] == "totalPopulation", "sum", "mean")
      exact_extract(covariates[[j]], nutsM_sf, fun = fun)
    })
    names(extracted_list) <- names(covariates)
    nutsM_data <- cbind(data.frame(nutsM_code = nutsM_sf$LocationCo), as.data.frame(extracted_list))
    
    stopifnot(all(envVariables %in% names(nutsM_data))) # all predictors present
    
    write_tsv(nutsM_data, paste0(out_env, "extracted_env_values.tsv"))
    cat("Extracted:", threshold, key, "\n")
  }
}

#average among replicates
average_replicates <- function(predictions_list) {
  combined <- do.call(rbind, predictions_list)
  colMeans(combined)
}

#projections for each BRT model
suitability_t0 <- list()
for (model in names(models)){
  cat("Projections with:", model, "\n")
  brt_model_list <- BRTs[[model]]
  
  for (threshold in thresholds){
    df_threshold <- global_warming_windows_list[[as.character(threshold)]]
    df_threshold <- df_threshold[!is.na(df_threshold$central_year),]
    GCM_ssp_predictions <- list()
    
    for (i in 1:nrow(df_threshold)){
      key <- paste0(df_threshold$model[i], "_", df_threshold$ssp[i])
      out_env <- paste0("WNV/Output/data_extraction_t0_BRTs_A_B/", threshold, "/", key, "_markerSSP3/")
      out_pred <- paste0("WNV/Output/data_extraction_t0_BRTs_A_B/", threshold, "/", key, "_markerSSP3/")
      
      nutsM_data <- as.data.frame(read_tsv(paste0(out_env, "extracted_env_values.tsv"), show_col_types = FALSE))
      
      predictions <- list()
      for (k in 1:length(brt_model_list)){
        object <- brt_model_list[[k]]
        prediction <- predict.gbm(object, nutsM_data[, -1], n.trees = object$gbm.call$best.trees,
                                  type = "response", single.tree = FALSE)
        temp <- data.frame(nutsM = nutsM_data$nutsM_code, predicted_suitability = prediction)
        write_tsv(temp, paste0(out_pred, "predicted_suitability_", k, ".tsv"))
        predictions[[k]] <- prediction
      }
      
      GCM_ssp_predictions[[key]] <- data.frame(nutsM = nutsM_data$nutsM_code,
                                               suitability = average_replicates(predictions))
    }
    suitability_t0[[model]][[as.character(threshold)]] <- GCM_ssp_predictions
  }
  
  rm(brt_model_list); gc()
}



#average across model and ssp combinations
suitability_t0_averaged <- lapply(suitability_t0, function(model){
  lapply(model, function(threshold){
    temp <- lapply(threshold, function(df) df[, "suitability"])  #extract suitability column from each of the dfs
    average_temp <- Reduce("+", temp) / length(temp)
    data.frame(nutsM = threshold[[1]][, "nutsM"],suitability = average_temp)
  })
})


#Plotting
colourScale <- rev(colorRampPalette(brewer.pal(11, "RdBu"))(101))
colourScale1 = rev(colorRampPalette(brewer.pal(11,"RdBu"))(121)[11:111]) #to map ecological suitability between 0 and 1 like SD
colourScale1 = rev(colorRampPalette(brewer.pal(11,"RdBu"))(121)[11:111])
colourScale2 = rev(colorRampPalette(brewer.pal(11,"PuOr"))(19)[2:18])
colourScale2[9] = paste0(colourScale2[9],"00") # to make that one transparent
colourScale3 = colourScale2; colourScale3[9] = "#E5E5E5"  #to map difference in ecological suitability, centered on 0 and with the values around 0 in grey


plot_threshold <- function(df, title = NULL){
  sf_data <- nutsM_sf
  sf_data$suitability <- df$suitability[match(sf_data$LocationCo, df$nutsM)]
  ggplot() +
    geom_sf(data = sf_data, aes(fill = suitability), color = NA) +
    geom_sf(data = contour_sf, fill = NA, color = "gray30", linewidth = 0.1) +
    scale_fill_gradientn(colors = colourScale1, limits = c(0,1), name = "Ecological\nsuitability") +
    ggtitle(title) +
    theme_bw() +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank())
}

plots <- lapply(names(suitability_t0_averaged), function(model){
  plot_threshold(suitability_t0_averaged[[model]][["1.3"]], title = paste0(model, " (+1.3 °C)"))
})

pdf("WNV/Figures/suitability_t0_BRTs_A_B.pdf", width = 15, height = 10)
cowplot::plot_grid(plotlist = plots, ncol = 2)
dev.off()
