#############################################################
#       FUTURE PROJECTIONS AT DIFF WARMING LEVELS          #
#############################################################

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

options(scipen=999)

thresholds <- c(1.3,1.9,2.3,2.5,2.8)#looking at the emission gap report 2025 with a prob of 66% of staying below that thresold.
MODELS_isimip3b <- c("CESM2-WACCM","CNRM-CM6-1","CNRM-ESM2-1","CanESM5",
                     "EC-Earth3","GFDL-ESM4","IITM-ESM","IPSL-CM6A-LR",
                     "KACE-1-0-G","MIROC6","MPI-ESM1-2-HR","MRI-ESM2-0",
                     "TaiESM1","UKESM1-0-LL")
scenarios <- c("ssp126", "ssp370", "ssp585")

##1. Load data

nutsM<-readRDS("WNV/Processed_data/nutsM_EU.rds")
nutsM_sf<-readRDS("WNV/Processed_data/nutsM_sf.rds")
nutsM_IDs <- read_tsv("WNV/Processed_data/nutsM_IDs.tsv")
contour_sf <- readRDS("WNV/Processed_data/europe_mask.rds")

BRTs <- readRDS("WNV/Output/2026-10-30_BRTs_A_B.rds")
envVariables <- c("temp_winter","temp_spring","temp_summer" ,"temp_fall" ,"temp_var","pr_spring","pr_summer",
                  "rh_winter","rh_summer","rh_fall","pastures","rangelands","urbanAreas","primaryForest",
                  "secondaryNonForest", "populationDensity") #shadow_testing_STGs

#load helper functions
source("WNV/helper_functions/get_seasonal_indices.r")
source("WNV/helper_functions/units_convertion.R")
source("WNV/helper_functions/load_gcm_rasters.R")
source("WNV/helper_functions/load_landcover_scenario_NEW.R")

#load climate warming level windows
landuse_21window <- read_tsv("WNV/Output/All_models_thresholds_66%chance_markerScenario.tsv")

GCMs_21window <- read_tsv("WNV/Output/All_models_thresholds_66%chance.tsv")
global_warming_windows <- function(df,thresh){
  tab <- df[,c("model", "ssp", paste0("thresh_",thresh))]
  colnames(tab) <- c("model","ssp","central_year")
  tab$start_year <- tab$central_year -10
  #tab$end_year <- tab$central_year +10
  tab$end_year <- pmin(tab$central_year + 10, 2100)
  return(tab)
}

global_warming_windows_list <- lapply(thresholds, function(t) {
  global_warming_windows(GCMs_21window, thresh = t)
})
names(global_warming_windows_list) <- thresholds

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

##2. Data extraction

if(!dir.exists("WNV/Output/data_extraction_GWL")){
  dir.create("WNV/Output/data_extraction_GWL")
}
climatic_variables <- c("temp", "pr", "rh")
seasons <- c("winter", "spring", "summer", "fall")
landuse_variables <- envVariables[which(envVariables %in% variable_codes2)]
start_year_raster <- 1901

for (threshold in thresholds) {
  print(paste0("working on threshold " ,threshold))
  df_threshold <- global_warming_windows_list[[as.character(threshold)]]
  
  #for certain threshold, there're some models (the least warm models) that don't cross, so we remove those lines
  if(sum(is.na(df_threshold$central_year)>0)){
    df_threshold <- df_threshold[!is.na(df_threshold$central_year),]
  }
  
  for (i in 1:nrow(df_threshold)){
    model_name <- df_threshold$model[i]
    scenario <- df_threshold$ssp[i]
    key <- paste0(model_name, "_", scenario)
    start_year <- df_threshold$start_year[i]
    end_year <-df_threshold$end_year[i]
    
    seasonal_indices <- get_seasonal_indices(year_start = start_year,
                                             year_end = end_year,
                                             start_year_raster = 1901)
    covariates <- list()
    for (variable in climatic_variables) {
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
      if (variable == "temp") {
        covariates[[paste0(variable, "_var")]] <- covariates[[paste0(variable, "_summer")]] - 
          covariates[[paste0(variable, "_winter")]]
      }
    }
    covariates <- covariates[names(covariates) %in% envVariables]
    
    #for land-use we will use the marker scenario
    start_year_marker <- landuse_21window$start_year[which(landuse_21window$threshold == threshold)]
    end_year_marker <- landuse_21window$end_year[which(landuse_21window$threshold == threshold)]
    start_layer_marker<- as.numeric(start_year_marker) -start_year_raster + 1
    end_layer_marker<- as.numeric(end_year_marker) -start_year_raster + 1
    
    for (var in landuse_variables) {
      mat <- landcover_data[[marker_scenario]][[var]]
      layers <- mat[[start_layer_marker:end_layer_marker]]
      if (length(layers) == 0) {
        covariates[[var]] <- NA
      }else{
        covariates[[var]] <-terra::mean(layers, na.rm = TRUE)
      }
    }
    
    stopifnot(all(envVariables %in% names(covariates))) # all predictors present
    
    nutsM_data<- as.data.frame(nutsM_IDs$nutsM_code)
    colnames(nutsM_data)<-"nutsM_code"
    
    extracted_list <- lapply(1:length(covariates), function(j) {
      fun <- ifelse(names(covariates)[j] == "totalPopulation", "sum", "mean")
      exact_extract(covariates[[j]], nutsM_sf, fun = fun)
    })
    
    names(extracted_list) <- names(covariates)
    extracted_df <- as.data.frame(extracted_list)
    nutsM_data <- cbind(nutsM_data,extracted_df)
    
    if(!dir.exists(paste0("WNV/Output/data_extraction_GWL/",threshold,"/",key,"_markerSSP3-70"))){
      dir.create(paste0("WNV/Output/data_extraction_GWL/",threshold,"/",key,"_markerSSP3-70"), recursive = TRUE)
    }
    
    write_tsv(nutsM_data, paste0("WNV/Output/data_extraction_GWL/",
                                 threshold,"/",key,"_markerSSP3-70/extracted_env_values.tsv"))
  }
}


#4. Perform predictions

brt_model_list <- BRTs[["model_B"]]

#average among replicates
average_replicates <- function(predictions_list) {
  combined <- do.call(rbind, predictions_list)
  colMeans(combined)
}

suitability_GWL <- list()
for (threshold in thresholds){
  print(paste0("working on threshold " ,threshold))
  
  df_threshold <- global_warming_windows_list[[as.character(threshold)]]
  df_threshold <- df_threshold[!is.na(df_threshold$central_year),]
  GCM_ssp_predictions <- list()
  
  for (i in 1:nrow(df_threshold)){
    key <- paste0(df_threshold$model[i], "_", df_threshold$ssp[i])
    out_env <- paste0("WNV/Output/data_extraction_GWL/", threshold, "/", key, "_markerSSP3-70/")
    out_pred <- paste0("WNV/Output/data_extraction_GWL/", threshold, "/", key, "_markerSSP3-70/")
    
    nutsM_data <- as.data.frame(read_tsv(paste0(out_env, "extracted_env_values.tsv"), show_col_types = FALSE))
    
    predictions <-list()
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
  suitability_GWL[[as.character(threshold)]] <- GCM_ssp_predictions
}

sapply(suitability_GWL, length)
#average across model and ssp combinations
suitability_GWL_averaged <- lapply(suitability_GWL, function(threshold){
  temp <- lapply(threshold, function(df) df[, "suitability"])
  average_temp <- Reduce("+", temp) / length(temp)
  data.frame(nutsM = threshold[[1]][, "nutsM"], suitability = average_temp)
})

saveRDS(suitability_GWL, "WNV/Output/2026-10-30_suitability_GWL_modelB_per_GCM_SSP.rds")
saveRDS(suitability_GWL_averaged, "WNV/Output/2026-10-30_suitability_GWL_modelB.rds")

#5. Plotting
colourScale <- rev(colorRampPalette(brewer.pal(11, "RdBu"))(101))
colourScale1 = rev(colorRampPalette(brewer.pal(11,"RdBu"))(121)[11:111]) #to map ecological suitability between 0 and 1
colourScale1 = rev(colorRampPalette(brewer.pal(11,"RdBu"))(121)[11:111])
colourScale2 = rev(colorRampPalette(brewer.pal(11,"PuOr"))(19)[2:18])
colourScale2[9] = paste0(colourScale2[9],"00") # to make that one transparent
colourScale3 = colourScale2; colourScale3[9] = "#E5E5E5"  #to map difference in ecological suitability, centered on 0 and with the values around 0 in grey

plot_threshold <- function(df){
  sf_data <- nutsM_sf
  sf_data$suitability <- df$suitability[match(sf_data$LocationCo, df$nutsM)]
  ggplot() +
    geom_sf(data = contour_sf, fill = NA, color = "gray30", size = 0.005) +  # thinner than 0.4
    geom_sf(data = sf_data, aes(fill = suitability),color = NA, size = 0.005) +
    scale_fill_gradientn(colors = colourScale1, limits = c(0,1), name = "Ecological suitability") +
    theme_bw() +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank())
}

plots <- lapply(suitability_GWL_averaged, plot_threshold)
legend <- get_legend(plots[[1]])
plots_no_legend <- lapply(plots, function(p) p + theme(legend.position = "none"))

merged_plots <- plot_grid(plotlist = plots_no_legend, ncol = 5,nrow = 1, labels = names(plots_no_legend))
final_plot <- plot_grid(merged_plots,legend,rel_widths = c(1, 0.15))

pdf("WNV/Figures/Future_projections_GWL_modelB.pdf", width = 15, height = 7)
final_plot
dev.off()

#plot the difference
current_threshold <- suitability_GWL_averaged[["1.3"]]
new_thresholds <- c("1.9", "2.3", "2.5", "2.8")

difference_th <- lapply(new_thresholds, function(threshold){
  df <- suitability_GWL_averaged[[threshold]]
  merged <- merge(df, current_threshold, by = "nutsM", suffixes = c("_new", "_ref"))
  data.frame(nutsM = merged$nutsM,suitability = merged$suitability_new - merged$suitability_ref)
})
names(difference_th) <- new_thresholds

all_values <-unlist(lapply(difference_th, function(df) df$suitability))
max_abs <-max(abs(all_values), na.rm = TRUE)
#min_value <-min(all_values)

plot_diff <- function(df){
  sf_data <- nutsM_sf
  sf_data$suitability <- df$suitability[match(sf_data$LocationCo, df$nutsM)]
  
  ggplot() +
    geom_sf(data=contour_sf, fill = NA, color = "gray30", size = 0.005) +
    geom_sf(data=sf_data, aes(fill = suitability), color = NA, size = 0.005) +
    scale_fill_gradientn(colors = colourScale3, limits = c(-max_abs, max_abs), name = "Difference t0") +
    theme_bw() +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank())
}

diff_plots <- lapply(difference_th, plot_diff)
diff_plots <- lapply(diff_plots, function(p) 
  p + theme(plot.margin = margin(2, 2, 2, 2))) #adding margins

diff_legend <- get_legend(diff_plots[[1]] + theme(legend.position = "right"))
diff_plots_no_legend <- lapply(diff_plots, function(p) p + theme(legend.position = "none"))

maps <- plot_grid(plotlist = diff_plots_no_legend, ncol = 4,labels = names(diff_plots_no_legend))
maps_with_legend <- plot_grid(maps, diff_legend, ncol = 2, rel_widths = c(1, 0.15))

pdf("WNV/Figures/Future_projections_GWL_diff_modelB.pdf", width = 15, height = 5)
maps_with_legend
dev.off()

## plot diff maps with density plots inside ##

global_warming_windows_diff <- list()
for (threshold in new_thresholds){
  df <- global_warming_windows_list[[threshold]]
  df$start_year_marker <- landuse_21window$start_year[which(landuse_21window$threshold == as.numeric(threshold))]
  df$end_year_marker <- landuse_21window$end_year[which(landuse_21window$threshold == as.numeric(threshold))]
  global_warming_windows_diff[[threshold]] <- df
}

plotting_windows_marker <- function(df,thresh){
  df <- df[df$threshold == thresh,]
  ggplot(df, aes(y = model, color = scenario)) +
    geom_segment(aes(x = start_year, xend = end_year, yend = model),linewidth=2) +
    xlim(c(1995, 2100))+
    ylab(NULL) +
    xlab(NULL) +
    scale_color_manual(values = c("SSP3-Baseline" = "#81b29a")) +
    theme_bw()
}

marker_plots <- lapply(thresholds, function(t) {
  plotting_windows_marker(landuse_21window, thresh = t)
})

diff_maps_density_plots <- function(df1, df2){
  sf_data <- nutsM_sf
  sf_data$suitability <- df1$suitability[match(sf_data$LocationCo, df1$nutsM)]
  
  map_plot <- ggplot() +
    geom_sf(data = contour_sf, fill = NA, color = "gray30", size = 0.005) +
    geom_sf(data = sf_data, aes(fill = suitability), color = NA, size = 0.005) +
    scale_fill_gradientn(colors = colourScale3, limits = c(-max_abs, max_abs), name = "Difference t0") +
    theme_bw() +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank())
  
  tab <- c()
  for (k in 1:nrow(df2)) {
    if (!is.na(df2$central_year[k])) {
      years <- seq(df2$start_year[k], df2$end_year[k])
      tab <- c(tab, years)
    }
  }
  df_threshold <- data.frame(years = tab)
  
  density_plot <- ggplot(df_threshold, aes(x = years)) +
    geom_density(alpha = 0.3) +
    labs(x = "year", y = "density") +
    xlim(c(2010,2100))+
    theme_bw() +
    theme(axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          strip.text = element_blank(),
          strip.background = element_blank())
  
  marker_plot <- ggplot(df2, aes(x = start_year_marker, xend = end_year_marker, y = 1, yend = 1)) +
    geom_segment(linewidth = 2, color = "#81b29a") +
    xlim(c(2010, 2100)) +
    ylab(NULL) +
    xlab("year") +
    theme_bw() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid = element_blank())
  
  map_plot_no_legend <- map_plot + theme(legend.position = "none")
  
  combined_plots <- plot_grid(map_plot_no_legend, density_plot, marker_plot, ncol = 1,
                              rel_heights = c(4, 1, 1), align = "v", axis = "lr")
  
  list(combined = combined_plots, legend = get_legend(map_plot))
}

diff_plots <- Map(diff_maps_density_plots, difference_th, global_warming_windows_diff)
combined_list <- lapply(diff_plots, function(x) x$combined) #pulling the plots.
shared_legend <- diff_plots[[1]]$legend #pulll only the first legend
maps_density_row <- plot_grid(plotlist = combined_list, ncol = 4,labels = names(combined_list),align = "hv", axis = "tblr")

pdf("WNV/Figures/Future_projections_GWL_diff_density_modelB.pdf",width = 10, height = 5)
plot_grid(maps_density_row, shared_legend,ncol = 2, rel_widths = c(1, 0.15))
dev.off()

##3.3 Plot uncertainty

suitability_GWL_stats <- lapply(suitability_GWL, function(thresh) {
  temp <- lapply(thresh, function(df) df[, "suitability"])#extract suitability column from each of the 42 dfs
  merged_df <- do.call(cbind, temp) #make a matrix, rows NUT3 and cols GCM.SSP
  data.frame(nutsM=thresh[[1]][, "nutsM"],suitability = rowMeans(merged_df),
             sd=apply(merged_df, 1, sd, na.rm = TRUE),
             q05=apply(merged_df, 1, quantile, probs = 0.05, na.rm = TRUE),
             q95=apply(merged_df, 1, quantile, probs = 0.95, na.rm = TRUE))
})

max(unlist(lapply(suitability_GWL_stats, function(df) df$sd)))

plotting_SD <- function(df){
  sf_data <- nutsM_sf
  sf_data$sd <- df$sd[match(sf_data$LocationCo, df$nutsM)]
  ggplot() +
    geom_sf(data = contour_sf, fill = NA, color = "gray30", size = 0.005) + 
    geom_sf(data = sf_data, aes(fill = sd),color = NA, size = 0.005) +
    scale_fill_viridis_c(option = "cividis", name = "SD", limits = c(0,0.35), breaks = seq(0, 1, by = 0.05)) +
    theme_minimal() +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank())
}

plots_SD <- lapply(suitability_GWL_stats, plotting_SD)
legend <- get_legend(plots_SD[[1]])
plots_SD_no_legend <- lapply(plots_SD, function(p) p + theme(legend.position = "none"))

merged_plots_SD <- plot_grid(plotlist = plots_SD_no_legend, ncol = 5,nrow = 1, labels = names(plots_SD_no_legend))
final_plots_SD <- plot_grid(merged_plots_SD,legend,rel_widths = c(1, 0.15))

## plotting uncertinity coming from BRT
key <- names(suitability_GWL[[1]])

get_BRTs_replicates <- function(threshold,key){
  path <- paste0("WNV/Output/data_extraction_GWL/",threshold,"/",key,"_markerSSP3-70")
  files <- list.files(path, pattern = "predicted_suitability", full.names = TRUE)
  temp <- lapply(files, function(f) read_tsv(f, show_col_types = FALSE,progress = F)) #load files aas list
  suit_values <- lapply(temp, function(df) df[, "predicted_suitability"]) #extract the pred.suit values
  merged_suit_values <- do.call(cbind, suit_values)#make a matrix, rows NUT3 and cols each key
  data.frame(nutsM=temp[[1]]$nutsM, brt_mean=rowMeans(merged_suit_values, na.rm = TRUE),
             sd = apply(merged_suit_values, 1, sd, na.rm = TRUE))
}

BRT_sd_key_thresholds <- lapply(names(suitability_GWL), function(th){
  keys_per_threshold <- names(suitability_GWL[[th]])
  per_model <- lapply(keys_per_threshold, function(k) get_BRTs_replicates(th, k))
  names(per_model) <- keys_per_threshold
  per_model
})
names(BRT_sd_key_thresholds) <- names(suitability_GWL)

#mean sd deviation among BRT replicates
average_brt_sd <- function(th){
  sd_values <- lapply(th, function(df) df[,"sd"])
  merged_sd_values <- do.call(cbind, sd_values)
  data.frame(nutsM=th[[1]]$nutsM, sd = rowMeans(merged_sd_values, na.rm = TRUE))
}

BRT_sd_thresholds <- lapply(BRT_sd_key_thresholds, average_brt_sd)

plots_SD_BRT <- lapply(BRT_sd_thresholds, plotting_SD)
legend_SD_BRT <- get_legend(plots_SD_BRT[[1]])
plots_SD_BRT_no_legend <- lapply(plots_SD_BRT, function(p) p + theme(legend.position = "none"))

merged_plots_SD_BRT<- plot_grid(plotlist = plots_SD_BRT_no_legend, ncol = 5,nrow = 1, labels = names(plots_SD_BRT_no_legend))
final_plots_SD_BRT <- plot_grid(merged_plots_SD_BRT,legend_SD_BRT,rel_widths = c(1, 0.15))

SD_BRT_ensamble <- plot_grid(final_plots_SD_BRT, final_plots_SD, ncol =1,
                             rel_widths = c(1, 1), rel_heights=c(1, 1))

pdf("WNV/Figures/Future_projections_uncertainty.pdf", width = 15, height = 10)
SD_BRT_ensamble
dev.off()
