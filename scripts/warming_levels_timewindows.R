###############################################
##    Estimate global warming levels         ##
###############################################

options(scipen=999)
require(readr)
require(plyr)
require(ggplot2)
require(viridis)
require(ggrepel)
require(patchwork)
MODELS_isimip3b <- c("CESM2-WACCM","CNRM-CM6-1","CNRM-ESM2-1","CanESM5",
                     "EC-Earth3","GFDL-ESM4","IITM-ESM","IPSL-CM6A-LR",
                     "KACE-1-0-G","MIROC6","MPI-ESM1-2-HR","MRI-ESM2-0",
                     "TaiESM1","UKESM1-0-LL")
scenarios <- c("ssp126", "ssp370", "ssp585")

nutsM<-readRDS("WNV/Processed_data/nutsM_EU.rds")
nutsM_sf<-readRDS("WNV/Processed_data/nutsM_sf.rds")
nutsM_IDs <- read_tsv("WNV/Processed_data/nutsM_IDs.tsv")
contour_sf <- readRDS("WNV/Processed_data/europe_mask.rds")

#1. Estimate climate warming windows

##calculate global mean baseline per model (1850-1900)
baseline = as.data.frame(matrix(ncol = 2, nrow=14)); colnames(baseline) = c("models", "temp")
baseline$models <- MODELS_isimip3b

for (m in 1:length(MODELS_isimip3b)){
  model_name <- MODELS_isimip3b[[m]]
  file <- list.files(path = paste0("Rasters/Environmental_rasters/gmst/historical/", model_name, "/"), pattern = "1850_2014.txt")
  dt <-  read_table(paste0("Rasters/Environmental_rasters/gmst/historical/", model_name, "/", file), col_names = FALSE, skip = 1, show_col_types = F)
  dt <- dt[,c(1,2)] ; colnames(dt) <- c("year", "tas_K")
  mean_tas <- mean(dt$tas_K[which(dt$year >= 1850 & dt$year <= 1900)]) # calculate the gloabl mean tas for the baseline period 1850 - 1900 
  baseline[m,"temp"] <- mean_tas -273.15 #convert from K to °C
}

##get the 1850-2100 timeseries
scenarios <- c("ssp126", "ssp370", "ssp585")
gmst_timeseries <- list()
for (m in 1:length(MODELS_isimip3b)){
  model_name <- MODELS_isimip3b[[m]]
  file <- list.files(path = paste0("Rasters/Environmental_rasters/gmst/historical/", model_name, "/"), pattern = "1850_2014.txt")
  hist_gmst <- read_table(paste0("Rasters/Environmental_rasters/gmst/historical/", model_name, "/", file), col_names = FALSE, skip = 1, show_col_types = F)
  hist_gmst <- hist_gmst[,c(1,2)] ; colnames(hist_gmst) <- c("year", "tas_K")
  for (s in 1:length(scenarios)){
    scenario = scenarios[[s]]
    file <- list.files(path = paste0("Rasters/Environmental_rasters/gmst/", scenario,"/",model_name, "/"), pattern = "2015_2100.txt")
    ssp_gmst <-  read_table(paste0("Rasters/Environmental_rasters/gmst/", scenario,"/",model_name,"/", file), col_names = FALSE, skip = 1, show_col_types = F)
    ssp_gmst <- ssp_gmst[,c(1,2)] ; colnames(ssp_gmst) <- c("year", "tas_K")
    merged_gmst <- rbind(hist_gmst,ssp_gmst)
    merged_gmst$model <- model_name; merged_gmst$scenario <- scenario
    gmst_timeseries[[paste0(model_name,"_",scenario)]] <- merged_gmst
  }
}

#find the first 21-year rolling window when the average gmst crosses the target threshold relative to a baseline (model-specific baseline)
thresholds <- c(1.3,1.9,2.3,2.5,2.8)#looking at the emission gap report 2025 with a prob of 66% of staying below that thresold.
results21window <- expand.grid(model = MODELS_isimip3b, ssp= scenarios, stringsAsFactors = FALSE)
for (threshold in thresholds) {
  results21window[[paste0("thresh_", threshold)]] <- NA
}
window <- 21  # 20-year rolling window
half_window <- floor(window / 2)
anormalies_list <- list()

for (k in 1:length(gmst_timeseries)){
  df <- gmst_timeseries[[k]]
  baseline_tas <- baseline$temp[which(baseline$models == unique(df$model))]
  df$anormalies <- (df$tas_K -273.15) - baseline_tas
  anormalies_list[[k]] <-df
  years <- df$year
  # 21-year rolling mean allowing partial windows
  rolling_gmst <- numeric(length(years))
  for (j in 1:length(years)) {
    start_index <- max(1, j - half_window)
    end_index <- min(length(years), j + half_window)
    rolling_gmst[j] <- mean(df$anormalies[start_index:end_index], na.rm = TRUE)
  }
  # determine first year crossing each threshold
  crossing_years <- numeric(length(thresholds))
  for (t in seq_along(thresholds)){
    threshold <- thresholds[t]
    index <- which(rolling_gmst >= threshold)
    if (length(index) > 0) {
      crossing_years[t] <- years[index[1]]
    }else{
      crossing_years[t] <- NA
    }
  }
  row_index <- which(results21window$model == unique(df$model) & 
                       results21window$ssp == unique(df$scenario))
  results21window[row_index, paste0("thresh_", thresholds)] <- crossing_years
}

write_tsv(results21window, "WNV/Output/All_models_thresholds_66%chance.tsv")

#2. Estimate warming level windows for land-use

ar6_gmst <- read_csv("Rasters/Environmental_rasters/ar6_snapshot_1774884382_SSP3.csv")
marker_scenerio_gmst <- ar6_gmst[which(ar6_gmst$Scenario == "SSP3-Baseline"),] #using the SSP3-baseline as it corresponds to SSP3-7 which corresponds to landuse from LUH2
stopifnot(nrow(marker_scenerio_gmst) == 1) 
temps <- as.numeric(marker_scenerio_gmst[, 6:106])
years <- as.numeric(colnames(marker_scenerio_gmst)[6:106])
marker_crossing_year <- data.frame(threshold = thresholds,central_year = NA)

window <- 21  # 21-year rolling window
half_window <- floor(window / 2)

for (i in 1:length(thresholds)) {
  rolling_gmst_marker <- numeric(length(years))
  
  #rolling window centered and truncated
  for (j in 1:length(years)) {
    start_index <- max(1, j - half_window)
    end_index <- min(length(years), j + half_window)
    rolling_gmst_marker[j] <- mean(temps[start_index:end_index], na.rm = TRUE) #estimate the mean of the rollign_window
  }
  index <- which(rolling_gmst_marker >= thresholds[i]) #crosses or equal to threshold
  if (length(index) > 0) {
    marker_year <- years[index[1]]
  }else{
    marker_year <- NA
  }
  marker_crossing_year$central_year[i] <- marker_year
}

marker_crossing_year$start_year <- marker_crossing_year$central_year - half_window
marker_crossing_year$end_year <- pmin(marker_crossing_year$central_year + half_window, 2100) #max is 2100
marker_crossing_year$model <- "AIM-CGE-2.0" 
marker_crossing_year$scenario <- "SSP3-Baseline" 

write_tsv(marker_crossing_year, "WNV/Output/All_models_thresholds_66%chance_markerScenario.tsv")
marker_crossing_year <- read_tsv("WNV/Output/All_models_thresholds_66%chance_markerScenario.tsv")


#2. plotting data ##

## plotting gmst timeseries 
gmst_timeseries_plotting <- lapply(gmst_timeseries, function(df){
  df$tas_C <- df$tas_K -273.15
  return(df)
})

all_temp <- ldply(gmst_timeseries_plotting, rbind)
pdf("WNV/Figures/tas_allmodels.pdf", width=6, height=4)
ggplot(all_temp, aes(x = year, y = tas_C, color = model)) +
  geom_line(linewidth = 0.5) +
  facet_grid(scenario ~ .) +
  theme_bw() +
  scale_color_viridis(discrete = T, direction =-1) +
  labs(x = "Year", y = "Temperature (C)",color = "GCM")
dev.off()


##plotting anormalies per model
all_anomalies <- ldply(anormalies_list, rbind)
pdf("WNV/Figures/anomalies_allmodels.pdf", width=6, height=4)

hline_df <- data.frame(yintercept = c(1.9, 2.3, 2.5,2.8),label = c("1.9°C", "2.3°C", "2.5°C","2.8°C"))
ggplot(all_anomalies, aes(x = year, y = anormalies, color = model)) +
  geom_line(linewidth = 0.5) +
  geom_hline(data = hline_df, aes(yintercept = yintercept), linetype = "dashed") +
  geom_label_repel(data = hline_df, aes(x = 1850, y = yintercept, label = label), inherit.aes = FALSE, direction = "y",nudge_x = -20,
                   segment.color = "grey50") +
  facet_grid(scenario ~ .) +
  theme_bw() +
  scale_color_viridis(discrete = T, direction = -1) +
  labs(x = "Year", y = "Anormaly (C)", color = "GCM")
dev.off()

#plotting for the marker scenarios 

ar6_SSP3 <- read_csv("Rasters/Environmental_rasters/ar6_snapshot_1774884382_SSP3.csv") #anoralies
ar6_SSP2 <- read_delim("Rasters/Environmental_rasters/ar6-projected-gsat-scenarios.csv", 
                       delim = ";", show_col_types = FALSE)
ar6_SSP2 <- ar6_SSP2[!grepl("SSP3",ar6_SSP2$Scenario),]
ar6 <- rbind(ar6_SSP2,ar6_SSP3)


marker_long <- do.call(rbind, lapply(1:nrow(ar6), function(i){
  data.frame(year = years,anomaly = as.numeric(ar6[i, 6:106]),scenario = ar6$Scenario[i], model = ar6$Model[i])
}))
pdf("WNV/Figures/gsat_IAM.pdf", width=6, height=4)
ggplot(marker_long, aes(x = year, y = anomaly, colour = scenario)) +
  geom_line(linewidth = 0.5) +
  geom_hline(data = hline_df, aes(yintercept = yintercept), linetype = "dashed") +
  geom_label_repel(data = hline_df, aes(x = 2000, y = yintercept, label = label), 
                   inherit.aes = FALSE, direction = "y", nudge_x = -10,
                   segment.color = "grey50") +
  theme_bw() +
  scale_color_viridis(discrete = TRUE, direction = -1) +
  labs(x = "Year", y = "Anomalies°C", color = "Scenario")
dev.off()

#plot warming level windows per model and ssp
global_warming_windows <- function(df,thresh){
  tab <- df[,c("model", "ssp", paste0("thresh_",thresh))]
  colnames(tab) <- c("model","ssp","central_year")
  tab$start_year <- tab$central_year -10
  tab$end_year <- pmin(tab$central_year + 10, 2100)
  return(tab)
}

global_warming_windows_list <- lapply(thresholds, function(t) {
  global_warming_windows(results21window, thresh = t)
})
names(global_warming_windows_list) <- paste0("thresh_",thresholds)

plotting_windows <- function(df,thresh){
  plot <- ggplot(df, aes(y = model, color = ssp)) +
    geom_segment(aes(x = start_year, xend = end_year, yend = model), 
                 position = position_jitter(0.001),linewidth=2) +
    scale_color_manual(values = c("ssp126" ="#FFB569","ssp370" = "#BC5308","ssp585" = "#0F505F"))+
    xlim(c(1995, 2100))+
    ylab(NULL) +
    xlab(NULL) +
    theme_bw()+
    ggtitle(paste0("+",thresh, " degrees"))
  return(plot)
}

windows_plots <- lapply(names(global_warming_windows_list), function(name){
  plotting_windows(global_warming_windows_list[[name]], thresh = gsub("thresh_", "", name))
})

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
  plotting_windows_marker(marker_crossing_year, thresh = t)
})

#combining the plots
all_plots <- c(windows_plots, marker_plots)

combined <- wrap_plots(all_plots, nrow = 2, byrow = TRUE) +
  plot_layout(guides = "collect", heights = c(6, 0.5)) &
  theme(legend.position = "right", plot.margin = margin(0, 0, 0, 0))

pdf("WNV/Figures/windows_models_marker_66chance.pdf", width = 20, height = 7)
combined
dev.off()
