######################################################
##      Estimating eco suit per country             ##
######################################################

require(readr)
require(dplyr)
require(plyr)
require(sf)
require(ggplot2)
require(RColorBrewer)

nutsM_sf<-readRDS("WNV/Processed_data/nutsM_sf.rds")
nutsM_IDs <- read_tsv("WNV/Processed_data/nutsM_IDs.tsv")
contour_sf <- readRDS("WNV/Processed_data/europe_mask.rds")

thresholds <- c(1.3,1.9,2.3,2.5,2.8)
suitability_GWL <- readRDS("WNV/Output/2026-10-30_suitability_GWL_modelB_per_GCM_SSP.rds")
n.iterations<- 100

#1. load predictions per GCM-SSP combo and BRT replicates
suitability_GWL_replicates <- list()
for (threshold in names(suitability_GWL)){
  print(paste0("working on threshold ", threshold))
  suitability_GWL_replicates[[threshold]] <- list()
  for (key in names(suitability_GWL[[threshold]])){
    path <- paste0("WNV/Output/data_extraction_GWL/", threshold, "/", key, "_markerSSP3-70/")
    for (k in 1:n.iterations){
      df <- as.data.frame(read_tsv(paste0(path, "predicted_suitability_", k, ".tsv"), show_col_types = FALSE, progress = FALSE))
      colnames(df) <- c("nutsM", "suitability")
      suitability_GWL_replicates[[threshold]][[paste0(key, "_BRT", k)]] <- df
    }
  }
}
sapply(suitability_GWL_replicates, length) #should be 17500 in total

#2.Get population values
#I will use the popDensity already extarcted per each GCM-SSP combo
nutsM_area <- as.numeric(st_area(nutsM_sf)) / 1e6 #km2

get_population <- function(threshold, key){
  env <- as.data.frame(read_tsv(paste0("WNV/Output/data_extraction_GWL/", threshold, "/", key,
                                       "_markerSSP3-70/extracted_env_values.tsv"), show_col_types = FALSE))
  data.frame(nutsM = env$nutsM_code,
             totalPop = env$populationDensity * nutsM_area[match(env$nutsM_code, nutsM_sf$LocationCo)])
}

population_GWL <- list()
for (threshold in names(suitability_GWL)){
  key <- names(suitability_GWL[[threshold]])[1]
  population_GWL[[threshold]] <- get_population(threshold, key)
}

#merge to have eco suita and total pop counts
suitability_GWL_replicates_totalPop <- list()
for (threshold in thresholds){
  temp <- suitability_GWL_replicates[[as.character(threshold)]]
  
  nutsM_totalPop <- population_GWL[[as.character(threshold)]] #same popdens per GWL
  
  temp_list <- list()
  for (model in names(temp)){
    df <- temp[[model]]
    df <- merge(df, nutsM_totalPop, by="nutsM")
    temp_list[[model]] <- df
  }
  suitability_GWL_replicates_totalPop[[as.character(threshold)]] <- temp_list
  
}

#3. aggregating at country level using pop weighted approach 
country_nutsM <- nutsM_sf[,c("LocationCo", "CountryNam")] %>% st_drop_geometry()

suitability_totalPop_country <- list()
for (threshold in thresholds){
  temp <- suitability_GWL_replicates_totalPop[[as.character(threshold)]]
  suitability_GCM_SSP_BRT <- list()
  for (model in names(temp)){
    df <- temp[[model]]
    df <- merge(df, country_nutsM, by.x = "nutsM", by.y = "LocationCo")
    df <- df[!is.na(df$suitability), ]
    df$weighted_suitability <- df$suitability * df$totalPop
    df_suit_country <- aggregate(cbind(weighted_suitability, totalPop) ~ CountryNam, data=df,FUN=sum)
    df_suit_country$suitability_mean <- df_suit_country$weighted_suitability / df_suit_country$totalPop
    
    suitability_GCM_SSP_BRT[[model]] <- df_suit_country
  }
  suitability_totalPop_country[[as.character(threshold)]] <- suitability_GCM_SSP_BRT
}

flat_list <- list()
i <- 1
for (threshold in names(suitability_totalPop_country)) {
  for (model in names(suitability_totalPop_country[[threshold]])) {
    df <- suitability_totalPop_country[[threshold]][[model]]
    df$model <- model
    df$threshold <- threshold
    flat_list[[i]] <- df[, c("CountryNam", "suitability_mean", "threshold", "model")]
    i <- i + 1
  }
}

suitability_country_flat <- ldply(flat_list,rbind)

#first get one suit value per Country per replicate per model per threshold
country_model_level <- aggregate(suitability_mean ~ threshold + model + CountryNam,
                                 data = suitability_country_flat, FUN = mean, na.rm = TRUE)
nrow(country_model_level) #39*42*100 + 39*37*100 +39*34*100 + 39*32*100 + 39*30*100

country_model_level$replicate <- as.numeric(sub(".*_BRT", "", country_model_level$model)) #100

#within each threshold, average eco suit across model per BRT replicate. So in the end
#we get three eco suit (one per count) per BRT replicate per threhold = 1500
country_BRT_level <- aggregate(suitability_mean ~ threshold + replicate + CountryNam,
                               data = country_model_level,FUN = mean, na.rm = TRUE)

country_level <- aggregate(suitability_mean ~ threshold + CountryNam,
                           data = country_BRT_level,FUN = mean, na.rm = TRUE)

suit_2.8 <- country_level[country_level$threshold == "2.8",]
country_order <- suit_2.8$CountryNam[order(suit_2.8$suitability_mean)]
country_level$CountryNam <- factor(country_level$CountryNam, levels = country_order) #most suitable at 2.8 at the top


#4. Plotting
colourScale1 = rev(colorRampPalette(brewer.pal(11,"RdBu"))(121)[11:111]) #to map ecological suitability between 0 and 1 

pdf("WNV/Figures/EcoSuitcountry_heatmap.pdf", height = 7, width = 7)
ggplot(country_level, aes(x = threshold, y = CountryNam, fill = suitability_mean)) +
  geom_tile(color = "white") +
  scale_fill_gradientn(colors = colourScale1, limits = c(0, 1), name = "Ecological\nsuitability") +
  labs(x = "Threshold", y = "Country") +
  theme_bw()
dev.off()


country_summary <- country_BRT_level %>%
  dplyr::group_by(threshold, CountryNam) %>%
  dplyr::summarise(median = median(suitability_mean), q025 = quantile(suitability_mean, 0.025),
                   q975 = quantile(suitability_mean, 0.975), .groups = "drop")
country_summary$CountryNam <- factor(country_summary$CountryNam, levels = country_order)
