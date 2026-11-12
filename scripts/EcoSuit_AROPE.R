###############################################################################
##  Eco suitability overlaps spatially with socioeconomic vulnerability ?   ##
###############################################################################

require(readr)
require(plyr)
require(dplyr)
require(sf)
require(ggplot2)
require(cowplot)
require(RColorBrewer)
require(scales)
require(viridis)
require(SpatialPack)
library(biscale)
library(giscoR)
require(harmonicmeanp)

##1. Load arope values
nutsM_sf<-readRDS("WNV/Processed_data/nutsM_sf.rds")
nutsM_IDs <- read_tsv("WNV/Processed_data/nutsM_IDs.tsv")
contour_sf <- readRDS("WNV/Processed_data/europe_mask.rds")

nutsM_AROPE <- readRDS("WNV/Processed_data/arope_nuts2_2024.rds")
nuts2_key <- nutsM_AROPE[,c("LocationCo","NUTS2_code")] %>% st_drop_geometry()
nuts2_key <- nuts2_key[!is.na(nuts2_key$NUTS2_code),] 

thresholds <- c(1.3,1.9,2.3,2.5,2.8)
suitability_GWL <- readRDS("WNV/Output/2026-10-30_suitability_GWL_modelB_per_GCM_SSP.rds")
n.iterations<- 100

##2. load predictions per GCM-SSP combo and BRT replicates
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

##3.Get population values
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

##4.Aggregate values at NUTS2 level (arope values are at nuts2)

suitability_totalPop_NUTS2 <- list()
for (threshold in thresholds){
  temp <- suitability_GWL_replicates_totalPop[[as.character(threshold)]]
  suitability_GCM_SSP_BRT <- list()
  for (model in names(temp)){
    df <- temp[[model]]
    df <- merge(df, nuts2_key, by.x = "nutsM", by.y = "LocationCo")
    df <- df[!is.na(df$suitability) & !is.na(df$totalPop), ]
    df$weighted_suitability <- df$suitability * df$totalPop
    df_suit_NUTS2 <- aggregate(cbind(weighted_suitability, totalPop) ~ NUTS2_code, data=df,FUN=sum)
    df_suit_NUTS2$suitability_mean <- df_suit_NUTS2$weighted_suitability / df_suit_NUTS2$totalPop
    
    suitability_GCM_SSP_BRT[[model]] <- df_suit_NUTS2
  }
  suitability_totalPop_NUTS2[[as.character(threshold)]] <- suitability_GCM_SSP_BRT
}

nutsM_AROPE_df <- nutsM_AROPE[, c("NUTS2_code", "values")] %>%
  st_drop_geometry() %>%
  dplyr::distinct(NUTS2_code, .keep_all = TRUE) %>%
  dplyr::filter(!is.na(NUTS2_code) & !is.na(values)) #remove those rows that dont have nuts2 level or arope info

nrow(nutsM_AROPE_df)  #should 250

#three categorise of arope values: low, medium and high
breaks <- quantile(nutsM_AROPE_df$values, probs = c(1/3, 2/3))
nutsM_AROPE_df$vulnerability <- "low"
nutsM_AROPE_df$vulnerability[which(nutsM_AROPE_df$values >= breaks[1])] <- "medium"
nutsM_AROPE_df$vulnerability[which(nutsM_AROPE_df$values >= breaks[2])] <- "high"
nutsM_AROPE_df$vulnerability <- factor(nutsM_AROPE_df$vulnerability, 
                                       levels = c("low", "medium", "high"))
table(nutsM_AROPE_df$vulnerability)

flat_list <- list()
i <- 1
for (threshold in names(suitability_totalPop_NUTS2)) {
  for (model in names(suitability_totalPop_NUTS2[[threshold]])) {
    df <- suitability_totalPop_NUTS2[[threshold]][[model]]
    df$model <- model
    df$threshold <- threshold
    flat_list[[i]] <- df[, c("NUTS2_code", "suitability_mean", "threshold", "model")]
    i <- i + 1
  }
}

suitability_NUTS2_flat <- ldply(flat_list,rbind)
suitability_NUTS2_flat <- merge(suitability_NUTS2_flat, nutsM_AROPE_df[, c("NUTS2_code", "vulnerability")], 
                                by = "NUTS2_code")
nrow(suitability_NUTS2_flat) #4375000 = 250*42*100+ 250*37*100 + 250*34*100 + 250*32*100 + 250*30*100

##arrange data: for each warming level, each run GCM-SSP + BRT and each AROPE cat, it takes the average suitability of all NUTS2 regions in that category
cat_model_level <- aggregate(suitability_mean ~ threshold + model + vulnerability,
                             data = suitability_NUTS2_flat, FUN = mean, na.rm = TRUE)
nrow(cat_model_level) # 3*100*42+3*100*37+3*100*34+3*100*32+3*100*30 = 52500
cat_model_level$replicate <- as.numeric(sub(".*_BRT", "", cat_model_level$model))
cat_model_level$vulnerability <- factor(cat_model_level$vulnerability, levels = c("low", "medium", "high"))

#5. Plotting
pdf("WNV/Figures/suitabilit_AROPE.pdf", height = 3, width = 6)
ggplot(cat_model_level, aes(x = vulnerability, y = suitability_mean, fill = vulnerability)) +
  geom_boxplot(outlier.alpha = 0.4) +
  scale_fill_manual(values = c("low" = "#734f5a", "medium" = "#fee08b", "high" = "#941c2f"))+
  facet_wrap(~ threshold, nrow=1) +
  theme_bw()+
  theme(axis.text.x = element_blank())+
  labs(x = "Socioeconomic vulnerability", y = "Average ecological suitability")
dev.off()

options(scipen = 999)

pairwise_results <- data.frame(threshold = as.character(), lowVSmedium = as.numeric(), lowVShigh = as.numeric(), mediumVShigh = as.numeric())
results <- data.frame(threshold = as.character(), category = as.character(), median = as.numeric(),
                      q025 = as.numeric(), q975 = as.numeric(), mean = as.numeric())

for(threshold in thresholds){
  sub <- cat_model_level[cat_model_level$threshold == as.character(threshold),]
  sub <- sub[order(sub$model),] #same GCM-SSP x BRT run in the same position in every category
  stopifnot(all(sub$model[sub$vulnerability== "low"] ==sub$model[sub$vulnerability == "medium"]),
            all(sub$model[sub$vulnerability== "low"] ==sub$model[sub$vulnerability == "high"]))
  
  test <- pairwise.wilcox.test(sub$suitability_mean, sub$vulnerability, p.adjust.method = "BH", paired = TRUE)
  pairwise_results <- rbind(pairwise_results, data.frame(threshold = threshold, lowVSmedium = test$p.value[1,1],
                                                         lowVShigh = test$p.value[2,1], mediumVShigh = test$p.value[2,2]))
  
  for(level in levels(sub$vulnerability)){
    sub_level <- sub[which(sub$vulnerability == level),]
    results <- rbind(results, data.frame(threshold = threshold, category = level,
                                         median=median(sub_level$suitability_mean),
                                         q025=quantile(sub_level$suitability_mean, 0.025, names = FALSE),
                                         q975=quantile(sub_level$suitability_mean, 0.975, names = FALSE),
                                         mean=mean(sub_level$suitability_mean)))
  }
}

pairwise_results[, -1] <- lapply(pairwise_results[, -1], format.pval, eps = 0.001)

write_tsv(pairwise_results, "WNV/Output/pairwise_results_AROPE_suitability.tsv")
write_tsv(results, "WNV/Output/median_values_AROPE_suitability.tsv")



#6. Plotting bi-variate maps
#https://www.datawim.com/post/creating-professional-bivariate-maps-in-r/

nuts2 <- gisco_get_nuts(year = "2024",epsg = "4326",resolution = "10",nuts_level = "2")

#get one value of eco suit per nuts2
nuts2_suit_summary <- suitability_NUTS2_flat %>%
  dplyr::group_by(NUTS2_code, threshold) %>%
  dplyr::summarise(suitability_mean = mean(suitability_mean), .groups = "drop")

bivar_arope_cat <- nuts2_suit_summary %>%
  left_join(nutsM_AROPE_df[, c("NUTS2_code", "values")], by = "NUTS2_code") %>%
  dplyr::rename(arope = values) %>%
  filter(!is.na(arope)) 

bivar_arope_cat$arope_class <- ifelse(bivar_arope_cat$arope < breaks[1], 1,ifelse(bivar_arope_cat$arope < breaks[2], 2, 3))
bivar_arope_cat$suit_class <- ntile(bivar_arope_cat$suitability_mean, 3) 
bivar_arope_cat$bi_class <- paste(bivar_arope_cat$suit_class, bivar_arope_cat$arope_class, sep = "-")

bivar_arope_cat %>%
  dplyr::group_by(arope_class) %>%
  dplyr::summarise(min = min(arope), max = max(arope))

bivar_arope_cat_sf <- nuts2 %>%
  left_join(bivar_arope_cat, by = c("NUTS_ID" = "NUTS2_code"))

bivar_arope_cat_sf <- bivar_arope_cat_sf %>% filter(!is.na(threshold))

pallet <- "BlueGold"
bi_pal(pallet, dim=3)

map<- ggplot() +
  geom_sf(data = contour_sf, fill = NA, color = "grey90", linewidth = 0.1) +
  geom_sf(data = bivar_arope_cat_sf, aes(fill = bi_class), color = NA, linewidth = 0, show.legend = FALSE) +
  bi_scale_fill(pal = pallet, dim = 3, na.value = "grey90") +
  facet_wrap(~ threshold, nrow = 1) +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

legend <- bi_legend(pal = pallet,   flip_axes = FALSE,rotate_pal = FALSE,
                    dim = 3,
                    xlab = "suitability mean",
                    ylab = "arope",
                    size = 10)
finalPlot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) + 
  draw_plot(legend, 0.7, 0.005, 0.28, 0.28) 

pdf("WNV/Figures/bivariate_maps_vulnearble_cat.pdf", width = 7, height = 5)
finalPlot
dev.off()

bivar_arope_cat %>%
  dplyr::filter(bi_class == "3-3") %>%
  dplyr::count(threshold)

bivar_arope_cat %>% dplyr::filter(bi_class %in% c("3-1", "1-3")) %>% 
  dplyr::count(threshold, bi_class)

#7. Stats: Dutilleul modified t-test, is the overlap more than we expect by change? neighbouring regions are similar. The test accoutns for spatial autocorrelation.

nuts2_sf <- nutsM_AROPE[which(nutsM_AROPE$NUTS2_code %in% nutsM_AROPE_df$NUTS2_code), "NUTS2_code"] %>%
  st_transform(3035) %>%
  dplyr::group_by(NUTS2_code) %>%
  dplyr::summarise()
nuts2_coords <- data.frame(NUTS2_code = nuts2_sf$NUTS2_code,
                           st_coordinates(st_point_on_surface(nuts2_sf)) / 1000)
nrow(nuts2_coords) 

dutilleul_results <- data.frame()
for (threshold in names(suitability_totalPop_NUTS2)){
  print(paste0("working on threshold ", threshold))
  temp <- suitability_totalPop_NUTS2[[threshold]]
  
  for (model in names(temp)){
    #one test per GCM-SSP-BRT run
    suit <- temp[[model]][, c("NUTS2_code", "suitability_mean")]
    stopifnot(!any(duplicated(suit$NUTS2_code))) #one value per NUTS2
    
    df <- merge(suit, nutsM_AROPE_df, by = "NUTS2_code")
    df <- merge(df, nuts2_coords, by = "NUTS2_code")
    
    test <- modified.ttest(x = df$suitability_mean, y = df$values, coords = as.matrix(df[, c("X", "Y")]))
    
    dutilleul_results <- rbind(dutilleul_results, data.frame(threshold = threshold, model = model, n_NUTS2 = nrow(df),
                                                             rho = test$corr, dof = test$dof, p.value = test$p.value))
  }
}

dutilleul_summary <- dutilleul_results %>%
  dplyr::group_by(threshold) %>%
  dplyr::summarise(rho_median = median(rho),
                   rho_q025= quantile(rho, 0.025),
                   rho_q975= quantile(rho, 0.975),
                   p_median= median(p.value),
                   p_hmp=as.numeric(p.hmp(p.value, L = length(p.value))),
                   prop_p_below_0.05 = mean(p.value < 0.05))
dutilleul_summary


write_tsv(dutilleul_results, "WNV/Output/dutilleul_suitability_AROPE_replicates.tsv")
write_tsv(dutilleul_summary, "WNV/Output/dutilleul_suitability_AROPE_summary.tsv")
