##############################################################
##      Estimating nb people living in eco suit. areas      ##
##############################################################
setwd("~/Dropbox/Postdoc_SpELL/GRP_Future_projections_Arbo")
require(readr)
require(plyr)
require(dplyr)
require(terra)
require(sf)
require(dismo)
require(gbm)
require(exactextractr)
require(ggplot2)
require(cowplot)
require(RColorBrewer)
require(scales)
require(viridis)
require(lmerTest)
require(emmeans)

##1. Load suiatbilities by warming level
nutsM_sf<-readRDS("WNV/Processed_data/nutsM_sf.rds")
nutsM_IDs <- read_tsv("WNV/Processed_data/nutsM_IDs.tsv")
contour_sf <- readRDS("WNV/Processed_data/europe_mask.rds")

thresholds <- c(1.3,1.9,2.3,2.5,2.8)
suitability_GWL <- readRDS("WNV/Output/2026-10-30_suitability_GWL_modelB_per_GCM_SSP.rds")
n.iterations <- 100

##2. Transform this to binary maps according to opt SI threshold maximum
BRT_perfomance <- read_csv("WNV/Output/2026-10-30_BRT_performance_model_B.csv")
opt_thresholds <- BRT_perfomance$opt_thresholds

#Binary maps per BRT replicate and GCM-SSP, using the opt. threshold of each replicate
binary_maps <- list()
for (threshold in names(suitability_GWL)){
  print(paste0("working on threshold ", threshold))
  binary_maps[[threshold]] <- list()
  for (key in names(suitability_GWL[[threshold]])){
    path <- paste0("WNV/Output/data_extraction_GWL/", threshold, "/", key, "_markerSSP3-70/")
    for (k in 1:n.iterations){
      df <- as.data.frame(read_tsv(paste0(path, "predicted_suitability_", k, ".tsv"), show_col_types = FALSE, progress = FALSE))
      colnames(df) <- c("nutsM", "suitability")
      df$suitable <- ifelse(df$suitability >= opt_thresholds[k], 1, 0)
      binary_maps[[threshold]][[paste0(key, "_BRT", k)]] <- df
    }
  }
}
sapply(binary_maps, length) #nb GCM-SSP*100 per GWL

#binary map for plotting: suitable if >= 50% of the GCM-SSP x BRT binary are > 1
binary_map_consensus <- lapply(binary_maps, function(thresh){
  temp <- lapply(thresh, function(df) df[, "suitable"])
  share_suitable <- Reduce("+", temp) / length(temp)
  data.frame(nutsM = thresh[[1]][, "nutsM"], suitability = share_suitable,
             suitable = ifelse(share_suitable >= 0.5, 1, 0))
})

## plot bonary maps on the average of models GCM-SSP
plot_binary_suit <- function(df){
  sf_data <- nutsM_sf[,c("LocationCo", "geometry")]
  sf_data$binary_suitability <- df$suitable[match(sf_data$LocationCo, df$nutsM)]
  ggplot() +
    geom_sf(data = contour_sf, fill = NA, color = "gray30", size = 0.005) +
    geom_sf(data = sf_data, aes(fill = factor(binary_suitability)), color = NA,size = 0.005) +
    scale_fill_manual(values = c("1" = "#941c2f", "0" = "#F6F4F1"),
                      name = "Binary ecological suitability ")+
    theme_minimal() +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank())
}

plots <- lapply(binary_map_consensus, plot_binary_suit)
legend <- get_legend(plots[[1]])
plots_no_legend <- lapply(plots, function(p) p + theme(legend.position = "none"))

merged_plots <- plot_grid(plotlist = plots_no_legend, ncol = 5,nrow = 1, labels = names(plots_no_legend))
final_plot <- plot_grid(merged_plots,legend,rel_widths = c(1, 0.15))

pdf("WNV/Figures/Future_projections_GWL_binaryMaps.pdf", width = 15, height = 7)
final_plot
dev.off()

##3.Get population values
#I will use the popDensity already extarcted per each GCM-SSP combo
nutsM_area <- as.numeric(st_area(nutsM_sf)) / 1e6 #km2

get_population <- function(threshold, key){
  env <- as.data.frame(read_tsv(paste0("WNV/Output/data_extraction_GWL/", threshold, "/", key,
                                       "_markerSSP3-70/extracted_env_values.tsv"), show_col_types = FALSE))
  data.frame(nutsM = env$nutsM_code,
             totalPop = env$populationDensity * nutsM_area[match(env$nutsM_code, nutsM_sf$LocationCo)])
}

#pop only depends on the warming level (marker window), not on the GCM-SSP
population_GWL <- list()
for (threshold in names(suitability_GWL)){
  key <- names(suitability_GWL[[threshold]])[1]
  population_GWL[[threshold]] <- get_population(threshold, key)
}

binary_maps_pop <- list()
for (threshold in thresholds){
  temp <- binary_maps[[as.character(threshold)]]
  
  nutsM_totalPop <- population_GWL[[as.character(threshold)]] #same popdens per GWL
  
  binary_maps_level <- list()
  for (model in names(temp)){
    df <- temp[[model]]
    df <- merge(df, nutsM_totalPop, by="nutsM")
    binary_maps_level[[model]] <- df
  }
  binary_maps_pop[[as.character(threshold)]] <- binary_maps_level
  
}

totalPop_GWL <- lapply(binary_maps_pop, function(thresh){
  sums <- sapply(thresh, function(df){
    sum(df$totalPop[df$suitable == 1], na.rm = TRUE)
  })
  data.frame(model = names(thresh),totalPop = sums)
})

totalPop_df <- ldply(totalPop_GWL, rbind) #nrow should be 17500
totalPop_df$GCM_SSP <- sub("_BRT[0-9]+$", "", totalPop_df$model)
totalPop_df$BRT <- sub(".*_BRT", "", totalPop_df$model)


pdf("WNV/Figures/totalPop_GWL.pdf", width = 5, height = 5)
p1 <- ggplot(totalPop_df, aes(x = .id,y = totalPop,fill=.id)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, direction = -1) +
  labs(y = "Total population count", x = "Warming level (+°C)") +
  theme_bw() +
  ggtitle("Varying Pop and eco suitability")+
  theme(legend.position = "none")
p1
dev.off()

totalPop_summary<- totalPop_df %>% dplyr::group_by(.id) %>%
  dplyr::summarise(median = median(totalPop) / 1e6,
                   q2.5 = quantile(totalPop, 0.025) / 1e6,
                   q97.5 = quantile(totalPop, 0.975) / 1e6)

write_tsv(totalPop_summary, "WNV/Output/total_pop_median_percentiles.tsv")

#percentage of change
#people at 1.3 for each run (GCM-SSP x BRT)
people1.3 <- totalPop_df[totalPop_df$.id == "1.3", c("model", "totalPop")]
colnames(people1.3)[2] <- "totalPop_1.3"

#% increase of each run vs the same run at 1.3
change_df <- merge(totalPop_df[totalPop_df$.id != "1.3",], people1.3, by = "model")
change_df$perc_increase <- (change_df$totalPop / change_df$totalPop_1.3 - 1) * 100

change_summary <- change_df %>%
  dplyr::group_by(.id) %>%
  dplyr::summarise(median = median(perc_increase),
                   q025 = quantile(perc_increase, 0.025),
                   q975 = quantile(perc_increase, 0.975))
change_summary

write_tsv(change_summary, "WNV/Output/percentage_increase_totalPop.tsv")

#4. is the average population at different GWL different ? 
#using linear mixed models

table(totalPop_df$.id)
model_log <- lmerTest::lmer(log(totalPop) ~ factor(.id) + (1 | GCM_SSP), 
                            data = totalPop_df)
summary(model_log)

## visualizing the results ##
plot(model_log)
qqnorm(resid(model_log)); qqline(resid(model_log))

predicted <- predict(model_log)
df <- data.frame(Observed = log(totalPop_df$totalPop),
                 Predicted = predicted)
pdf("WNV/Figures/totalpopCount_LLM_output.pdf", width = 5, height = 5)
ggplot(df, aes(x = exp(Observed), y = exp(Predicted))) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  xlab("Observed totalPop") +
  ylab("Predicted totalPop") +
  theme_bw()
dev.off()

results_LMM <- as.data.frame(summary(model_log)$coefficients)
results_LMM$fixed_effects <- rownames(results_LMM); rownames(results_LMM) <- NULL
colnames(results_LMM) <- c("estimate", "sd", "df", "t.value", "p.value","fixed_effects")
results_LMM<- results_LMM[, c("fixed_effects", "estimate", "sd", "df", "t.value", "p.value")]
write_tsv(results_LMM, "WNV/Output/results_LMM_totalpopCount.tsv")

## Estimating the Estimated Marginal Means, aka Least-Squares Means to do pairwise comparisons
emm <- emmeans(model_log, ~ .id)
pairs(emm)
pairs_emm <- as.data.frame(pairs(emm))

write_tsv(pairs_emm, "WNV/Output/results_EMM_totalPopCount.tsv")

##5. Robustness test

### Estimating human population living at eco suit areas while fixing human pop at +1.3

options(digits = 3, scipen = -2)
binary_maps_popFixed<- list()

for (threshold in thresholds){
  temp <- binary_maps[[as.character(threshold)]]
  
  if(threshold == 1.3){
    nutsM_totalPop <- population_GWL[[as.character(threshold)]]
    
    binary_maps_level <- list()
    for (model in names(temp)){
      df <- temp[[model]]
      df <- merge(df, nutsM_totalPop, by="nutsM")
      binary_maps_level[[model]] <- df
    }
    binary_maps_popFixed[[as.character(threshold)]] <- binary_maps_level
  }else{
    binary_maps_level <- list()
    for (model in names(temp)){
      df<- temp[[model]]
      nutsM_totalPop <- binary_maps_popFixed[["1.3"]][[model]][,c("nutsM","totalPop")]
      df <- merge(df, nutsM_totalPop, by="nutsM")
      binary_maps_level[[model]] <- df
    }
    binary_maps_popFixed[[as.character(threshold)]] <- binary_maps_level
  }
}

totalPop_GWL_popFixed <- lapply(binary_maps_popFixed, function(thresh){
  sums <- sapply(thresh, function(df){
    sum(df$totalPop[df$suitable == 1], na.rm = TRUE)
  })
  data.frame(model = names(thresh),totalPop = sums)
})
totalPop_popFixed_df <- ldply(totalPop_GWL_popFixed, rbind)
totalPop_popFixed_df$GCM_SSP <- sub("_BRT[0-9]+$", "", totalPop_popFixed_df$model)
totalPop_popFixed_df$BRT <- sub(".*_BRT", "", totalPop_popFixed_df$model)

pdf("WNV/Figures/totalPop_GWL_popFixed.pdf", width = 5, height = 5)
p_popFixed <- ggplot(totalPop_popFixed_df, aes(x = .id,y = totalPop,fill=.id)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, direction = -1) +
  geom_jitter(color="black", size=0.4, alpha=0.5) +
  labs(y = "Total population count", x = "Warming level (+°C)") +
  theme_bw() +
  ggtitle("Fixed Total pop counts to +1.3°C")+
  theme(legend.position = "none")
p_popFixed
dev.off()

totalPopFixed_summary <- totalPop_popFixed_df %>% dplyr::group_by(.id) %>%
  dplyr::summarise(median = median(totalPop) / 1e6,
                   q2.5 = quantile(totalPop, 0.025) / 1e6,
                   q97.5 = quantile(totalPop, 0.975) / 1e6)

write_tsv(totalPopFixed_summary, "WNV/Output/totalPopFixed_median_percentiles.tsv")

#stats
model_log_PopFixed <- lmerTest::lmer(log(totalPop) ~ factor(.id) + (1 | GCM_SSP), 
                                     data = totalPop_popFixed_df)
summary(model_log_PopFixed)

emm_PopFixed <- emmeans(model_log_PopFixed, ~ .id)
pairs(emm_PopFixed)
pairs_emm_PopFixed<- as.data.frame(pairs(emm_PopFixed))

write_tsv(pairs_emm_PopFixed, "WNV/Output/results_EMM_totalPopCount_PopFixed.tsv")

### fixing eco suitability to +1.3C ###

binary_maps_t0 <- binary_maps[["1.3"]]

binary_maps_ecoFixed <- list()
for (threshold in thresholds){
  temp <- binary_maps[[as.character(threshold)]]
  binary_maps_level <- list()
  if(threshold == 1.3){
    nutsM_totalPop <- population_GWL[[as.character(threshold)]]
    
    for (model in names(temp)){
      df <- temp[[model]]
      df <- merge(df, nutsM_totalPop, by="nutsM")
      binary_maps_level[[model]] <- df
    }
    binary_maps_ecoFixed[[as.character(threshold)]] <- binary_maps_level
  }else{
    nutsM_totalPop <- population_GWL[[as.character(threshold)]]
    
    for (model in names(temp)){
      df <- binary_maps_t0[[model]]
      df <- merge(df, nutsM_totalPop, by="nutsM")
      binary_maps_level[[model]] <- df
    }
    binary_maps_ecoFixed[[as.character(threshold)]] <- binary_maps_level
  }
}

totalPop_GWL_ecoFixed <- lapply(binary_maps_ecoFixed, function(thresh){
  sums <- sapply(thresh, function(df){
    sum(df$totalPop[df$suitable == 1], na.rm = TRUE)
  })
  data.frame(model = names(thresh),totalPop = sums)
})

totalPop_ecoFixed_df <- ldply(totalPop_GWL_ecoFixed, rbind)
totalPop_ecoFixed_df$GCM_SSP <- sub("_BRT[0-9]+$", "", totalPop_ecoFixed_df$model)
totalPop_ecoFixed_df$BRT <- sub(".*_BRT", "", totalPop_ecoFixed_df$model)

pdf("WNV/Figures/totalPop_GWL_ecoFixed.pdf", width = 5, height = 5)
p_ecoFixed <- ggplot(totalPop_ecoFixed_df, aes(x = .id,y = totalPop,fill=.id)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, direction = -1) +
  geom_jitter(color="black", size=0.4, alpha=0.5) +
  labs(y = "Total population count", x = "Warming level (+°C)") +
  theme_bw() +
  ggtitle("Eco suitability fixed to +1.3°C")+
  theme(legend.position = "none")
p_ecoFixed
dev.off()

totalPop_ecoFixed_summary <- totalPop_ecoFixed_df %>% dplyr::group_by(.id) %>%
  dplyr::summarise(median = median(totalPop) / 1e6,
                   q2.5 = quantile(totalPop, 0.025) / 1e6,
                   q97.5 = quantile(totalPop, 0.975) / 1e6)
write_tsv(totalPop_ecoFixed_summary, "WNV/Output/totalPop_ecoFixed_median_percentiles.tsv")

#stats
model_log_ecoFixed <- lmerTest::lmer(log(totalPop) ~ factor(.id) + (1 | GCM_SSP), 
                                     data = totalPop_ecoFixed_df)
summary(model_log_ecoFixed)

emm_ecoFixed <- emmeans(model_log_ecoFixed, ~ .id)
pairs_emm_ecoFixed<- as.data.frame(pairs(emm_ecoFixed))
pairs(emm_ecoFixed)

write_tsv(pairs_emm_ecoFixed, "WNV/Output/results_EMM_totalPopCount_EcoFixed.tsv")

#4.PLotting togther

df1 <- totalPop_df %>% mutate(dataset="original")
df2 <- totalPop_ecoFixed_df %>% mutate(dataset="ecoFixed")
df3 <- totalPop_popFixed_df %>%mutate(dataset="popFixed")

all_totalPop <- bind_rows(df1, df2, df3)
all_totalPop$.id <- factor(all_totalPop$.id, levels = c("1.3", "1.9","2.3" ,"2.5", "2.8"))
all_totalPop$dataset <- factor(all_totalPop$dataset, levels = c("original", "ecoFixed", "popFixed"))

pdf("WNV/Figures/TotalPop_fixed_eco_pop.pdf", width = 12, height = 7)
ggplot(all_totalPop, aes(x = dataset, y = totalPop, fill = .id)) +
  geom_boxplot()+
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, direction = -1) +
  labs(y = "Total population count", x= "",fill = "Warming level (+°C)") +
  theme_bw() +
  theme(legend.position = "right")
dev.off()


#5. Testing nb of people in eco suit places only in "common" models

common_models <- Reduce(intersect, lapply(suitability_GWL, names))
totalPop_common <- totalPop_df[which(totalPop_df$GCM_SSP %in% common_models),]

#paired Wilcox test
options(scipen = 0)
wilcox_results <- data.frame(x=as.character(), y=as.character(), p.value = as.numeric(), method= as.character())
for (i in 1:(length(thresholds) - 1)){
  thresh.x <- thresholds[i]
  for (j in (i + 1):length(thresholds)){
    thresh.y <- thresholds[j]
    
    df.x <- totalPop_common[totalPop_common$.id == as.character(thresh.x),]
    df.y <- totalPop_common[totalPop_common$.id == as.character(thresh.y),]
    df.x <- df.x[order(df.x$model), ]
    df.y <- df.y[order(df.y$model), ]
    
    test <- wilcox.test(df.x$totalPop, df.y$totalPop, paired = TRUE)
    wilcox_results<- rbind(wilcox_results, data.frame(x=thresh.x, y=thresh.y, p.value = test$p.value,method= test$method))
    
  }
}
wilcox_results$p_BH <- p.adjust(wilcox_results$p.value, method = "BH")
wilcox_results$p_BH <- format.pval(wilcox_results$p_BH, eps = 0.001)
wilcox_results$p.value <- format.pval(wilcox_results$p_BH, eps = 0.001)
write_tsv(wilcox_results, "WNV/Output/wilcox_results_totalPopcount.tsv")



