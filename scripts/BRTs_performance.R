##############################################################
##      Estimating final metric performances                ##
##############################################################
setwd("~/Dropbox/Postdoc_SpELL/GRP_Future_projections_Arbo")
require(readr)
require(ecospat)    

source("WNV/helper_functions/get_sorensen_index.r")

BRTs <- readRDS("WNV/Output/2026-10-30_BRTs_A_B.rds")
envVariables <- c("temp_winter","temp_spring","temp_summer" ,"temp_fall" ,"temp_var","pr_spring","pr_summer",
                  "rh_winter","rh_summer","rh_fall","pastures","rangelands","urbanAreas","primaryForest",
                  "secondaryNonForest", "populationDensity") #shadow_testing

n.iterations <-100
gbm.y <- "response"
gbm.x <- envVariables
theRange <- 1345 #from BRT_analysis_script
n.trees<-100

env_all_years <- readRDS("WNV/Output/2026-10-22_env_all_years.rds")

for (model in names(BRTs)){
  brt_model_list <- BRTs[[model]]
  results <- data.frame(model = model)
  metrics <- data.frame(Boyce = rep(NA, n.iterations), AUC = rep(NA, n.iterations), SI = rep(NA, n.iterations), opt_thresholds = rep(NA, n.iterations))
  
  for (j in 1:n.iterations){
    brt_model <- brt_model_list[[j]]
    
    metrics$AUC[j] <- brt_model$cv.statistics$discrimination.mean
    sorensen_results <- get_sorensen_index(brt_model, thresholds = seq(0, 1, 0.01))
    metrics$SI[j] <-sorensen_results$max_sorensen
    metrics$opt_thresholds[j] <- sorensen_results$optimised_threshold
    
    pred_all <- rowMeans(sapply(brt_model$fold.models, function(m) #background predicted by the fold models (needs keep.fold.models = TRUE)
      predict.gbm(m, env_all_years[, gbm.x], n.trees = brt_model$gbm.call$best.trees, type = "response")))
    resp <- brt_model$gbm.call$dataframe$response
    
    metrics$Boyce[j] <- ecospat.boyce(fit = pred_all, obs = plogis(brt_model$fold.fit)[resp == 1],
                                      nclass = 0, window.w = "default", PEplot = FALSE)$cor
    
  }
  
  for (metric in colnames(metrics)){
    x <- metrics[[metric]]
    median <- median(x)
    q <- quantile(x, c(0.025, 0.975), na.rm = TRUE)
    results[1, metric]<- paste0(round(median, 3), " (", round(q[1], 3), "-", round(q[2], 3), ")")
  }
  
  write.csv(metrics, paste0("WNV/Output/2026-10-30_BRT_performance_", model, ".csv"), row.names = FALSE)
  write.csv(results, paste0("WNV/Output/2026-10-30_BRT_performance_quantiles_", model, ".csv"), row.names = FALSE)
}
