###############################################
###      Hyperparamter fine-tunning         ###
###############################################
s
require(dismo)
require(gbm)
require(gmodels)
require(blockCV)
require(sf)
require(readr)

options(scipen=999)

tree.complexity.grid <- c(1, 3, 5)
learning.rate.grid<- c(0.0005, 0.001, 0.005) 
bag.fraction.grid<- c(0.75, 0.8, 0.85)

param_grid <- expand.grid(tree.complexity=tree.complexity.grid,learning.rate=learning.rate.grid,
                          bag.fraction=bag.fraction.grid)

source("WNV/helper_functions/brt_training.r")
source("WNV/helper_functions/get_sorensen_index.r")
nutsM_sf <-readRDS("WNV/Processed_data/nutsM_sf.rds")
nutsM_centroids<-st_coordinates(st_centroid(nutsM_sf))

s="STG2"

shadow_results <- read_tsv("WNV/Output/2026-10-28_All_RI_shadow_values_STG2.tsv")
retained <- shadow_results$variable[shadow_results$prop_RIs_RIa < 0.05]

envVariables <- c("temp_winter","temp_spring","temp_summer" ,"temp_fall" ,"temp_var","pr_spring","pr_summer",
                  "rh_winter","rh_summer","rh_fall","pastures","rangelands","urbanAreas","primaryForest",
                  "secondaryNonForest", "populationDensity") #shadow_testing_STGs

env_all_years <- readRDS("WNV/Output/2026-10-22_env_all_years.rds")

strategies <- c("STG2")
n.iterations <-100
gbm.y <- "response"
gbm.x <- envVariables
theRange <- 1345 #from BRT_analysis_env_all
n.trees<-100

all_results <- list(); BRTs <- list()
for (s in strategies){
  cat("Shadow testing with", s, "\n")
  dataframes_list <- readRDS(paste0("WNV/Output/2026-10-22_data_env_train_", s, ".rds"))
  
  dataframes <- lapply(dataframes_list, function(df){
    index <- match(df$nutsM_code, nutsM_sf$LocationCo)
    df$x <- nutsM_centroids[index, 1]
    df$y <- nutsM_centroids[index, 2]
    return(df)
  })
  
  results <- param_grid
  results$AUC <- rep(NA, nrow(results))
  results$SI <- rep(NA, nrow(results))
  results$opt_thresholds <- rep(NA, nrow(results))
  
  all_BRTs <-list()
  for (i in 1:nrow(param_grid)){
    cat("Testing combination", i, "of", nrow(param_grid), "\n")
    tree.complexity=param_grid$tree.complexity[i]
    learning.rate=param_grid$learning.rate[i]
    bag.fraction=param_grid$bag.fraction[i]
    
    brt_model_list <- lapply(dataframes, function(df) {
      suppressMessages(brt_training(df, gbm.y, gbm.x, theRange, tree.complexity, learning.rate, bag.fraction, n.trees))
    })
    
    all_BRTs[[i]] <- brt_model_list
    
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
      q <- quantile(x, c(0.25, 0.5, 0.75), na.rm = TRUE)
      results[s, metric] <- paste0(round(q[2], 3), " (", round(q[1], 3), "-", round(q[3], 3), ")")
    }
    
    write.csv(results, paste0("WNV/Output/2026-10-27_BRT_parameter_tuning_", s, ".csv"), row.names = FALSE)
    saveRDS(all_BRTs, paste0("WNV/Output/2026-06-15_BRTs_tuning_",s,".rds"))
    
  }
}

