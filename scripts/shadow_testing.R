###############################################
###     ENV VARIABLES SHADOW TESTING       ###
###############################################

require(terra)
require(sf)
require(sp)
require(blockCV)
require(dismo)
require(gbm)
require(readr)
require(future.apply)

source("WNV/helper_functions/brt_training.r")

envVariables <- c("temp_winter","temp_spring","temp_summer","temp_fall", "temp_var","pr_winter" ,"pr_spring","pr_summer","pr_fall",
                  "rh_winter","rh_spring","rh_summer","rh_fall","croplands", "pastures", "rangelands", "urbanAreas","primaryForest",
                  "primaryNonForest", "secondaryForest", "secondaryNonForest","populationDensity")

n.iterations <-100
nutsM_sf <-readRDS("WNV/Processed_data/nutsM_sf.rds")
nutsM <-readRDS("WNV/Processed_data/nutsM_EU.rds")
nutsM_centroids<-st_coordinates(st_centroid(nutsM_sf))


strategies <- c("STG2")
ranges_STGs <- list("STG2" = 1345) #from BRT_analysis_all_env

gbm.y <- "response"
gbm.x <- envVariables
tree.complexity <- 5
learning.rate <- 0.001
bag.fraction <- 0.85
n.trees <- 100
n.rep <- 10

plan(multisession, workers = parallel::detectCores() - 1)

shadow_results <- list()
for (s in strategies){
  cat("Shadow testing with", s, "\n")
  dataframes_list <- readRDS(paste0("WNV/Output/2026-10-22_data_env_train_", s, ".rds"))
  
  dataframes <- lapply(dataframes_list, function(df){
    index <- match(df$nutsM_code, nutsM_sf$LocationCo)
    df$x <- nutsM_centroids[index, 1]
    df$y <- nutsM_centroids[index, 2]
    return(df)
  })
  
  theRange <- ranges_STGs[[s]]
  
  tab <- matrix(0, nrow = length(envVariables), ncol = 1)
  row.names(tab) <- envVariables
  colnames(tab) <- "prop_RIs_RIa" #prop (RI shadow >= RI actual)
  
  set.seed(123)
  n_fitted <- 0
  RI_all <- list()
  for (h in 1:n.rep){
    #for (i in 1:length(dataframes)){
    RI_rep <- future_lapply(1:length(dataframes), function(i){ #the 100 datasets run in parallel
      shadows <- dataframes[[i]][, gbm.x]
      colnames(shadows) <- paste0(colnames(shadows), "_shadow")
      
      for (k in 1:dim(shadows)[2]){
        shadows[, k] <- shadows[sample(1:nrow(shadows), nrow(shadows), replace = FALSE), k] #shuffling all shadows independently
      }
      
      dataframe_shadow <- cbind(dataframes[[i]], shadows)
      gbm.x_shadow <- c(gbm.x, colnames(shadows))
      
      #fit BRT with original + shadow env variables.
      brt_model <- brt_training(dataframe_shadow, gbm.y, gbm.x_shadow, theRange, tree.complexity, 
                                learning.rate, bag.fraction, n.trees)
      if (is.null(brt_model)) return(NULL) 
      ri_summary <- summary(brt_model, plotit = FALSE)
      relativeImportances <- matrix(nrow = length(gbm.x_shadow), ncol = 1)
      row.names(relativeImportances) <- gbm.x_shadow
      
      for (j in 1:length(gbm.x_shadow)) {
        relativeImportances[j, 1] <- ri_summary[gbm.x_shadow[j], "rel.inf"]
      }
      return(relativeImportances)
    }, future.seed = TRUE, future.packages = c("dismo", "gbm", "blockCV", "sf"))
    
    for (relativeImportances in RI_rep) {
      if (is.null(relativeImportances)) next
      n_fitted <- n_fitted + 1
      RI_all[[length(RI_all) + 1]] <- relativeImportances[, 1] # store RIs
      for (j in 1:length(gbm.x)) {
        if (relativeImportances[gbm.x[j], 1] < relativeImportances[paste0(gbm.x[j], "_shadow"), 1]){
          tab[j, ] <- tab[j, ] + 1
        }
      }
    }
  }
  
  cat(s, ":", n_fitted, "of", length(dataframes) * n.rep, "BRTs converged\n")
  RI_all <- do.call(rbind, RI_all) # n_fitted x 44 (real + shadow RIs) should be 1000*44
  saveRDS(RI_all, paste0("WNV/Output/2026-10-28_RI_shadow_all_", s, ".rds"))
  
  tab <- data.frame(variable = row.names(tab), prop_RIs_RIa = round(tab[, 1] / n_fitted, 3))
  shadow_results[[s]] <- tab
  
  write_tsv(tab, paste0("WNV/Output/2026-10-28_All_RI_shadow_values_",s,".tsv"))
  print(tab$variable[tab$prop_RIs_RIa < 0.05])
  print(round(colMeans(RI_all[, gbm.x] == 0 & RI_all[, paste0(gbm.x, "_shadow")] == 0), 3)) #print those var that are both 0, meaning not used in the model
  
}
plan(sequential)


