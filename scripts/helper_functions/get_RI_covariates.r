get_RI_covariates <- function(brt_models,save_path) {
  
  envVars <- brt_models[[1]]$var.names
  nb_vars <- length(envVars)
  nb_runs <- length(brt_models)
  
  RIs <- matrix(0, nrow = nb_vars, ncol = nb_runs)
  rownames(RIs) <- envVars; colnames(RIs) <- paste0("run_", seq_len(nb_runs))
  
 # for (j in seq_along(brt_models)) {
 #   s <- summary(brt_models[[j]])
 #   for (k in seq_along(envVars)) {
 #     RIs[k, j] <- s[envVars[k], "rel.inf"]
 #   }
 # }
  for (j in 1:length(brt_models)) {
    s <- brt_models[[j]]$contributions 
    for (k in seq_along(envVars)) {
      index <- which(s$var == envVars[k])
      if (length(index) == 1) {
        RIs[k, j] <- s$rel.inf[index]
      } else {
        RIs[k, j] <- NA  # in case variable not found
      }
    }
  }
  
 # RI_summary <- t(apply(RIs, 1, function(x) {
 #   x <- na.omit(x); t_res <- t.test(x)
 #   c(estimate=t_res$estimate, CI_lower = t_res$conf.int[1], CI_upper = t_res$conf.int[2])
 # }))
  
  RI_summary <- t(apply(RIs, 1, function(x) {
    x <- na.omit(x)
    c(median = median(x), q025 = quantile(x, 0.025, names = FALSE), q975 = quantile(x, 0.975, names = FALSE))
  }))
  
  
  relativeInfluences <- as.data.frame(RI_summary)
  colnames(relativeInfluences) <- c("Estimate", "CI_lower", "CI_upper")
  
  write.csv(relativeInfluences, file = file.path(save_path, "Relative_influences.csv"), row.names = TRUE)
  write.csv(RIs, file = file.path(save_path, "All_relative_influences.csv"), row.names = TRUE)
  
  #return as a list
  return(list(summary = relativeInfluences,all_values = RIs))
}