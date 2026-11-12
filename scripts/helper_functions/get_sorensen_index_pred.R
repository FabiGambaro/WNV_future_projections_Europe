#Modified function of 'get_sorensen_index' to take data from the predictions directly, not pulling from the brt.
get_sorensen_index_pred <- function(pred, obs, thresholds){ 
  P<- sum(obs == 1)
  A<- sum(obs == 0)
  prev<- P / (P + A)
  x<- (P/A) * ((1-prev)/prev)
  
  sorensen_values <- numeric(length(thresholds))
  names(sorensen_values) <- thresholds
  
  for (j in seq_along(thresholds)){
    threshold <- thresholds[j]
    TP<- sum(obs == 1 & pred >= threshold)
    FN<- sum(obs == 1 & pred < threshold)
    FP<- sum(obs == 0 & pred >= threshold)
    
    sorensen_values[j] <- (2 * TP) / ((2 * TP) + (x * FP) + FN)
  }
  
  return(list(max_sorensen = max(sorensen_values, na.rm = TRUE),
              optimised_threshold = thresholds[which.max(sorensen_values)],
              sorensen_curve = sorensen_values))
}