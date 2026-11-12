#spatially cross-validated sorensen index
get_sorensen_index <- function(brt_model, thresholds){
  responses  <- brt_model$gbm.call$dataframe$response
  
  if (is.null(brt_model$fold.fit)) stop("fold.fit missing: train with keep.fold.fit = TRUE")
  if (length(brt_model$fold.fit) != length(responses)) stop("fold.fit and responses differ in length")

  prediction <- plogis(brt_model$fold.fit) #predictions are stored in logit scale, so here we bring it back to 0 - 1 predictions. 
  
  P<- sum(responses == 1)
  A<- sum(responses == 0)
  #prev <- P / (P + A) # proportion of recorded sites where the species is present
  #x <- (P/A) * ((1-prev)/prev)
  x=1
  sorensen_values <- numeric(length(thresholds))
  names(sorensen_values) <- thresholds
  
  for (j in seq_along(thresholds)){
    threshold <- thresholds[j]
    TP<-sum(responses == 1 & prediction >= threshold)
    FN<-sum(responses == 1 & prediction < threshold)
    FP<-sum(responses == 0 & prediction >= threshold)
    
    sorensen_values[j] <- (2 * TP) / ((2 * TP) + (x * FP) + FN)
  }
  
  optimised_threshold <- thresholds[which.max(sorensen_values)]
  max_sorensen <- max(sorensen_values, na.rm = TRUE)
  
  return(list(max_sorensen = max_sorensen,
              optimised_threshold = optimised_threshold,
              sorensen_curve = sorensen_values))
}
