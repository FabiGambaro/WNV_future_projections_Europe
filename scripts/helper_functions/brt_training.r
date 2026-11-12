brt_training <- function(dataframe, gbm.y, gbm.x, theRange, tree.complexity, learning.rate, bag.fraction, n.trees) {
  theRanges = c(theRange, theRange) * 1000
  offset = NULL
  tree.complexity = tree.complexity
  learning.rate = learning.rate
  bag.fraction = bag.fraction
  site.weights = rep(1, dim(dataframe)[1])
  var.monotone = rep(0, length(gbm.x))
  n.folds = 5
  prev.stratify = TRUE
  family = "bernoulli"
  step.size = 5
  n.trees = n.trees
  max.trees = 10000
  tolerance.method = "auto"
  tolerance = 0.001
  plot.main = FALSE
  plot.folds = FALSE
  verbose = FALSE
  silent = TRUE
  #keep.fold.models = FALSE
  keep.fold.models = TRUE #fold models needed for the CV Boyce index on the full study area
  keep.fold.vector = FALSE
  keep.fold.fit = TRUE #keeps predictions of each observation (during the model training in the 5 bloacks) in brt_model$fold.fit

  spdf <- st_as_sf(dataframe, coords = c("x", "y"), crs = 4326) #4326 is the identifier for the WGS84 system
  myblocks<-cv_spatial(spdf, column="response", k=n.folds, size=theRanges[1], 
                        selection="random", plot=F, progress=F, report=F)
  fold.vector <- myblocks$folds_ids
  
  brt_model<-gbm.step(dataframe, gbm.x, gbm.y, offset, fold.vector, tree.complexity,
                      learning.rate, bag.fraction, site.weights, var.monotone, n.folds,
                      prev.stratify, family, n.trees, step.size, max.trees,
                      tolerance.method, tolerance, plot.main, plot.folds, verbose,
                      silent, keep.fold.models, keep.fold.vector, keep.fold.fit)
  return(brt_model)
}
