#################################################
###  Estimating and plotting response curves  ###
#################################################

brt_model_scvs = readRDS("WNV/Output/2026-10-30_BRTs_A_B.rds")[["model_B"]] 
source("WNV/helper_functions/get_RI_covariates.r") #get and stores RIs
RI_results <- get_RI_covariates(brt_models = brt_model_scvs, save_path = "WNV/Output/")

#plotting response curves

envVariableNames<-brt_model_scvs[[1]]$var.names
data_curv<-brt_model_scvs[[1]]$gbm.call$dataframe
data_curv<-data_curv[data_curv[, "response"] == 1, ]
envVariableValues = matrix(nrow=3, ncol=length(envVariableNames))
row.names(envVariableValues) = c("median","minV","maxV")
colnames(envVariableValues) = envVariableNames

for (j in 1:length(envVariableNames)){
  minV = min(data_curv[,envVariableNames[j]], na.rm=T)
  maxV = max(data_curv[,envVariableNames[j]], na.rm=T)
  medianV = median(data_curv[,envVariableNames[j]], na.rm=T)
  envVariableValues[,j] = rbind(medianV, minV, maxV)
}

envVariableValues_list = envVariableValues

#covariates in descending order of mean relative influence
var_order <- order(RI_results$summary$Estimate, decreasing = TRUE)
RI_results$summary[var_order, ] #check

# plot with Y axis scaled from 0 to 1
pdf("WNV/Figures/response_curves_percentiles.pdf", width = 4.75, height = 5.125) 
par(mfrow=c(4,4), oma=c(0.5,0.5,0,0.5), mar=c(2.3,2,0.3,0), lwd=0.2, col="gray30")

panel_id <- 0 
for (i in var_order){
  panel_id <- panel_id + 1 
  predictions_list = list(); dfs = list()
  valuesInterval = 0.1; valuesInterval = (envVariableValues_list["maxV",i]-envVariableValues_list["minV",i])/100
  df = data.frame(matrix(nrow=length(seq(envVariableValues_list["minV",i],envVariableValues_list["maxV",i],valuesInterval)),ncol=length(envVariableNames)))
  colnames(df) = envVariableNames
  
  for (k in 1:length(envVariableNames)){
    valuesInterval = 0.1; valuesInterval = (envVariableValues_list["maxV",k]-envVariableValues_list["minV",k])/100
    if (i == k) df[,envVariableNames[k]] = seq(envVariableValues_list["minV",k],envVariableValues_list["maxV",k],valuesInterval)
    if (i != k) df[,envVariableNames[k]] = rep(envVariableValues_list["median",k],dim(df)[1])
  }
  dfs = df 
  predictions = list()
  
  for (j in 1:length(brt_model_scvs)){
    n.trees = brt_model_scvs[[j]]$gbm.call$best.trees; type = "response"; single.tree = FALSE
    prediction = predict.gbm(brt_model_scvs[[j]], newdata=df, n.trees, type, single.tree)
    if ((j == 1)){
      minX = min(df[,envVariableNames[i]]) 
      maxX = max(df[,envVariableNames[i]])
      minY = min(prediction); maxY = max(prediction)
    }	else	{
      if (minX > min(df[,envVariableNames[i]])) minX = min(df[,envVariableNames[i]])
      if (maxX < max(df[,envVariableNames[i]])) maxX = max(df[,envVariableNames[i]])
      if (minY > min(prediction)) minY = min(prediction)
      if (maxY < max(prediction)) maxY = max(prediction)
    }
    
    predictions[[j]] = prediction
  }
  predictions_list = predictions
  cols = "firebrick2"
  
  for (l in 1:length(brt_model_scvs)){
    if ((l == 1)){
      plot(dfs[,envVariableNames[i]],predictions_list[[l]],col=cols,ann=F,axes=F,lwd=0.2,type="l",xlim=c(minX,maxX),ylim=c(0,1))
    }	else	{
      lines(dfs[,envVariableNames[i]],predictions_list[[l]],col=cols,lwd=0.2)
    }
    
  }
  
  env_label <- row.names(RI_results$summary)[i]
  ri_label <- paste0("RI = ",round(RI_results$summary[i,1],2), "% [",round(RI_results$summary[i,2],2), "-", 
                     round(RI_results$summary[i,3],2), "]")
  
  axis(side=1, lwd.tick=0.2, cex.axis=0.6, lwd=0, tck=-0.030, col.axis="gray30", mgp=c(0,0.07,0))
  axis(side=2, lwd.tick=0.2, cex.axis=0.6, lwd=0, tck=-0.030, col.axis="gray30", mgp=c(0,0.2,0))
  
  n_col = 4
  if ((panel_id-1)%% n_col==0){title(ylab="Predicted value", cex.lab=0.65, mgp=c(1.3,0,0), col.lab="gray30")}
  title(xlab = env_label, cex.lab=0.65,mgp=c(0.7,0,0), col.lab="gray30")
  title(xlab = ri_label, cex.lab=0.65, mgp=c(1.3,0,0), col.lab="gray30")
  box(lwd=0.2, col="gray30")
}

dev.off()

RI_plot <- RI_results$summary
RI_plot$variable <- factor(rownames(RI_plot), levels = rownames(RI_plot)[order(RI_plot$Estimate)])
ggplot(RI_plot, aes(x = Estimate, y = variable)) +
  geom_col(fill = "firebrick2") +
  geom_errorbar(aes(xmin = CI_lower, xmax = CI_upper), width = 0.3) +
  labs(x = "Relative influence (%)", y = NULL) +
  theme_bw()
