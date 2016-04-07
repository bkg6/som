rm(list = ls())
library(dplyr)
library(kohonen)
library(ggplot2)
library(RColorBrewer)
library(colorRamps)

setwd("D:/FRACTAL/Genomics/Microsoft")
select <- dplyr::select
data <- read.csv("BBB_00805_US_EPG_Model_Data_20150716.csv")


data_sub <- data %>% select(TPID,
                            Industry,
                            Rev_Bucket,
                            Segment,
                            subsegment_MSS,
                            avg_daysToClose,
                            max_daysToClose,
                            win_loss_ratio,
                            Num_Of_Children,
                            Num_ProductFamily,
                            Percent_Rev_Annuity,
                            Percent_Rev_NonAnnuity,
                            Rev_Annuity,
                            Rev_NonAnnuity,
                            Rev_Office_365,
                            Rev_Per_agreement,
                            Rev_Project,
                            Rev_Security)


data_sub[, c("avg_daysToClose","max_daysToClose", "win_loss_ratio", "Percent_Rev_Annuity",
             "Percent_Rev_NonAnnuity")] <- lapply(data_sub[, c("avg_daysToClose","max_daysToClose", "win_loss_ratio", 
                                                               "Percent_Rev_Annuity", "Percent_Rev_NonAnnuity")], as.character)
data_sub[, c("avg_daysToClose","max_daysToClose", "win_loss_ratio", "Percent_Rev_Annuity",
             "Percent_Rev_NonAnnuity")] <- lapply(data_sub[, c("avg_daysToClose","max_daysToClose", "win_loss_ratio", 
                                                               "Percent_Rev_Annuity", "Percent_Rev_NonAnnuity")], as.numeric)
data_sub <- na.omit(data_sub)
unique_data <- sapply(data_sub, function(x){return(length(unique(x)))})
z <- sapply(data_sub, is.factor)
zz <- names(data_sub[z])
data_som <- data_sub
data_som[z] <- lapply(data_som[z], as.numeric)
data_som <- select(data_som, -TPID)
data_sub <- select(data_sub, -TPID)


capVector <- function(x, probs = c(0.08,0.92)){
  #remove things above and below the percentiles specified
  ranges <- quantile(x, probs=probs, na.rm=T)
  x[x < ranges[1]] <- ranges[1]
  x[x > ranges[2]] <- ranges[2]
  return(x)
}

data_som <- sapply(data_som, capVector)
data_som_scaled <- scale(data_som)
som_model <- som(data_som_scaled, grid = somgrid(15, 15, "hexagonal"), rlen = 500)



type = c("changes",  "codes","counts", "dist.neighbours")
for (i in 1: length(type)){
  mypath <- paste("D:/FRACTAL/Genomics/Microsoft/som/plots/", type[i], ".png", sep = "")
  png(file=mypath, width=2000,height=1200) 
  plot(som_model, type = type[i], main = "", palette.name= colorRampPalette(brewer.pal(7, "Greens")), cex = 2, font.main= 4)
  title(type[i], cex.main = 3)
  dev.off()
}


for(i in 1:ncol(data.frame(data_som))){
  mypath <- paste("D:/FRACTAL/Genomics/Microsoft/som/plots/", i,".", colnames(som_model$data)[i], ".png", sep = "")
  png(file=mypath, width=2000,height=1200)
  unscaled <- aggregate(as.numeric(data_sub[,i]), by=list(som_model$unit.classif), FUN=mean, simplify=TRUE)[,2]
  if(colnames(som_model$data)[i] %in% zz){
    if(length(unique(data_som[,i])) > 12){
      # plot(som_model, type = "property", property = unscaled, main = "", palette.name= colorRampPalette(brewer.pal(length(unique(data_sub[,i])),"Paired")), heatkey = F,  ncolors = length(unique(data_sub[,i])), cex = 2, font.main= 4)
      plot(som_model, type = "property", property = unscaled, main = "", palette.name= colorRampPalette(primary.colors(length(unique(data_som[,i])))), heatkey = F,  ncolors = length(unique(data_sub[,i])), cex = 2, font.main= 4)
      legend("left", legend = unique(data_sub[,i]), fill = brewer.pal(length(unique(data_sub[,i])),"Paired"), cex = 2,  bty = "n")
      title(colnames(som_model$data)[i], cex.main = 3)
    }
    else{
    plot(som_model, type = "property", property = unscaled, main = "", palette.name= colorRampPalette(brewer.pal(length(unique(data_sub[,i])),"Paired")), heatkey = F,  ncolors = length(unique(data_sub[,i])), cex = 2, font.main= 4)
    legend("left", legend = unique(data_sub[,i]), fill = brewer.pal(length(unique(data_sub[,i])),"Paired"), cex = 2,  bty = "n")
    title(colnames(som_model$data)[i], cex.main = 3)
    }
  }
  else{
    plot(som_model, type = "property", property = unscaled, main="", palette.name=colorRampPalette(brewer.pal(6,"Blues")), heatkey = T, heatkeywidth = 0.6,  cex = 2, font.main= 4)
    title(colnames(som_model$data)[i], cex.main = 3)
  }
  dev.off()
}


purples_palette <- brewer.pal(9, "Purples")
pretty_palette <- c("#1f77b4", '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2')
mypath <- paste("D:/FRACTAL/Genomics/Microsoft/som/plots/", "clusters", ".png", sep = "")
png(file=mypath, width=2000,height=1200)
som_cluster <- cutree(hclust(dist(som_model$codes)), 6)
plot(som_model, type="mapping", bgcol = pretty_palette[som_cluster], main = "Clusters", cex = 3, font.main= 4)
add.cluster.boundaries(som_model, som_cluster)
dev.off()



