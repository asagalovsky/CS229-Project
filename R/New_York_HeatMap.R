# Clear all
rm(list=ls())

# Load libraries
library(ggplot2)
library(gplots)
library(RColorBrewer)

setwd('~/Documents/Stanford/CS229/CS229-Project/CleanData/')
grid <- read.csv('heatmap_df.csv', header=T, stringsAsFactors=F)

colnames(grid) <- c('p','100','250','500','750','1000')
rownames(grid) <- grid[,1]

grid <- grid[,-1]

grid <- round(sqrt(grid), 3)
grid <- as.matrix(grid)
melted_grid <- melt(grid)

melted_grid$Var1 <- with(melted_grid,factor(Var1,levels = sort(unique(Var1))))
melted_grid$Var2 <- with(melted_grid,factor(Var2,levels = sort(unique(Var2))))

names(melted_grid) <- c('Vars_Considered','nTrees','RMSE')


setwd('~/Documents/Stanford/CS229/CS229-Project/Visualizations/')

jpeg('Gridsearch_heatmap.jpeg')
ggplot(data = melted_grid, aes(nTrees, Vars_Considered, fill = RMSE))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "navy", high = "pink", mid = "white", 
                       midpoint = 0.5, limit = c(0.43,0.57), space = "Lab", 
                       name="RMSE") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 10, hjust = 1))+
  coord_fixed()
dev.off()