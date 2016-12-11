# Clear all
rm(list=ls())

# Load libraries
library(gpclib)
library(geosphere)
library(ggplot2)
library(maptools)
library(plyr)
library(raster)
library(rgdal)
library(sp)
library(stringr)

#---------------------------------------------------------------------

# Load data
crimes <- read.csv('~/Documents/Stanford/CS229/CS229-Project/CleanData/NewYork_geoid.csv',header=T,stringsAsFactors=F)
lookup <- read.csv('~/Documents/Stanford/CS229/CS229-Project/Merge/Id_population_lookup.csv',header=T,stringsAsFactors=F)

crimes <- merge(crimes, lookup, by='geoid10', all.x=TRUE)
crimes$logCrimeRate <- log(crimes$Frequency / crimes$Pop)

map.df <- read.csv('~/Downloads/file-for-ariel.csv',header=T, stringsAsFactors=F)
map.df$num <- 1:nrow(map.df)

GEOID_list <- intersect(crimes$geoid10, map.df$GEOID)
map.df <- map.df[map.df$GEOID %in% GEOID_list,]

map <- merge(crimes, map.df, by.x='geoid10', by.y='GEOID', all.y=T)
map <- map[order(map$num),]

setwd('~/Documents/Stanford/CS229/CS229-Project/Visualizations/')

# Save New York Map
jpeg('New_York_Crime_Rate.jpeg')
ggplot() +
  geom_polygon(data=map, aes(long, lat, group=group, fill=logCrimeRate), color="grey35", alpha=1) +
  coord_equal() +
  scale_fill_gradient(low = "navy", high = "white") +
  labs(x='longitude', y='latitude')
dev.off()