# Clear all
rm(list=ls())

# Load libraries
library(raster)
library(rgdal)
library(sp)

#---------------------------------------------------------------------

# Set working directory
setwd('~/Documents/Stanford/CS229/CS229-Project/Data/Chicago/')

# Load data
crimes <- read.csv('Crimes_-_2001_to_present.csv', header=T, stringsAsFactors=F)

crimes <- data.frame(date=crimes$Date, year=crimes$Year, type=crimes$Primary.Type,
                     latitude=crimes$Latitude, longitude=crimes$Longitude)
crimes <- subset(crimes, year==2010)

# Only retain complete cases (remove NAs)
crimes <- crimes[complete.cases(crimes), ]

tract <- shapefile('TractLookup/tl_2010_17031_tract10.shp')
tract <- spTransform(x=tract, CRSobj=CRS('+proj=longlat +datum=WGS84'))
names(tract@data) <- tolower(names(tract@data))

crimes <- SpatialPointsDataFrame(coords=crimes[, c('longitude', 'latitude')],
                             data=crimes[, c('year', 'date', 'type')],
                             proj4string=CRS('+proj=longlat +datum=WGS84'))

# Spatial overlay to identify Census polygon in which each coordinate falls
crimes_tract <- over(x=crimes, y=tract)
crimes_tract <- data.frame(tract=crimes_tract$tractce10, tract_name=crimes_tract$namelsad10,
                           latitude=crimes_tract$intptlat10, longitude=crimes_tract$intptlon10)

# Add Census tract data to crime dataset
result <- data.frame(crimes@data, crimes_tract)
write.csv(result, 'Chicago_clean.csv')
