# Clear all
rm(list=ls())

# Load libraries
library(plyr)
library(raster)
library(rgdal)
library(sp)

#---------------------------------------------------------------------

# Set working directory
setwd('~/Documents/Stanford/CS229/CS229-Project/RawData/Philadelphia/')

# Load data
crimes <- read.csv('PPD_Crime_Incidents_2006-Present.csv', header=T, stringsAsFactors=F)

# Only retain complete cases (remove NAs)
crimes <- crimes[complete.cases(crimes), ]

# Convert coordinate string to Latitude/Longitude
coordinates <- read.table(text=crimes$Shape, sep=' ', colClasses='character')
crimes$Longitude <- as.numeric(substr(coordinates$V2, 2, nchar(coordinates$V2)))
crimes$Latitude <- as.numeric(substr(coordinates$V3, 1, nchar(coordinates$V3)-1))

crimes$Year <- as.numeric(substr(crimes$Dispatch.Date, 1, 4))
crimes <- data.frame(date=crimes$Dispatch.Date.Time, year=crimes$Year,type=crimes$General.Crime.Category,
                     latitude=crimes$Latitude, longitude=crimes$Longitude)
crimes <- subset(crimes, year==2010)

tract <- shapefile('TractLookup/tl_2010_42101_tract10.shp')
tract <- spTransform(x=tract, CRSobj=CRS('+proj=longlat +datum=WGS84'))
names(tract@data) <- tolower(names(tract@data))

crimes <- SpatialPointsDataFrame(coords=crimes[, c('longitude', 'latitude')],
                                 data=crimes[, c('year', 'date', 'type')],
                                 proj4string=CRS('+proj=longlat +datum=WGS84'))

# Spatial overlay to identify Census polygon in which each coordinate falls
crimes_tract <- over(x=crimes, y=tract)
crimes_tract <- data.frame(tract_name=crimes_tract$namelsad10)

# Add Census tract data to crime dataset
result <- data.frame(crimes@data, crimes_tract)
result <- count(result, c('tract_name'))
result$city <- rep('Philadelphia', nrow(result))

write.csv(result, '~/Documents/Stanford/CS229/CS229-Project/CleanData/Philadelphia_clean.csv')
