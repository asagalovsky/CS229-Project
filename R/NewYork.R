# Clear all
rm(list=ls())

# Load libraries
library(raster)
library(rgdal)
library(sp)

#---------------------------------------------------------------------

# Function definitions
substrRight <- function(x, n) {
  substr(x, nchar(x)-n+1, nchar(x))
}

#---------------------------------------------------------------------

# Set working directory
setwd('~/Documents/Stanford/CS229/CS229-Project/Data/NewYork/')

# Load data
crimes <- read.csv('NYPD_Complaint_Data_Historic_2010.csv', header=T, stringsAsFactors=F)
crimes$Year <- substrRight(crimes$CMPLNT_FR_DT, 4)

# Break up crimes by Borough (because of different shape files for each)
crimes_BRNX <- crimes[which(crimes$BORO_NM == 'BRONX'),]
crimes_BKLN <- crimes[which(crimes$BORO_NM == 'BROOKLYN'),]
crimes_MHTN <- crimes[which(crimes$BORO_NM == 'MANHATTAN'),]
crimes_QNS  <- crimes[which(crimes$BORO_NM == 'QUEENS'),]
crimes_STNI <- crimes[which(crimes$BORO_NM == 'STATEN ISLAND'),]

# Load and transform shape files
tract_BRNX <- shapefile('tractLookup/tl_2010_36005_tract10.shp')             # Bronx: 36005
tract_BKLN <- shapefile('tractLookup/tl_2010_36047_tract10.shp')             # Brooklyn: 36047
tract_MHTN <- shapefile('tractLookup/tl_2010_36061_tract10.shp')             # Manhattan: 36061
tract_QNS  <- shapefile('tractLookup/tl_2010_36081_tract10.shp')             # Queens: 36081
tract_STNI <- shapefile('tractLookup/tl_2010_36085_tract10.shp')             # Staten Island: 36085

tract_BRNX <- spTransform(x=tract_BRNX, CRSobj=CRS('+proj=longlat +datum=WGS84'))
tract_BKLN <- spTransform(x=tract_BKLN, CRSobj=CRS('+proj=longlat +datum=WGS84'))
tract_MHTN <- spTransform(x=tract_MHTN, CRSobj=CRS('+proj=longlat +datum=WGS84'))
tract_QNS  <- spTransform(x=tract_QNS,  CRSobj=CRS('+proj=longlat +datum=WGS84'))
tract_STNI <- spTransform(x=tract_STNI, CRSobj=CRS('+proj=longlat +datum=WGS84'))

names(tract_BRNX@data) <- tolower(names(tract_BRNX@data))
names(tract_BKLN@data) <- tolower(names(tract_BKLN@data))
names(tract_MHTN@data) <- tolower(names(tract_MHTN@data))
names(tract_QNS@data)  <- tolower(names(tract_QNS@data))
names(tract_STNI@data) <- tolower(names(tract_STNI@data))

# Create smaller dataframes with relevant columns only
crimes_BRNX <- data.frame(date=crimes_BRNX$CMPLNT_FR_DT, year=crimes_BRNX$Year, type=crimes_BRNX$OFNS_DESC,
                          latitude=crimes_BRNX$Latitude, longitude=crimes_BRNX$Longitude)
crimes_BKLN <- data.frame(date=crimes_BKLN$CMPLNT_FR_DT, year=crimes_BKLN$Year, type=crimes_BKLN$OFNS_DESC,
                          latitude=crimes_BKLN$Latitude, longitude=crimes_BKLN$Longitude)
crimes_MHTN <- data.frame(date=crimes_MHTN$CMPLNT_FR_DT, year=crimes_MHTN$Year, type=crimes_MHTN$OFNS_DESC,
                          latitude=crimes_MHTN$Latitude, longitude=crimes_MHTN$Longitude)
crimes_QNS  <- data.frame(date=crimes_QNS$CMPLNT_FR_DT,  year=crimes_QNS$Year,  type=crimes_QNS$OFNS_DESC,
                          latitude=crimes_QNS$Latitude,  longitude=crimes_QNS$Longitude)
crimes_STNI <- data.frame(date=crimes_STNI$CMPLNT_FR_DT, year=crimes_STNI$Year, type=crimes_STNI$OFNS_DESC,
                          latitude=crimes_STNI$Latitude, longitude=crimes_STNI$Longitude)

# Only retain complete cases (remove NAs)
crimes_BRNX <- crimes_BRNX[complete.cases(crimes_BRNX), ]
crimes_BKLN <- crimes_BKLN[complete.cases(crimes_BKLN), ]
crimes_MHTN <- crimes_MHTN[complete.cases(crimes_MHTN), ]
crimes_QNS  <- crimes_QNS[complete.cases(crimes_QNS), ]
crimes_STNI <- crimes_STNI[complete.cases(crimes_STNI), ]

crimes_BRNX <- SpatialPointsDataFrame(coords=crimes_BRNX[, c('longitude', 'latitude')],
                                     data=crimes_BRNX[, c('year', 'date', 'type')],
                                     proj4string=CRS('+proj=longlat +datum=WGS84'))
crimes_BKLN <- SpatialPointsDataFrame(coords=crimes_BKLN[, c('longitude', 'latitude')],
                                      data=crimes_BKLN[, c('year', 'date', 'type')],
                                      proj4string=CRS('+proj=longlat +datum=WGS84'))
crimes_MHTN <- SpatialPointsDataFrame(coords=crimes_MHTN[, c('longitude', 'latitude')],
                                      data=crimes_MHTN[, c('year', 'date', 'type')],
                                      proj4string=CRS('+proj=longlat +datum=WGS84'))
crimes_QNS  <- SpatialPointsDataFrame(coords=crimes_QNS[, c('longitude', 'latitude')],
                                      data=crimes_QNS[, c('year', 'date', 'type')],
                                      proj4string=CRS('+proj=longlat +datum=WGS84'))
crimes_STNI <- SpatialPointsDataFrame(coords=crimes_STNI[, c('longitude', 'latitude')],
                                      data=crimes_STNI[, c('year', 'date', 'type')],
                                      proj4string=CRS('+proj=longlat +datum=WGS84'))

# Spatial overlay to identify Census polygon in which each coordinate falls
crimes_tract_BRNX <- over(x=crimes_BRNX, y=tract_BRNX)
crimes_tract_BRNX <- data.frame(tract=crimes_tract_BRNX$tractce10, tract_name=crimes_tract_BRNX$namelsad10,
                           latitude=crimes_tract_BRNX$intptlat10, longitude=crimes_tract_BRNX$intptlon10)

crimes_tract_BKLN <- over(x=crimes_BKLN, y=tract_BKLN)
crimes_tract_BKLN <- data.frame(tract=crimes_tract_BKLN$tractce10, tract_name=crimes_tract_BKLN$namelsad10,
                                latitude=crimes_tract_BKLN$intptlat10, longitude=crimes_tract_BKLN$intptlon10)

crimes_tract_MHTN <- over(x=crimes_MHTN, y=tract_MHTN)
crimes_tract_MHTN <- data.frame(tract=crimes_tract_MHTN$tractce10, tract_name=crimes_tract_MHTN$namelsad10,
                                latitude=crimes_tract_MHTN$intptlat10, longitude=crimes_tract_MHTN$intptlon10)

crimes_tract_QNS  <- over(x=crimes_QNS, y=tract_QNS)
crimes_tract_QNS  <- data.frame(tract=crimes_tract_QNS$tractce10, tract_name=crimes_tract_QNS$namelsad10,
                                latitude=crimes_tract_QNS$intptlat10, longitude=crimes_tract_QNS$intptlon10)

crimes_tract_STNI <- over(x=crimes_STNI, y=tract_STNI)
crimes_tract_STNI <- data.frame(tract=crimes_tract_STNI$tractce10, tract_name=crimes_tract_STNI$namelsad10,
                                latitude=crimes_tract_STNI$intptlat10, longitude=crimes_tract_STNI$intptlon10)

# Add Census tract_MHTN data to crime dataset
result_BRNX <- data.frame(crimes_BRNX@data, crimes_tract_BRNX)
result_BKLN <- data.frame(crimes_BKLN@data, crimes_tract_BKLN)
result_MHTN <- data.frame(crimes_MHTN@data, crimes_tract_MHTN)
result_QNS  <- data.frame(crimes_QNS@data,  crimes_tract_QNS)
result_STNI <- data.frame(crimes_STNI@data, crimes_tract_STNI)

# Stack individual dataframes into single result
result <- data.frame(rbind(result_BRNX, result_BKLN, result_MHTN, result_QNS, result_STNI))
write.csv(result, 'NewYork_clean.csv')

