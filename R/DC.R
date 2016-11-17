# Clear all
rm(list=ls())

#---------------------------------------------------------------------

# Set working directory
setwd('~/Documents/Stanford/CS229/CS229-Project/Data/DC/')

# Load data
crimes <- read.csv('Crime_Incidents__2010.csv', header=T, stringsAsFactors=F)
crimes$Year <- as.numeric(substr(crimes$REPORTDATETIME, 1, 4))

crimes <- data.frame(year=crimes$Year, date=crimes$REPORTDATETIME, type=crimes$OFFENSE, tract=crimes$CENSUS_TRACT)

# Only retain complete cases (remove NAs)
crimes <- crimes[complete.cases(crimes), ]

result <- crimes
write.csv(result, 'DC_clean.csv')