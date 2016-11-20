# Clear all
rm(list=ls())

# Load libraries
library(plyr)

#---------------------------------------------------------------------

# Function definitions
substrRight <- function(x, n) {
  substr(x, nchar(x)-n+1, nchar(x))
}

#---------------------------------------------------------------------

# Set working directory
setwd('~/Documents/Stanford/CS229/CS229-Project/RawData/DC/')

# Load data
crimes <- read.csv('Crime_Incidents__2010.csv', header=T, stringsAsFactors=F)
crimes$Year <- as.numeric(substr(crimes$REPORTDATETIME, 1, 4))

crimes <- data.frame(year=crimes$Year, date=crimes$REPORTDATETIME, type=crimes$OFFENSE, tract_name=crimes$CENSUS_TRACT)

# Only retain complete cases (remove NAs)
crimes <- crimes[complete.cases(crimes), ]

result <- crimes
result <- count(result, c('tract_name'))
result$city <- rep('DC', nrow(result))

for (i in 1:nrow(result)) {
  if (substrRight(result[i,]$tract_name, 2) == '00') {
    result[i,]$tract_name <- paste('Census Tract ', substr(result[i,]$tract_name, 1, nchar(result[i,]$tract_name) - 2), sep='')
  }
  else {
    result[i,]$tract_name <- paste('Census Tract ', as.numeric(result[i,]$tract_name) / 100, sep='')
  }
}

write.csv(result, '~/Documents/Stanford/CS229/CS229-Project/CleanData/DC_clean.csv')
