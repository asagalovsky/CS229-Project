# Clear all
rm(list=ls())

#---------------------------------------------------------------------

getTract <- function(x) {
  y = unlist(strsplit(x, ' '))
  y[3]
}

#---------------------------------------------------------------------

# Set working directory
setwd('~/Documents/Stanford/CS229/CS229-Project/CleanData/')

# Read in individual CSV files
merged <- do.call(rbind, lapply(list.files(pattern = ".csv"), read.csv))

merged$tract_name <- as.character(merged$tract_name)
merged$tract <- lapply(merged$tract_name, getTract)
merged$tract <- unlist(merged$tract)
merged <- merged[,-c(1:2)]

names(merged) <- c('Frequency', 'City', 'Tract')

write.csv(merged, 'Aggregated_Crimes.csv')
