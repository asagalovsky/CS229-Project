# Clear all
rm(list=ls())

#---------------------------------------------------------------------

# Set working directory
setwd('~/Documents/Stanford/CS229/CS229-Project/Merge/')

# Import datasets
crimes <- read.csv('Aggregated_Crimes.csv', header=T, stringsAsFactors=F)
census <- read.csv('census_total.csv', header=T, stringsAsFactors=F)

# Merge datasets
merged <- merge(crimes, census, by.x=c('City', 'Tract'), by.y=c('city', 'census_tract'), all.x=T)
names(merged) <- c('City', 'Tract', 'x', 'Frequency', 'y', 'Id', 'TotalPop', 'PopUnder5years', 
                   'Pop5to9years', 'Pop10to14years', 'Pop15to19years', 'Pop20to24years', 
                   'Pop25to29years', 'Pop30to34years', 'Pop35to39years', 'Pop40to44years',
                   'Pop45to49years', 'Pop50to54years', 'Pop55to59years', 'Pop60to64years',
                   'Pop65to69years', 'Pop70to74years', 'Pop75to79years', 'Pop80to84years', 
                   'Pop85yearsandover', 'MedianAge', 'Pop16yearsandover', 'Pop18yearsandover',
                   'Pop21yearsandover', 'Pop62yearsandover', 'Pop65yearsandover', 'MalePop', 
                   'FemalePop', 'RaceWhite', 'RaceBlack', 'RaceNative', 'RaceAsian', 
                   'RacePacificIslander', 'RaceOther', 'EthnHispanic', 'FamilyhouseholdsAll', 
                   'FamilyhouseholdChildunder18', 'FamilyhouseholdsHusbandwife', 
                   'FamilyhouseholdsMalehouseholder', 'FamilyhouseholdsFemalehouseholder', 
                   'NonfamilyhouseholdsAll', 'Nonfamilyhouseholdslivingalone', 
                   'NonfamilyhouseholdslivingaloneMale', 'NonfamilyhouseholdslivingaloneFemale',
                   'TotalHouseholdsUnder18years', 'TotalHouseholds65yearsover', 'AvgHHsize', 
                   'TotalHousingUnits', 'HousingVacantunits', 'HousingVacantunitsseasonal', 
                   'HousingOwneroccupied')

merged <- merged[,-which(names(merged) %in% c('Tract','x','y','Id'))]
class(merged$City) <- as.factor(merged$City)

merged[,-1] <- apply(merged[,-1], 2, as.numeric)
merged <- merged[merged$TotalPop != 0,]
merged$Response <- merged$Frequency / merged$TotalPop

merged <- merged[,-which(names(merged) == 'Frequency')]
merged$logResponse <- log(merged$Response)

#---------------------------------------------------------------------

## Data Visualization

par(mfrow=c(1,2))
hist(merged$Response,xlim=c(0,1),breaks=10200)
hist(merged$logResponse,breaks=100)

#---------------------------------------------------------------------

## Modeling/Prediction

# Center and scale data
cols_to_center <- which(!names(merged) %in% c('City','Response','logResponse'))
merged[,cols_to_center] <- scale(merged[,cols_to_center], center=T, scale=T)

# Test/train split
trainIndex <- sample(1:nrow(merged), 0.8*nrow(merged))
train <- merged[trainIndex, ]
test <- merged[-trainIndex, ]

# SUPERVISED

# Subset selection


# Linear models: OLS, Ridge, Lasso, Elastic Net

lm.model <- lm(Response ~ ., data=merged, na.action = na.omit)

# UNSUPERVISED

# K-means clustering
# PCA