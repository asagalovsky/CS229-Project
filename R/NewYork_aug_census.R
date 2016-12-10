# Add new census features, re-run models for violent/non-violent crime, report RMSE

# Clear all
rm(list=ls())

# Load libraries
library(caret)
library(party)
library(partykit)
library(randomForest)
library(rattle)
library(RColorBrewer)
library(rpart)
library(rpart.plot)

#---------------------------------------------------------------------------------

getTract <- function(x) {
  y = unlist(strsplit(x, ' '))
  y[3]
}

#---------------------------------------------------------------------------------

# Build augmented Census dataset
setwd('~/Documents/Stanford/CS229/CS229-Project/Merge/')
census_addl <- read.csv('~/Documents/Stanford/CS229/CS229-Project/Merge/nyc_additional_clean.csv', header=T, stringsAsFactors=F)
census <- read.csv('census_total.csv', header=T, stringsAsFactors=F)

names(census) <- c('x', 'Id', 'TotalPop', 'PopUnder5years', 
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
                   'HousingOwneroccupied','city', 'census_tract')

census_aug <- merge(census, census_addl, by=c('city','census_tract'))

# Merge with crimes
crimes <- read.csv('~/Documents/Stanford/CS229/CS229-Project/Merge/Aggregated_Crimes.csv')
crimes <- crimes[which(crimes$City %in% c('Bronx','Brooklyn', 'Manhattan', 'Queens', 'StatenIsland')),]

merged <- merge(crimes, census_aug, by.x=c('City','Tract'), by.y=c('city','census_tract'), all.x=T)

# Remove irrelevant fields
merged <- merged[,-which(names(merged) %in% c('Tract','X.x','X.y','x','Id'))]
class(merged$City) <- as.factor(merged$City)

merged[,-1] <- apply(merged[,-1], 2, as.numeric)
merged <- merged[merged$TotalPop != 0,]
merged$Response <- merged$Frequency / merged$TotalPop

merged <- merged[,-which(names(merged) == 'Frequency')]
merged$logResponse <- log(merged$Response)

cols_to_center <- which(!names(merged) %in% c('City','Response','logResponse'))
merged[,cols_to_center] <- scale(merged[,cols_to_center], center=T, scale=T)
mergedLog <- merged[,-which(names(merged) == 'Response')]

design <- model.matrix(~., mergedLog)
sampleInd <- sample(1:nrow(design), 0.7*nrow(design))

# Remove response and intercept variables 
Design <- design[,-which(colnames(design) %in% c('(Intercept)', 'logResponse'))]
Response <- design[,which(colnames(design) == 'logResponse')]

xTrain <- Design[sampleInd,]
xTest <- Design[-sampleInd,]
yTrain <- Response[sampleInd]
yTest <- Response[-sampleInd]

subsetDF <- as.data.frame(cbind(xTrain, yTrain))

# Random Forest
bag.model <- randomForest(yTrain~.,data=subsetDF,mtry=10,importance=TRUE)
bag.model

preds.bag <- predict(bag.model,newdata=xTest)
sqrt(mean((preds.bag-yTest)^2))

importance(bag.model)
varImpPlot(bag.model, main = 'Variable Importance')

#---------------------------------------------------------------------------------

# TODO: unscale data
unscaled <- merged[,-which(names(merged) == 'Response')]

design <- model.matrix(~., unscaled)
sampleInd <- sample(1:nrow(design), 0.7*nrow(design))

# Remove response and intercept variables 
Design <- design[,-which(colnames(design) %in% c('(Intercept)', 'logResponse'))]
Response <- design[,which(colnames(design) == 'logResponse')]

xTrain <- Design[sampleInd,]
xTest <- Design[-sampleInd,]
yTrain <- Response[sampleInd]
yTest <- Response[-sampleInd]

subsetDF <- as.data.frame(cbind(xTrain, yTrain))

# RPART for tree split
rpart.model <- rpart(yTrain ~ ., data = subsetDF)

setwd('~/Documents/Stanford/CS229/CS229-Project/Visualizations/')

jpeg('RPART_tree.jpeg')
prp(rpart.model, main='Recursive Partition Tree')
dev.off()

jpeg('Fancy_RPART_tree.jpeg')
fancyRpartPlot(rpart.model)
dev.off()

