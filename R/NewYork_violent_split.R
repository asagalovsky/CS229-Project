# TODO
# - Construct model using original New York dataset with no distinction between violent and non-violent, report RMSE
# - Break down crimes by violent/non-violent, create two models, compute nominal crimes, merge, report RMSE
# - Add new census features, re-run models for violent/non-violent crime, report RMSE
# - Report variable importance

# Clear all
rm(list=ls())

# Load libraries
library(randomForest)

#---------------------------------------------------------------------------------

getTract <- function(x) {
  y = unlist(strsplit(x, ' '))
  y[3]
}

#---------------------------------------------------------------------

# Read in violent crimes dataset
violent_crimes <- read.csv('~/Documents/Stanford/CS229/CS229-Project/CleanData/NewYork_violent.csv')

violent_crimes$tract_name <- as.character(violent_crimes$tract_name)
violent_crimes$tract <- lapply(violent_crimes$tract_name, getTract)
violent_crimes$tract <- unlist(violent_crimes$tract)
violent_crimes <- violent_crimes[,-c(1:2)]

names(violent_crimes) <- c('Frequency', 'City', 'Tract')

# Read in original census dataset
census <- read.csv('~/Documents/Stanford/CS229/CS229-Project/Merge/census_total.csv')

# Merge violent crimes with census data
merged <- merge(violent_crimes, census, by.x=c('City','Tract'), by.y=c('city','census_tract'), all.x=T)
names(merged) <- c('City', 'Tract', 'Frequency', 'x', 'Id', 'TotalPop', 'PopUnder5years', 
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

# Remove NA values
merged <- na.omit(merged)

# Remove irrelevant fields
merged <- merged[,-which(names(merged) %in% c('Tract','x','y','Id'))]
class(merged$City) <- as.factor(merged$City)

merged[,-1] <- apply(merged[,-1], 2, as.numeric)
merged <- merged[merged$TotalPop != 0,]
merged$Response <- merged$Frequency / merged$TotalPop

merged <- merged[,-which(names(merged) == 'Frequency')]
merged$logResponse <- log(merged$Response)

# Center and scale data
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
violent.rmse <- sqrt(mean((preds.bag-yTest)^2))
num_violent <- sum(violent_crimes$Frequency)

#---------------------------------------------------------------------

# Read in nonviolent crimes dataset
nonviolent_crimes <- read.csv('~/Documents/Stanford/CS229/CS229-Project/CleanData/NewYork_nonviolent.csv')

nonviolent_crimes$tract_name <- as.character(nonviolent_crimes$tract_name)
nonviolent_crimes$tract <- lapply(nonviolent_crimes$tract_name, getTract)
nonviolent_crimes$tract <- unlist(nonviolent_crimes$tract)
nonviolent_crimes <- nonviolent_crimes[,-c(1:2)]

names(nonviolent_crimes) <- c('Frequency', 'City', 'Tract')

# Merge nonviolent crimes with census data
merged <- merge(nonviolent_crimes, census, by.x=c('City','Tract'), by.y=c('city','census_tract'), all.x=T)
names(merged) <- c('City', 'Tract', 'Frequency', 'x', 'Id', 'TotalPop', 'PopUnder5years', 
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

# Remove NA values
merged <- na.omit(merged)

# Remove irrelevant fields
merged <- merged[,-which(names(merged) %in% c('Tract','x','y','Id'))]
class(merged$City) <- as.factor(merged$City)

merged[,-1] <- apply(merged[,-1], 2, as.numeric)
merged <- merged[merged$TotalPop != 0,]
merged$Response <- merged$Frequency / merged$TotalPop

merged <- merged[,-which(names(merged) == 'Frequency')]
merged$logResponse <- log(merged$Response)

# Center and scale data
cols_to_center <- which(!names(merged) %in% c('City','Response','logResponse'))
merged[,cols_to_center] <- scale(merged[,cols_to_center], center=T, scale=T)
mergedLog <- merged[,-which(names(merged) == 'Response')]

design <- model.matrix(~., mergedLog)

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
nonviolent.rmse <- sqrt(mean((preds.bag-yTest)^2))
num_nonviolent <- sum(nonviolent_crimes$Frequency)

#---------------------------------------------------------------------

total_crimes <- num_violent + num_nonviolent
rmse.total <- (num_nonviolent / total_crimes) * nonviolent.rmse + (num_violent / total_crimes) * violent.rmse
rmse.total