# Clear all
rm(list=ls())

# Load libraries
library(devtools)
install_github("ggbiplot", "vqv")
library(ggbiplot)
library(ggplot2)
library(glmnet)
library(leaps)
library(MASS)
library(mboost)
library(ISLR)
library(pls)
library(randomForest)
library(reshape2)
library(splines)

#---------------------------------------------------------------------

# Set working directory
setwd('~/Documents/Stanford/CS229/CS229-Project/Merge/')
# setwd("/Users/alysonkane/desktop/cs229/census_aly")

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

setwd('~/Documents/Stanford/CS229/CS229-Project/Visualizations/')

jpeg('ResponseHistogram.jpg')
hist(merged$Response,xlim=c(0,1),breaks=10000, xlab='Nominal Rate', main='Crime Rate')
dev.off()

jpeg('LogResponseHistogram.jpg')
hist(merged$logResponse,breaks=100, xlab='Log Rate', main='Log Crime Rate')
dev.off()

jpeg('CrimeRate_Hist.jpeg')
ggplot(data=merged, aes(merged$Response)) + 
  geom_histogram(breaks=seq(0, 0.8, by=0.02), 
                 col="white", 
                 aes(fill=..count..)) +
                 labs(x="Crime Rate", y="Count")
dev.off()

jpeg('LogCrimeRate_Hist.jpeg')
ggplot(data=merged, aes(merged$logResponse)) + 
  geom_histogram(breaks=seq(-7, 1, by=0.2), 
                 col="white", 
                 aes(fill=..count..)) +
                 labs(x="Log Crime Rate", y="Count")
dev.off()
#---------------------------------------------------------------------

## Modeling/Prediction - Formatting data

# Center and scale data
cols_to_center <- which(!names(merged) %in% c('City','Response','logResponse'))
merged[,cols_to_center] <- scale(merged[,cols_to_center], center=T, scale=T)

## create dataset with Response variable and logResponse variable
mergedLog <- merged[,-which(names(merged) == 'Response')]
mergedResponse <- merged[,-which(names(merged) == 'logResponse')]

# Test/train split
trainIndex <- sample(1:nrow(merged), 0.7*nrow(merged))
train <- mergedLog[trainIndex, ]
test <- mergedLog[-trainIndex, ]

trainR <- mergedResponse[trainIndex, ]
testR <- mergedResponse[-trainIndex, ]

#---------------------------------------------------------------------

## SUPERVISED
## Linear models: OLS, Ridge, Lasso, Elastic Net

## (1) OLS 
lm.model <- lm(logResponse ~ ., data=train, na.action = na.omit)

# Calculate RMSE
lm.predict <- predict(lm.model, newdata = test)
lm.mse <- mean(na.omit(test$logResponse - lm.predict)^2)
lm.rmse <- sqrt(lm.mse)
sprintf("Baseline prediction error is: %f",lm.rmse)

# Diagnostic plots
# plot(lm.model)

## (2) Poisson GLM to Response 
glm.model <- glm(Response ~ ., data = trainR, family='poisson', na.action = na.omit)
glm.predict <- predict(glm.model, newdata = testR)

# Calculate RMSE
glm.mse <- mean(na.omit(testR$Response - glm.predict)^2)
glm.rmse <- sqrt(glm.mse)
sprintf("Poisson prediction error is: %f",glm.rmse)

# note: RMSE is much higher for poisson GLM.  Because of outliers (max is ~400%), model is predicting 
# negative numbers for smaller percentages.  This doesn't make sense, so we should continue to use log(Response).

## (3) Ridge, Lasso, Elastic Net

# Format Training Data
design <- model.matrix(~., mergedLog)
sampleInd <- sample(1:nrow(design), 0.7*nrow(design))

# Remove response and intercept variables 
Design <- design[,-which(colnames(design) %in% c('(Intercept)', 'logResponse'))]
Response <- design[,which(colnames(design) == 'logResponse')]

xTrain <- Design[sampleInd,]
xTest <- Design[-sampleInd,]
yTrain <- Response[sampleInd]
yTest <- Response[-sampleInd]

# Run penalized models 
lm.ridge <- glmnet(xTrain, yTrain, alpha = 0)
lm.lasso <- glmnet(xTrain, yTrain, alpha = 1)
lm.elasticnet <- glmnet(xTrain, yTrain, alpha = .5)

# CV to tune lambda 
set.seed(10)
cv.ridge <- cv.glmnet(xTrain,yTrain,alpha=0)
cv.lasso <- cv.glmnet(xTrain,yTrain,alpha=1)
cv.elasticnet <- cv.glmnet(xTrain,yTrain,alpha=.5)

bestlam.ridge <- cv.ridge$lambda.min
bestlam.lasso <- cv.lasso$lambda.min
bestlam.elasticnet <- cv.elasticnet$lambda.min

# calculate RMSE 
ridge.pred <- predict(lm.ridge, newx=xTest, s=bestlam.ridge)
lasso.pred <- predict(lm.lasso, newx=xTest, s=bestlam.lasso)

mseRidge <- mean((ridge.pred - yTest)^2)
mseLasso <- mean((lasso.pred - yTest)^2)

sprintf("Ridge prediction error is: %f", sqrt(mseRidge))
sprintf("Lasso prediction error is: %f", sqrt(mseLasso))

## Tune Alpha for Elastic Net 
for (i in 1:9) {
  assign(paste("fit", i, sep=""), cv.glmnet(xTrain, yTrain, type.measure="mse", 
                                            alpha=i/10, family="gaussian"))
}

yhat1 <- predict(fit1, s=fit1$lambda.1se, newx=xTest)
yhat2 <- predict(fit2, s=fit2$lambda.1se, newx=xTest)
yhat3 <- predict(fit3, s=fit3$lambda.1se, newx=xTest)
yhat4 <- predict(fit4, s=fit4$lambda.1se, newx=xTest)
yhat5 <- predict(fit5, s=fit5$lambda.1se, newx=xTest)
yhat6 <- predict(fit6, s=fit6$lambda.1se, newx=xTest)
yhat7 <- predict(fit7, s=fit7$lambda.1se, newx=xTest)
yhat8 <- predict(fit8, s=fit8$lambda.1se, newx=xTest)
yhat9 <- predict(fit9, s=fit9$lambda.1se, newx=xTest)

mse1 <- mean((yTest - yhat1)^2)
mse2 <- mean((yTest - yhat2)^2)
mse3 <- mean((yTest - yhat3)^2)
mse4 <- mean((yTest - yhat4)^2)
mse5 <- mean((yTest - yhat5)^2)
mse6 <- mean((yTest - yhat6)^2)
mse7 <- mean((yTest - yhat7)^2)
mse8 <- mean((yTest - yhat8)^2)
mse9 <- mean((yTest - yhat9)^2)

minRMSE <- min(c(sqrt(mse1),sqrt(mse2),sqrt(mse3),sqrt(mse4),sqrt(mse5),sqrt(mse6),sqrt(mse7),sqrt(mse8),sqrt(mse9)))
alpha <- which.min(c(sqrt(mse1),sqrt(mse2),sqrt(mse3),sqrt(mse4),sqrt(mse5),sqrt(mse6),sqrt(mse7),sqrt(mse8),sqrt(mse9)))

# sprintf("The min RMSE for the elastic net is: %f", minRMSE)
# sprintf("Corresponding Alpha value is: %f", alpha/10)

lm.elasticnet <- glmnet(xTrain, yTrain, alpha = alpha/10)
net.pred <- predict(lm.elasticnet, newx=xTest, s=bestlam.elasticnet)
mseNet <- mean((net.pred - yTest)^2)
sprintf("Elastic Net prediction error is: %f", sqrt(mseNet))

#---------------------------------------------------------------------

# Plot predictions and residuals
par(mfrow=c(1,1))

jpeg('OLS_Residuals.jpg')
plot(lm.predict - test$logResponse, ylim=c(-10,10), main='OLS Residuals')
abline(0,0)
dev.off()

jpeg('OLS_Residuals_Hist.jpg')
hist(lm.predict - test$logResponse, main='OLS Residuals', xlim=c(-10,10), breaks=50)
dev.off()

jpeg('Ridge_Residuals.jpg')
plot(ridge.pred - yTest, ylim=c(-10,10), main='Ridge Residuals')
abline(0,0)
dev.off()

jpeg('Ridge_Residuals_Hist.jpg')
hist(ridge.pred - yTest, main='Ridge Residuals', xlim=c(-10,10), breaks=50)
dev.off()

jpeg('Lasso_Residuals.jpg')
plot(lasso.pred - yTest, ylim=c(-10,10), main='Lasso Residuals')
abline(0,0)
dev.off()

jpeg('Lasso_Residuals_Hist.jpg')
hist(lasso.pred - yTest, main='Lasso Residuals', xlim=c(-10,10), breaks=50)
dev.off()

jpeg('ENet_Residuals.jpg')
plot(net.pred - yTest, ylim=c(-10,10), main='Elastic Net Residuals')
abline(0,0)
dev.off()

jpeg('ENet_Residuals_Hist.jpg')
hist(net.pred - yTest, main='Elastic Net Residuals', xlim=c(-10,10), breaks=50)
dev.off()

#---------------------------------------------------------------------

subsetDF <- as.data.frame(cbind(xTrain, yTrain))

# Random Forest
bag.model <- randomForest(yTrain~.,data=subsetDF,mtry=10,importance=TRUE)
bag.model

preds.bag <- predict(bag.model,newdata=xTest)
sqrt(mean((preds.bag-yTest)^2))

importance(bag.model)
varImpPlot(bag.model, main = 'Variable Importance')

jpeg('RF_Residuals.jpg')
hist(preds.bag - yTest, breaks=100, xlab='Residual', main='Random Forest Residuals')
dev.off()

# Plot learning curves for Random Forest
trainSize <- seq(0.05,0.95,0.05)
training_error <- c()
cv_error <- c()

for (size in trainSize) {
  print (size)
  
  sampleInd <- sample(1:nrow(design), size*nrow(design))
  xTrain <- Design[sampleInd,]
  xTest <- Design[-sampleInd,]
  yTrain <- Response[sampleInd]
  yTest <- Response[-sampleInd]
  
  subsetDF <- as.data.frame(cbind(xTrain, yTrain))
  
  bag.model <- randomForest(yTrain~.,data=subsetDF,mtry=10,importance=TRUE)
  train_preds.bag <- predict(bag.model,newdata=xTrain)
  test_preds.bag <- predict(bag.model,newdata=xTest)
  
  training_error <- cbind(training_error, sqrt(mean((train_preds.bag-yTrain)^2)))
  cv_error <- cbind(cv_error, sqrt(mean((test_preds.bag-yTest)^2)))
}

jpeg('RF_LearningCurve.jpg')
plot(trainSize, training_error, xlab='Training Size Fraction', ylab='Prediction Error', 
     ylim=c(0,2), type='l', lwd=2.5, col='blue', main='Learning Curve for Random Forest')
lines(trainSize, cv_error[1,], lwd=2.5, col='green')
legend('topright',legend=c('Training Error', 'Test Error'),lty=c(1,1),lwd=c(2.5,2.5),col=c('blue','green'))
dev.off()

learning_curve_table <- as.data.frame(rbind(training_error, cv_error))
names(learning_curve_table) <- as.character(seq(0.05,0.95,0.05))
write.csv(learning_curve_table, '~/Documents/Stanford/CS229/CS229-Project/CleanData/RF_learning_rate_table.csv')

# GBM
boosted.model <- glmboost(yTrain ~ ., data = subsetDF, control = boost_control(mstop = 1000))
coef(boosted.model, off2int=TRUE)

# Tune number of iterations
cvm <- cvrisk(boosted.model)
opt.iters <- which.min(apply(cvm, 2, mean))[[1]]

jpeg('Boosting_tuning.jpeg')
plot(cvm, main='Optimal Boosting Iterations using CV')
dev.off()

boosted.model <- glmboost(yTrain ~ ., data = subsetDF, control = boost_control(mstop = opt.iters))
boosted.pred <- predict(boosted.model, newdata=as.data.frame(xTest))
boosted.mse <- mean(na.omit(yTest - boosted.pred)^2)
boosted.rmse <- sqrt(boosted.mse)
sprintf("Boosting prediction error is: %f", boosted.rmse)

# Remove linear dependencies
remove_idx <- which(names(subsetDF) == 'FemalePop')
reduced_subset <- subsetDF[,-remove_idx]


best_subset <- regsubsets(yTrain ~., data=reduced_subset, nbest=1, nvmax=NULL, 
                          force.in=NULL, force.out=NULL, method = "forward")
summary.out <- summary(best_subset)

vars_to_keep <- seq(1,40,1)
rmse_vec <- rep(0,length(vars_to_keep))

# Grid search to optimize number of parameters to keep
for (i in vars_to_keep) {
  
  rel_features <- names(which(summary.out$which[i,] == TRUE))[-1]
  subset_train <- reduced_subset[,which(names(reduced_subset) %in% c(rel_features,'yTrain'))]
  subset_test <- as.data.frame(xTest[,which(colnames(xTest) %in% names(subset_train))])
  names(subset_test) <- names(subset_train)[-length(names(subset_train))]
  
  cv.lm.model <- lm(yTrain ~ ., data=subset_train, na.action = na.omit)
  cv.lm.predict <- predict(cv.lm.model, newdata=subset_test)
  cv.lm.rmse <- sqrt(mean(na.omit(yTest - cv.lm.predict)^2))
  rmse_vec[i] <- cv.lm.rmse
}

best_subset_size <- which.min(rmse_vec)
rel_features <- names(which(summary.out$which[best_subset_size,] == TRUE))[-1]

rel_vars <- reduced_subset[,which(names(reduced_subset) %in% c(rel_features, 'yTrain'))]
rel_subset <- regsubsets(yTrain ~., data=rel_vars, nbest=1, nvmax=NULL, force.in=NULL, force.out=NULL, method = "forward")

# Plot best subset size
jpeg('Best_Subset_Size.jpeg')
plot(rmse_vec, xlab='Number of features', ylab='RMSE', type='l', col='blue', ylim=c(0.85, 1.1), main='RMSE vs Subset Size for OLS', lty=1, lwd=2.5)
abline(min(rmse_vec), 0, lty=3)
dev.off()

# Linear vs. non-linear models
nl_y <- rel_vars[,ncol(rel_vars)]
nl_x <- as.matrix(rel_vars[,-ncol(rel_vars)])

# Linear
lm.fit <- lm(nl_y~nl_x)

# Polynomial
poly.fit3 <- lm(nl_y ~ poly(nl_x,3))

# Natural spline
nat_spline.fit3 <- lm(nl_y ~ ns(nl_x, 3))

# Smoothing splines
spline.fit <- smooth.spline(nl_y ~ nl_x, nknots=15)






# m.lower <- lm(yTrain ~ 1, data=subsetDF)
# m.upper <- lm(yTrain ~ ., data=subsetDF)
# 
# m.hybrid <- step(m.lower, scope=list(lower=m.lower, upper=m.upper), direction="both", na.action = na.omit)
# 
# y.pred.hybrid <- predict(m.hybrid,new =xTest)
# RMS.pred.hybrid <- sqrt(mean((y.pred.hybrid - yTest)^2))

# PCR
# set.seed(1)
# pcr.model <- pcr(logResponse ~ ., data=train, scale=TRUE, validation="CV")
# validationplot(pcr,val.type="MSEP")
# pcr.pred <- predict(pcr,test,ncomp=5)
# mse.pcr <- (pcr.pred-test$logResponse)^2
# rmse.pcr <- sqrt(mean(temp[!is.na(mse.pcr)]))



#---------------------------------------------------------------------

# Correlation matrix for k chosen predictors
Predictors <- c('FamilyhouseholdsHusbandwife','FamilyhouseholdsFemalehouseholder','TotalPop','RaceBlack', 
                'NonfamilyhouseholdslivingaloneMale', 'TotalHousingUnits', 'RaceWhite')
Corr_pred <- cbind(xTrain[,which(colnames(xTrain) %in% Predictors)], yTrain)
colnames(Corr_pred)[ncol(Corr_pred)] <- 'log(Crime Rate)'
cormat <- round(cor(Corr_pred),2)
melted_cormat <- melt(cormat)

ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

# Print the heatmap
jpeg('Predictor_heatmap.jpg')
print(ggheatmap)
dev.off()

#---------------------------------------------------------------------

# UNSUPERVISED

# K-means clustering
# cols_to_ignore <- c('City','Response', 'logResponse')
unsupervised <- na.omit(merged[,names(merged) %in% Predictors])

n_clusters <- 3
unsupervised$cluster <- kmeans(unsupervised, centers=n_clusters)$cluster


# PCA
merged.pca <- prcomp(unsupervised, center=TRUE, scale.=TRUE)
summary(merged.pca)
loadings(merged.pca)

plot(merged.pca, type='l')

unsupervised$cluster <- as.factor(unsupervised$cluster)
g <- ggbiplot(merged.pca, obs.scale = 1, var.scale = 1, 
              groups = unsupervised$cluster, ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')

jpeg('PCA_VarPlot.jpg')
print(g)
dev.off()

#---------------------------------------------------------------------

model.name <- c('OLS','Ridge','Lasso','ENet','RF','GBM','GAM',
                'RF (NYC)','w/ Classification','Augmented')
RMSE <- c(1.124245, 0.908030, 0.899431, 0.897220, 0.692712, 0.892571, 0.9049774, 0.4909961, 0.5489236, 0.4334941)
model.name <- factor(model.name,levels=unique(model.name))

jpeg('Model_Improvement.jpeg')
q <- qplot(model.name, RMSE, alpha=I(.5), geom='density', size=4, ylab="RMSE", ylim=c(0,1.2))
q + theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

