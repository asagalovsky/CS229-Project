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

ggbiplot2 <- function (pcobj, choices = 1:2, scale = 1, pc.biplot = TRUE, 
                       obs.scale = 1 - scale, var.scale = scale, groups = NULL, 
                       ellipse = FALSE, ellipse.prob = 0.68, labels = NULL, labels.size = 3, 
                       alpha = 1, var.axes = TRUE, circle = FALSE, circle.prob = 0.69, 
                       varname.size = 3, varname.adjust = 1.5, varname.abbrev = FALSE, 
                       color = muted("red"), # <- add new arguments to the function
                       linetype = "solid",
                       alpha_arrow = 1)
{
  library(ggplot2)
  library(plyr)
  library(scales)
  library(grid)
  stopifnot(length(choices) == 2)
  if (inherits(pcobj, "prcomp")) {
    nobs.factor <- sqrt(nrow(pcobj$x) - 1)
    d <- pcobj$sdev
    u <- sweep(pcobj$x, 2, 1/(d * nobs.factor), FUN = "*")
    v <- pcobj$rotation
  }
  else if (inherits(pcobj, "princomp")) {
    nobs.factor <- sqrt(pcobj$n.obs)
    d <- pcobj$sdev
    u <- sweep(pcobj$scores, 2, 1/(d * nobs.factor), FUN = "*")
    v <- pcobj$loadings
  }
  else if (inherits(pcobj, "PCA")) {
    nobs.factor <- sqrt(nrow(pcobj$call$X))
    d <- unlist(sqrt(pcobj$eig)[1])
    u <- sweep(pcobj$ind$coord, 2, 1/(d * nobs.factor), FUN = "*")
    v <- sweep(pcobj$var$coord, 2, sqrt(pcobj$eig[1:ncol(pcobj$var$coord), 
                                                  1]), FUN = "/")
  }
  else if (inherits(pcobj, "lda")) {
    nobs.factor <- sqrt(pcobj$N)
    d <- pcobj$svd
    u <- predict(pcobj)$x/nobs.factor
    v <- pcobj$scaling
    d.total <- sum(d^2)
  }
  else {
    stop("Expected a object of class prcomp, princomp, PCA, or lda")
  }
  choices <- pmin(choices, ncol(u))
  df.u <- as.data.frame(sweep(u[, choices], 2, d[choices]^obs.scale, 
                              FUN = "*"))
  v <- sweep(v, 2, d^var.scale, FUN = "*")
  df.v <- as.data.frame(v[, choices])
  names(df.u) <- c("xvar", "yvar")
  names(df.v) <- names(df.u)
  if (pc.biplot) {
    df.u <- df.u * nobs.factor
  }
  r <- sqrt(qchisq(circle.prob, df = 2)) * prod(colMeans(df.u^2))^(1/4)
  v.scale <- rowSums(v^2)
  df.v <- r * df.v/sqrt(max(v.scale))
  if (obs.scale == 0) {
    u.axis.labs <- paste("standardized PC", choices, sep = "")
  }
  else {
    u.axis.labs <- paste("PC", choices, sep = "")
  }
  u.axis.labs <- paste(u.axis.labs, sprintf("(%0.1f%% explained var.)", 
                                            100 * pcobj$sdev[choices]^2/sum(pcobj$sdev^2)))
  if (!is.null(labels)) {
    df.u$labels <- labels
  }
  if (!is.null(groups)) {
    df.u$groups <- groups
  }
  if (varname.abbrev) {
    df.v$varname <- abbreviate(rownames(v))
  }
  else {
    df.v$varname <- rownames(v)
  }
  df.v$angle <- with(df.v, (180/pi) * atan(yvar/xvar))
  df.v$hjust = with(df.v, (1 - varname.adjust * sign(xvar))/2)
  g <- ggplot(data = df.u, aes(x = xvar, y = yvar)) + xlab(u.axis.labs[1]) + 
    ylab(u.axis.labs[2]) + coord_equal()
  if (var.axes) {
    if (circle) {
      theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, 
                                                length = 50))
      circle <- data.frame(xvar = r * cos(theta), yvar = r * 
                             sin(theta))
      g <- g + geom_path(data = circle, color = muted("white"), 
                         size = 1/2, alpha = 1/3)
    }
    g <- g + geom_segment(data = df.v, aes(x = 0, y = 0, xend = xvar, yend = yvar),
                          arrow = arrow(length = unit(1/2, "picas")),
                          color = color, linetype = linetype, alpha = alpha_arrow)
  }
  if (!is.null(df.u$labels)) {
    if (!is.null(df.u$groups)) {
      g <- g + geom_text(aes(label = labels, color = groups), 
                         size = labels.size)
    }
    else {
      g <- g + geom_text(aes(label = labels), size = labels.size)
    }
  }
  else {
    if (!is.null(df.u$groups)) {
      g <- g + geom_point(aes(color = groups), alpha = alpha)
    }
    else {
      g <- g + geom_point(alpha = alpha)
    }
  }
  if (!is.null(df.u$groups) && ellipse) {
    theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
    circle <- cbind(cos(theta), sin(theta))
    ell <- ddply(df.u, "groups", function(x) {
      if (nrow(x) <= 2) {
        return(NULL)
      }
      sigma <- var(cbind(x$xvar, x$yvar))
      mu <- c(mean(x$xvar), mean(x$yvar))
      ed <- sqrt(qchisq(ellipse.prob, df = 2))
      data.frame(sweep(circle %*% chol(sigma) * ed, 2, 
                       mu, FUN = "+"), groups = x$groups[1])
    })
    names(ell)[1:2] <- c("xvar", "yvar")
    g <- g + geom_path(data = ell, aes(color = groups, group = groups))
  }
  if (var.axes) {
    g <- g + geom_text(data = df.v, aes(label = varname, 
                                        x = xvar, y = yvar, angle = angle, hjust = hjust), 
                       color = "darkred", size = varname.size)
  }
  return(g)
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

Id_pop <- data.frame(geoid10=census_aug$Id, Pop=census_aug$TotalPop)
write.csv(Id_pop, 'Id_population_lookup.csv')

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

write.csv(merged, '~/Documents/Stanford/CS229/CS229-Project/CleanData/NewYork_Master.csv')

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

#---------------------------------------------------------------------------------

# PCA
Predictors <- c('FamilyhouseholdsHusbandwife','FamilyhouseholdsFemalehouseholder','TotalPop','RaceBlack', 
                'NonfamilyhouseholdslivingaloneMale', 'TotalHousingUnits','yTrain')

unsupervised <- na.omit(subsetDF[,names(subsetDF) %in% Predictors])
names(unsupervised)[ncol(unsupervised)] <- 'logCrimeRate'

n_clusters <- 2
unsupervised$cluster <- kmeans(unsupervised$logCrimeRate, centers=n_clusters)$cluster
clusters <- unsupervised$cluster

unsupervised <- unsupervised[,-ncol(unsupervised)]

merged.pca <- prcomp(unsupervised, center=TRUE, scale.=TRUE)
summary(merged.pca)
loadings(merged.pca)

plot(merged.pca, type='l')

unsupervised$cluster <- as.factor(clusters)
unsupervised$label <- rep('NA', nrow(unsupervised))

unsupervised[unsupervised$cluster == '1',]$label <- 'High'
unsupervised[unsupervised$cluster == '2',]$label <- 'Low'

# unsupervised$cluster <- as.factor(unsupervised$cluster)

g <- ggbiplot(merged.pca, obs.scale = 1, var.scale = 1, 
              groups = unsupervised$label, ellipse = TRUE, 
              circle = TRUE, arrow.color = "black")
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')


g$layers <- c(g$layers, g$layers[[2]])

setwd('~/Documents/Stanford/CS229/CS229-Project/Visualizations/')
jpeg('PCA.jpeg')
print (g)
dev.off()

#---------------------------------------------------------------------------------

# Cluster on top predictors, plot overlapping distributions of log crime rate
Predictors <- c('FamilyhouseholdsHusbandwife','FamilyhouseholdsFemalehouseholder','TotalPop','RaceBlack', 
                'NonfamilyhouseholdslivingaloneMale', 'TotalHousingUnits','HousingVacantunits','RaceWhite',
                'IncomeLT10k','yTrain')

unsupervised <- na.omit(subsetDF[,names(subsetDF) %in% Predictors])
names(unsupervised)[ncol(unsupervised)] <- 'logCrimeRate'

n_clusters <- 3
unsupervised$cluster <- kmeans(unsupervised[,-ncol(unsupervised)], centers=n_clusters)$cluster
