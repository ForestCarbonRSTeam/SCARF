# Version 1.0: Spatial Mismatch and Systematic Prediction Error Corrected 
#              cAscade Random Forests (SCARF) algorithm
# 
# Date: 26. Jun, 2024

# Load library
# Used in R package randomForest.
library(randomForest)
library(Metrics)
library(plyr)

# Input features.
# Components of test and training data:
# (1) Spectral-temporal features from the Landsat time series 
# using the CCDC tool in the Google Earth Engine platform;
# (2) Foerst biomass of field plots.

# Load test and train data
# Change the path to your own code location
test_allfeat<-read.csv('C:/path/TestSet.csv')
train_allfeat<-read.csv('C:/path/TrainSet.csv')
test_allfeat<-na.omit(test_allfeat)
train_allfeat<-na.omit(train_allfeat)
head(test_allfeat)
head(train_allfeat)

# Calls BC.SLR algorithm
# Code path
path = "C:/path/"
setwd(path)
source('BC_SLR.R')

# Calls Prediction Random Forest.
# Fit the Normal Random Forest with training sets 
# and output the residuals of regression values at the first run.
# [,1:columns], columns represents the total number of columns
# [,"col+1"], col represents the total number of columns, remove "" at runtime
rf1 <- randomForest(x=train_allfeat[,1:columns], y=train_allfeat[,"col+1"],ntree=250,
                  important=TRUE,proximity=FALSE,corr.bias=FALSE)
print(importance(rf1))

# Use the univariate regression method to estimate the residuals of prediction values
# and alter the predictions by subtracting the residuals to obtain output values (Output).
RF_predict <- predict(rf1, train_allfeat)
x1 <- train_allfeat$Cden
y1 <- RF_predict
R2 <- cor(x1,y1)^2
rmse(y1,x1)
print(R2)

# Fit a simple linear regression (SLR) using the training data and Ouptput 
# as explanatory variables, corresponding to estimating residuals.
# i = 1
train_allfeat_CasRF <- cbind(y1,train_allfeat)
RF_predict2 <- predict.BC.SLR(rf1, train_allfeat, test_allfeat)
x1 <- test_allfeat$Cden
y1 <- RF_predict2
R2 <- cor(x1,y1)^2
rmse(y1,x1)
print(R2)
test_allfeat_CasRF <- cbind(y1,test_allfeat)
result <- matrix(nrow=124,ncol=1)
CasRF1 <- randomForest(x=train_allfeat_CasRF[,1:columns], y=train_allfeat_CasRF[,"col+1"],ntree=250,
                  important=TRUE,proximity=FALSE,corr.bias=FALSE)
test_R2 <- vector()

# The cascade RF continuously predicts output values with decreasing residual values, 
# including systematic prediction error and variance, 
# and stops when the residuals no longer decrease.
# i = 1 ——> i = n
for (i in 1:50){
  RF_predict <- predict(CasRF1, train_allfeat_CasRF)
  x1 <- train_allfeat_CasRF$Cden
  y1 <- RF_predict
  train_R2 <- cor(x1,y1)^2
  train_RMSE <- rmse(y1,x1)
  RF_predict2 <- predict.BC.SLR(CasRF1,train_allfeat_CasRF,test_allfeat_CasRF)
  train_allfeat_CasRF <- cbind(y1,train_allfeat)
  x1 <- test_allfeat_CasRF$Cden
  y1 <- RF_predict2
  test_R2[i] <- cor(x1,y1)^2
  test_RMSE <- rmse(y1,x1)
  test_allfeat_CasRF <- cbind(y1,test_allfeat)
  result <- cbind(result,y1)
  CasRF1 <- randomForest(x=train_allfeat_CasRF[,1:columns], y=train_allfeat_CasRF[,"col+1"],ntree=250,
                         important=TRUE,proximity=FALSE,corr.bias=FALSE)
  print(importance(CasRF1))
  # Cascade stops when the residuals no longer decrease.
  if(i>=1){
    print(test_R2[i])
    print(test_RMSE)
  }
  if(i>1){if (test_R2[i] <= test_R2[i-1]){
    print(i)
    break
  }}
}
result <- cbind(result,x1)
# Write result data
write.table(result,file='./path/Results/Results.csv',sep=",",quote=F,col.names=F,row.names=F)