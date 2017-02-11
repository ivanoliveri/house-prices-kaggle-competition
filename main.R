library(mice)

library(reshape2)

library(Metrics)

library(ade4)

library(caret)

str.workingPath <- "C:/Users/Ivan/Documents/GitHub/house-prices-kaggle-competition/"

str.trainingFilename <- "train.csv"

str.testingFilename <- "test.csv"

df.train <- read.csv(paste(str.workingPath,str.trainingFilename,
                           sep =""))

df.test <- read.csv(paste(str.workingPath,str.testingFilename,
                          sep = ""))

vec.response <- df.train$SalePrice

vec.transformedResponse <- log(df.train$SalePrice + 1, 10)

vec.idsTesting <- df.test$Id

df.allData <- rbind(df.train[,-81],df.test[,])

#Imputate Missing Values
vec.columnsWithMissingValuesOne <- c("MSZoning","Exterior1st","Exterior2nd","BsmtFinSF1",
                                  "BsmtFinSF2","BsmtUnfSF","TotalBsmtSF","BsmtFullBath",
                                  "BsmtHalfBath","KitchenQual","Functional","GarageCars",
                                  "GarageArea")

vec.columnsWithMissingValuesTwo <- c("SaleType","Electrical","FireplaceQu","GarageType",
                                     "GarageYrBlt","GarageFinish","GarageQual","GarageCond","PoolQC",
                                     "Fence","MiscFeature","LotFrontage","Alley","Utilities",
                                     "MasVnrType","MasVnrArea","BsmtQual","BsmtCond","BsmtExposure",
                                     "BsmtFinType1","BsmtFinType2")

lst.missingValues <- list(vec.columnsWithMissingValuesOne,
                          vec.columnsWithMissingValuesTwo)

for(vec.missingValues in lst.missingValues){
 
  df.imputations <- df.allData[, vec.missingValues]
  
  df.imputated <- complete(mice(df.imputations, m=2))
  
  for(str.columnName in vec.missingValues){
    
    df.allData[,str.columnName] <- df.imputated[,str.columnName]
    
  }
   
}

df.allData <- df.allData[,-1]

#Transform Skew Variables

vec.numericColumns <- names(df.allData)[unlist(lapply(names(df.allData), function(x) 
                      is.numeric(df.allData[,x])))]

vec.skewNumericColumns <- vec.numericColumns[unlist(lapply(vec.numericColumns, function(x) 
                                                           skewness(df.allData[,x])>=0.75))]

for(oneSkewColumn in vec.skewNumericColumns){
  
  df.allData[,oneSkewColumn] = log(df.allData[,oneSkewColumn] + 1, 10)
  
}

#CONVERT QUALITY MEASURES TO NUMERIC

vec.qualityMeaseures <- c("BsmtQual","ExterQual","GarageQual","KitchenQual","HeatingQC")

for(str.qualityMeaseure in vec.qualityMeaseures){
  
  df.allData[,str.qualityMeaseure] =  ifelse(df.allData[,str.qualityMeaseure] == "Po",1,
                                      ifelse(df.allData[,str.qualityMeaseure] == "Fa",2,
                                      ifelse(df.allData[,str.qualityMeaseure]=="TA",3,
                                      ifelse(df.allData[,str.qualityMeaseure]=="Gd",4,5))))
  
}

vec.houseAge <- 2010 - df.allData$YearBuilt

vec.timeSinceSold <- 2010 - df.allData$YrSold

vec.timeSinceRemodel <- df.allData$YrSold - df.allData$YearRemodAdd

df.allData <- data.frame(df.allData, vec.houseAge, vec.timeSinceSold, 
                         vec.timeSinceRemodel)

#One Hot Encoding

vec.factorColumns <- names(df.allData)[unlist(lapply(names(df.allData), 
                                                      function(x) is.factor(df.allData[,x])))]

df.newFactor <- data.frame(a=1:2919)

for(str.columnName in vec.factorColumns){
  
  df.currentDummy <- acm.disjonctif(df.allData[str.columnName])
  
  df.newFactor <- data.frame(df.newFactor, df.currentDummy)
  
  df.allData <- df.allData[,-which(str.columnName == names(df.allData))]

}

df.newFactor <- df.newFactor[,-1]

#Delete One Hot Enconding Columns that are Missing in Testing

vec.columnsNamesToRemove <- c("Exterior1st.ImStucc", "Exterior1st.Stone", "Exterior2nd.Other", "HouseStyle.2.5Fin", "RoofMatl.Membran", "RoofMatl.Metal", "RoofMatl.Roll",
                              "Condition2.RRAn", "Condition2.RRAe", "Condition2.RRNn", "Heating.Floor", "Heating.OthW", "Electrical.Mix", "MiscFeature.TenC", "PoolQC.Fa")

for(str.columnName in vec.columnsNamesToRemove){
  
  df.newFactor <- df.newFactor[,-which(str.columnName == names(df.newFactor))]
  
}

df.allData <- data.frame(df.allData, df.newFactor)

vec.columnsWithNearToZeroVariance <- c("MSZoning.C..all.","MSZoning.RH","Street.Grvl","LotShape.IR2","LotShape.IR3","LandContour.Low",
                                       "Utilities.NoSeWa","LotConfig.FR3","LandSlope.Sev","Neighborhood.Blmngtn","Neighborhood.Blueste",
                                       "Neighborhood.BrDale","Neighborhood.ClearCr","Neighborhood.MeadowV","Neighborhood.NoRidge",
                                       "Neighborhood.NPkVill","Neighborhood.StoneBr","Neighborhood.SWISU","Neighborhood.Timber",
                                       "Neighborhood.Veenker","Condition1.Artery","Condition1.PosA","Condition1.PosN","Condition1.RRAe",
                                       "Condition1.RRAn","Condition1.RRNe","Condition1.RRNn","Condition2.Artery","Condition2.Feedr",
                                       "Condition2.PosA", "Condition2.PosN","BldgType.2fmCon", "BldgType.Twnhs","HouseStyle.1.5Unf",
                                       "HouseStyle.2.5Unf","HouseStyle.SFoyer","RoofStyle.Flat","RoofStyle.Gambrel","RoofStyle.Mansard",
                                       "RoofStyle.Shed","RoofMatl.ClyTile","RoofMatl.CompShg","RoofMatl.Tar.Grv","RoofMatl.WdShake",
                                       "RoofMatl.WdShngl","Exterior1st.AsbShng","Exterior1st.AsphShn","Exterior1st.BrkComm","Exterior1st.CBlock",
                                       "Exterior1st.Stucco","Exterior1st.WdShing","Exterior2nd.AsbShng","Exterior2nd.AsphShn","Exterior2nd.Brk.Cmn",
                                       "Exterior2nd.BrkFace","Exterior2nd.CBlock","Exterior2nd.ImStucc","Exterior2nd.Stone","Exterior2nd.Stucco",
                                       "MasVnrType.BrkCmn","ExterCond.Ex","ExterCond.Fa","ExterCond.Po","Foundation.Slab","Foundation.Stone",
                                       "Foundation.Wood","BsmtCond.Po","BsmtFinType2.ALQ","BsmtFinType2.BLQ","BsmtFinType2.GLQ","Heating.GasW",
                                       "Heating.Grav","Heating.Wall","Electrical.FuseF","Electrical.FuseP","Functional.Maj1","Functional.Maj2",
                                       "Functional.Min1","Functional.Min2","Functional.Mod","Functional.Sev","FireplaceQu.Ex","GarageType.Basment",
                                       "GarageType.CarPort","GarageCond.Ex","GarageCond.Fa","GarageCond.Gd","GarageCond.Po","PavedDrive.P",
                                       "SaleType.COD","SaleType.Con","SaleType.ConLD","SaleType.ConLI","SaleType.ConLw","SaleType.CWD",
                                       "SaleType.Oth","SaleCondition.AdjLand","SaleCondition.Alloca","SaleCondition.Family","Utilities.AllPub")

for(str.columnToRemove in vec.columnsWithNearToZeroVariance){
  
  int.columnPositionToRemove <- which(str.columnToRemove == names(df.allData))
  
  df.allData <- df.allData[,-int.columnPositionToRemove]
  
}


vec.totalArea <-  df.allData$LotArea + df.allData$MasVnrArea + df.allData$GrLivArea + df.allData$PoolArea +
                  df.allData$X1stFlrSF + df.allData$X2ndFlrSF
  
vec.totalBathrooms <- df.allData$BsmtFullBath + df.allData$BsmtHalfBath + df.allData$FullBath + df.allData$HalfBath
  
vec.secondFloorIndicator <- ifelse(df.allData$X2ndFlrSF >0,1,0)
  
df.allData <- data.frame(df.allData, vec.totalArea, vec.totalBathrooms, vec.secondFloorIndicator)

#Split into Training and Testing

df.newTrain <- df.allData[1:1460,]

df.newTrain <- data.frame(df.newTrain,vec.transformedResponse)
  
df.newTest <- df.allData[1461:2919,]

#TRAIN MODELS

firstLayer.control <- trainControl(method="cv", 
                                   number = 5, 
                                   allowParallel = TRUE,
                                   verboseIter = TRUE)

mars.grid <- expand.grid(degree = c(2,3,5))

mars.model <- train(df.newTrain[,-length(df.newTrain)], 
                    vec.transformedResponse,	
                    method = 'gcvEarth', 
                    trControl = firstLayer.control,
                    tuneGrid = mars.grid,
                    metric='RMSE')

lm.model <- step(lm(vec.transformedResponse ~ ., data = df.newTrain))

#Measure Training Before Outliers Filtering

vec.predictionsMARS <- 10^(predict(mars.model, newdata = df.newTrain)) - 1

vec.predictionsLM <- 10^(predict(lm.model, newdata = df.newTrain)) - 1

vec.finalPredictions <-  (vec.predictionsMARS+vec.predictionsLM)/2

print(paste("RMSE",rmse(log(df.newTrain$vec.transformedResponse,10),
                        log(vec.finalPredictions,10))))

#Measure Training After Outliers Filtering

df.newTrainFiltered <- data.frame(df.newTrain, 
                                  diff = (vec.finalPredictions-(10^(df.newTrain$vec.transformedResponse)-1))/
                                          (10^(df.newTrain$vec.transformedResponse)-1))

df.newTrainFiltered <- subset(df.newTrainFiltered, abs(diff)<0.4)

df.newTrainFiltered <- df.newTrainFiltered[,-length(df.newTrainFiltered)]

mars.model <- train(df.newTrainFiltered[,-length(df.newTrainFiltered)], 
                    df.newTrainFiltered$vec.transformedResponse,	
                    method = 'gcvEarth', 
                    trControl = firstLayer.control,
                    tuneGrid = mars.grid,
                    metric='RMSE')

lm.model <- step(lm(vec.transformedResponse ~ ., data = df.newTrainFiltered))

lasso.grid <- expand.grid(lambda = c(0.001,0.005, 0.0001, 0.0005,0.00001))

lasso.model <- train( df.newTrainFiltered[,-length(df.newTrainFiltered)], 
                    df.newTrainFiltered$vec.transformedResponse,		
                    method = "rqlasso", trControl = firstLayer.control,		
                    preProc = c("center", "scale"),		          
                    tuneGrid = lasso.grid, metric="RMSE")

vec.predictionsMARS <- 10^(predict(mars.model, newdata = df.newTrainFiltered)) - 1

vec.predictionsLM <- 10^(predict(lm.model, newdata = df.newTrainFiltered)) - 1

vec.selectedColumns <- names(lasso.model$trainingData)[-length(names(lasso.model$trainingData))]

vec.predictionsLASSO <- 10^(predict(lasso.model, newdata = df.newTrainFiltered[,vec.selectedColumns])) - 1

vec.finalPredictions <-  (vec.predictionsMARS+vec.predictionsLM+vec.predictionsLASSO)/3

print(paste("RMSE",rmse(log(df.newTrainFiltered$vec.transformedResponse,10),
     log(vec.finalPredictions,10))))

#Measure Testing

vec.predictionsMARS <- 10^(predict(mars.model, newdata = df.newTest)) - 1

vec.predictionsLM <- 10^(predict(lm.model, newdata = df.newTest)) - 1

vec.selectedColumns <- names(lasso.model$trainingData)[-length(names(lasso.model$trainingData))]

vec.predictionsLASSO <- 10^(predict(lasso.model, newdata = df.newTest[,vec.selectedColumns])) - 1

vec.finalPredictions <-  (vec.predictionsMARS+vec.predictionsLM+vec.predictionsLASSO)/3

df.resultSet <- data.frame(Id = vec.idsTesting, SalePrice = vec.finalPredictions)

write.csv(df.resultSet, file = paste(str.workingPath, "submission.csv", sep = ""), row.names=FALSE)
