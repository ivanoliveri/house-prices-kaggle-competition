library(mice)

library(reshape2)

library(Metrics)

library(ade4)

str.workingPath <- "C:/Users/user/Documents/NuevaVersionHouseCompetition/"

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

#Split into Training and Testing

df.newTrain <- df.allData[1:1460,]

df.newTrain <- data.frame(df.newTrain,vec.transformedResponse)
  
df.newTest <- df.allData[1461:2919,]

lm.model <- step(lm(vec.transformedResponse ~ ., data = df.newTrain))

#TEST
vec.predictions <- predict(lm.model, newdata = df.newTest)

vec.finalPredictions <- 10^(vec.predictions) - 1

df.resultSet <- data.frame(Id = vec.idsTesting, 
                           SalePrice = vec.finalPredictions)

  write.csv(df.resultSet, file = paste(str.workingPath, 
                                       "submission.csv", sep = ""), row.names=FALSE)
