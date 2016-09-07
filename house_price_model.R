#Load Libraries

library(randomForest)

library(Metrics)

#Define Constants

kFilePath <- "C:/Users/Ivan/Documents/GitHub/house-prices-kaggle-competition/"

kTrainFile <- "train.csv"

kTestFile <- "test.csv"

kOutputFile <- "predictions.csv"

kFeatureEngineeringFile <- "house_price_feature_engineering.R"

#Load User Defined Functions

source(paste(kFilePath,kFeatureEngineeringFile,sep=""))

df.trainData <- read.csv(file = paste(kFilePath,kTrainFile, sep = ""), header = T)

df.testData <- read.csv(file = paste(kFilePath,kTestFile, sep = ""), header = T)

vec.salePrice <- df.trainData$SalePrice

vec.trainID <- df.trainData$Id

vec.testID <- df.testData$Id

#Call Feautre Engineering Function

df.allData <- PerformFeatureEngineering(df.trainData, df.testData, vec.salePrice)

#Split the data into Train and Test after Feature Engineering

df.trainData <- df.allData[1:nrow(df.trainData),]

df.testData  <- df.allData[(nrow(df.trainData)+1):nrow(df.allData),]

#First Layer

rf.firstLayerModel <- randomForest(vec.salePrice ~ ., data = df.trainData,
                                   ntree = 750, mtry = 15)

vec.predictions <- predict(rf.firstLayerModel, newdata = df.trainData)

df.firstLayerResultSet <- data.frame(Id = vec.trainID, Predicted = vec.predictions,
                                     Real = vec.salePrice, 
                                     Diff = abs(100*((vec.predictions-vec.salePrice)/vec.salePrice)))

vec.idsToRemove <- subset(df.firstLayerResultSet$Id, subset = df.firstLayerResultSet$Diff>50)

vec.rowsToRemove <- unlist(lapply(vec.idsToRemove,function(x) which(x == vec.trainID)))

df.trainData <- df.trainData[-vec.rowsToRemove,]

vec.salePrice <- vec.salePrice[-vec.rowsToRemove]

#Second Layer

rf.secondLayerModel <- randomForest(vec.salePrice ~ ., data = df.trainData,
                                    ntree = 750, mtry = 15)

lm.secondLayerModel <- lm(vec.salePrice ~ ., data = df.trainData)

lm.secondLayerModel <- step(lm.secondLayerModel)

lst.allDatasets <- list(df.trainData,df.testData)

int.index <- 0

for(df.oneDataset in lst.allDatasets){
  
  vec.predictionsRF <- predict(rf.secondLayerModel, newdata = df.oneDataset)
  
  vec.predictionsLM <- predict(lm.secondLayerModel, newdata = df.oneDataset)
  
  vec.predictions <- (vec.predictionsRF + vec.predictionsLM)/2
  
  if(int.index == 0){
    
    print(rmse(log(vec.salePrice), log(vec.predictions)))
    
  }
  
  int.index <- int.index + 1  
    
}

df.resultSet <- data.frame(Id = vec.testID, SalePrice = vec.predictions)

write.csv(df.resultSet, file = paste(kFilePath, kOutputFile, sep = ""), row.names=FALSE)
