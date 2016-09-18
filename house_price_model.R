#Load Libraries

library(randomForest)

library(Metrics)

library(MCMCpack)

library(nnet)

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

vec.idsToRemove <- subset(df.firstLayerResultSet$Id, subset = df.firstLayerResultSet$Diff>45)

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
  
  vec.partialPredictions <- (vec.predictionsRF + vec.predictionsLM)/2
  
  if(int.index == 0){
    
    #Third Layer
    
    df.trainData <- data.frame(df.trainData,vec.partialPredictions)
    
    rf.thirdLayerModel <- randomForest(vec.salePrice ~ ., data = df.trainData,
                                        ntree = 750, mtry = 15)
    
    lm.thirdLayerModel <- lm(vec.salePrice ~ ., data = df.trainData)
    
    lm.thirdLayerModel <- step(lm.secondLayerModel)
    
    nnet.thirdLayerModel <- nnet(vec.salePrice ~ ., data = df.trainData,
                                 size=10, linout=TRUE, skip=TRUE, MaxNWts=10000, 
                                 trace=FALSE, maxit=100)

    vec.partiallPredictionsRF <- predict(rf.thirdLayerModel, newdata = df.trainData)
    
    vec.partialPredictionsLM <- predict(lm.thirdLayerModel, newdata = df.trainData)
    
    vec.partialPredictionsNNET <- predict(nnet.thirdLayerModel, newdata = df.trainData)
    
    vec.partialPredictions <- (vec.partialPredictionsRF + vec.partialPredictionsLM +
                               vec.partialPredictionsNNET) / 
      
    df.trainData <- df.trainData[,-length(df.trainData)]
      
    df.trainData <- data.frame(df.trainData, vec.partialPredictions)
      
    xgb.fourthLayerModel
      
    print(rmse(log(vec.salePrice), log(vec.finalPredictions)))
    
  }
  
  vec.partialPredictionsRF <- predict(rf.thirdLayerModel, newdata = df.oneDataset)
  
  vec.partialPredictionsLM <- predict(lm.thirdLayerModel, newdata = df.oneDataset)
  
  vec.partialPredictionsNNET <- predict(nnet.thirdLayerModel, newdata = df.oneDataset)
  
  vec.partialPredictions <- (vec.partialPredictionsRF + vec.partialPredictionsLM +
                             vec.partialPredictionsNNET) / 3
  
  int.index <- int.index + 1  
    
}

df.resultSet <- data.frame(Id = vec.testID, SalePrice = vec.finalPredictions)

write.csv(df.resultSet, file = paste(kFilePath, kOutputFile, sep = ""), row.names=FALSE)
