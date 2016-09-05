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

#Call Feautre Engineering Function

df.allData <- PerformFeatureEngineering(df.trainData,df.testData)

#Split the data into Train and Test after Feature Engineering

df.trainData <- df.allData[1:nrow(df.trainData),]

df.testData  <- df.allData[(nrow(df.trainData)+1):nrow(df.allData),]

#Train the Model

rf.priceModel <- randomForest(vec.salePrice ~ ., data = df.trainData[,-1],
                              importance = T, ntree = 1000)

#Apply the model To Make Predictions

lst.allDatasets <- list(df.trainData,df.testData)

int.index <- 0

for(df.oneDataset in lst.allDatasets){
  
  vec.predictions <- predict(rf.priceModel, newdata = df.oneDataset)
  
  if(int.index == 0){
    
    print(rmse(log(vec.salePrice), log(vec.predictions)))
    
  }
  
  int.index <- int.index + 1  
    
}


df.resultSet <- data.frame(Id = df.testData$Id, SalePrice = vec.predictions)

write.csv(df.resultSet, file = paste(kFilePath, kOutputFile, sep = ""), row.names=FALSE)
