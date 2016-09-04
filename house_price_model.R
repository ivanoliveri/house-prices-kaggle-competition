library(randomForest)

library(Metrics)

PerformFeatureEngineering <- function(df.trainData, df.testData){
  
  df.allData <- rbind(df.trainData[,-length(df.trainData)],df.testData)
  
  vec.naByColumn <- unlist(lapply(df.allData, function(x) 100*(length(is.na(x)[is.na(x)==T])/nrow(df.allData))))
  
  #Remove Columns with over 10% of NA or Categorical Variables
  
  vec.naColumnsToRemove <- names(vec.naByColumn[vec.naByColumn > 10])
  
  vec.naCategoricalColumnsToRemove <- unlist(lapply(names(vec.naByColumn[vec.naByColumn>0]), 
                                                    function(x) {
                                                      if(is.factor(df.allData[,x])==T){
                                                        return(x)  
                                                      }}))
  
  vec.naColumnsToRemove <- unique(c(vec.naColumnsToRemove, vec.naCategoricalColumnsToRemove))
  
  vec.naColumnsToRemovePosition <-  unlist(lapply(vec.naColumnsToRemove, function(x) which(x==names(df.allData))))
  
  df.allData <- df.allData[,-vec.naColumnsToRemovePosition]
  
  #Create LM to imputate NA values
  
  vec.naColumnsToImputate <- setdiff(names(vec.naByColumn[vec.naByColumn>0]),vec.naColumnsToRemove)
  
  for(oneColumnToImputate in vec.naColumnsToImputate){
    
    print(paste("Imputating",oneColumnToImputate,Sys.time(),sep=" "))
    
    vec.columnsToRemove <-names(vec.naByColumn[vec.naByColumn > 0])
    
    vec.columnsToRemove <- setdiff(vec.columnsToRemove,oneColumnToImputate)
    
    vec.columnsToRemovePosition <- unlist(lapply(vec.columnsToRemove, 
                                                 function(x) which(x==names(df.allData))))
    
    df.currentTrainset <- subset(df.allData[,-vec.columnsToRemovePosition],
                                 subset =is.na(df.allData[,oneColumnToImputate])==F)
    
    df.currentTestset <- subset(df.allData[,-vec.columnsToRemovePosition],
                                subset =is.na(df.allData[,oneColumnToImputate])==T)
    
    int.responseVariablePosition <- which(names(df.currentTestset) == oneColumnToImputate)
    
    vec.responseVariable <- df.currentTrainset[,oneColumnToImputate]
    
    lm.imputateNA <- lm(vec.responseVariable ~ ., 
                        data = df.currentTrainset[,-c(1,int.responseVariablePosition)])
    
    lm.imputateNAWithStepwise <- step(lm.imputateNA)
    
    vec.predictions <- predict(lm.imputateNAWithStepwise, newdata = df.currentTestset)
    
    df.allData[,oneColumnToImputate] <- replace(df.allData[,oneColumnToImputate],
                                                is.na(df.allData[,oneColumnToImputate]),
                                                vec.predictions)
    
  }
  
  #Create Dummy Variables for Categorical Variables that Have Upto 5 Levels
  
  vec.dummyVariables <- unlist(lapply(names(df.allData), function(x) {
                                                            if(is.factor(df.allData[,x]) == T){
                                                              
                                                              if(length(unique(df.allData[,x])) <= 5){
                                                               
                                                                return(x)
                                                                 
                                                              }
                                                              
                                                            }
                                                          }))
  
  for(oneCategoricalVariable in vec.dummyVariables){
    
    vec.categoricalColumn <- df.allData[,oneCategoricalVariable]
    
    mat.current <- model.matrix( ~ vec.categoricalColumn - 1)  
    
    df.dummyDataset <- data.frame(mat.current)
    
    df.dummyDataset[,1:length(df.dummyDataset)] <- lapply(df.dummyDataset[,1:length(df.dummyDataset)],
                                                          function(x) as.factor(x))
    
    int.currentPosition <- which(oneCategoricalVariable==names(df.allData))
    
    df.allData <- df.allData[,-int.currentPosition]
    
    df.allData <- data.frame(df.allData, mat.current)
    
  }
  
  #Check if there's a column with 0 variance
  
  return(df.allData)
  
}

kFilePath <- "C:/Users/Ivan/Documents/GitHub/house-prices-kaggle-competition/"

kTrainFile <- "train.csv"

kTestFile <- "test.csv"

kOutputFile <- "predictions.csv"

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
