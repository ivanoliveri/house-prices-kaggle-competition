library(randomForest)

library(Boruta)

library(caret)

library(plyr)

PerformFeatureEngineering <- function(df.trainData, df.testData, vec.salePrice){
  
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
    
    rf.imputateNA <- randomForest(vec.responseVariable ~ ., 
                        data = df.currentTrainset[,-c(1,int.responseVariablePosition)],
                        importance = T, ntree = 1000)
  
    
    vec.predictions <- predict(rf.imputateNA, newdata = df.currentTestset)
    
    df.allData[,oneColumnToImputate] <- replace(df.allData[,oneColumnToImputate],
                                                is.na(df.allData[,oneColumnToImputate]),
                                                vec.predictions)
    
  }
  
  #Remove Near To Zero Variance Columns
  
  vec.nearToZeroVarPositions <- nearZeroVar(df.allData)
  
  df.allData <- df.allData[,-vec.nearToZeroVarPositions]
  
  #Create Dummy Variables for Categorical Variables that Have Upto 5 Levels
  
  vec.dummyVariables <- unlist(lapply(names(df.allData), function(x) {
    if(is.factor(df.allData[,x]) == T){
      
      if(length(levels(df.allData[,x])) <= 5){
        
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
    
    df.allData <- data.frame(df.allData, df.dummyDataset)
    
  }
  
  df.allData <- PerformCustomizedFeatureEngineering(df.allData)
    
  #Perform Bourta Algorithm for Feature Selection
  
  df.partialTrainData <- df.allData[1:nrow(df.trainData),]
  
  bor.results <- Boruta(df.partialTrainData, vec.salePrice,
                        maxRuns=100, doTrace=0)
  
  #Only Keep Non Rejected Columns
  
  vec.nonRejectedColumns <- 
          names(bor.results$finalDecision[bor.results$finalDecision=="Confirmed"])
  
  vec.nonRejectedColumnsPositions <-
          unlist(lapply(vec.nonRejectedColumns,
                        function(x) which(x==names(df.allData))))
  
  df.allData <- df.allData[,vec.nonRejectedColumnsPositions]
  
  return(df.allData)
  
}

PerformCustomizedFeatureEngineering <- function(df.allData){
  
  vec.twoFloorsIndicator <- as.factor(ifelse(df.allData$X1stFlrSF > 0 
                                     & df.allData$X2ndFlrSF>0, 1, 0))
  
  df.allData <- data.frame(df.allData, vec.twoFloorsIndicator)
  
  df.allData$BsmtFullBath <- round(df.allData$BsmtFullBath,0)
  
  df.allData$BsmtHalfBath <- round(df.allData$BsmtHalfBath,0)
  
  df.allData$GarageCars <- round(df.allData$GarageCars,0)
  
  #Assign Neightboor Cluster
  
  df.clusterData <- data.frame(salePrice = vec.salePrice,
                               neighborhood=df.trainData$Neighborhood)
  
  df.clusterDataSummarized <- ddply(df.clusterData,~neighborhood,
                                    summarise,mean=mean(salePrice),
                                    max=max(salePrice),
                                    min=min(salePrice))
  
  cls.neighboors <- kmeans(df.clusterDataSummarized[,2:4],4)
  
  df.resultClusters <- data.frame(df.clusterDataSummarized$neighborhood,
                                  cluster = cls.neighboors$cluster,
                                  mean = df.clusterDataSummarized$mean)
  
  vec.neighboorCluster <- as.factor(unlist(lapply(df.allData$Neighborhood, 
                                 function(x) return(subset(df.resultClusters,
                                              subset = df.resultClusters$df.clusterDataSummarized.neighborhood==x)[2]))))
  
  df.allData = data.frame(df.allData,neighboorCluster = vec.neighboorCluster)
  
  return(df.allData)
  
}