#========== CREATE TRAINING SET ==========
trainingSet <- function(data,startDate,endDate)
{
  training <- filter(data,date >= startDate & date <= endDate)
  return(training)
}

#========== CREATE TEST SET ==========
testSet <- function(data,startDate,endDate)
{
  test <- filter(data,date >= startDate & date <= endDate)
  return(test)
}

#========== FIT LM ==========
fitLM <- function(data, formula)
{
  datalm <- lm(data,formula)
  return
}

#========== PREDICT ==========
predictLM <- function()
{
  
}

#========== AVERAGE GLM PERFORMANCE ==========
avgGLM <- function(numTrain,numTest,forNum=100,data1=data)
{
  count <- as.numeric(0)
  for(i in 1:forNum)
  {
    data_train <- sample_n(data1,numTrain)
    data_test <- sample_n(data1,numTest)
    
    data_glm_temp <- glm(data=data_train, openNext ~ open+openRatioNext*close)
    
    prediction <- predict(data_glm_temp,newdata = data_test,type="response")
    
    table <- table((prediction/data_test$openNext)>0.90 & 
                     (prediction/data_test$openNext)<1.1)
    
    count <- count + as.numeric(sum(table[1])/(sum(table))*100)
  }
  return(100-count/forNum)
}




avgGLMPlot <- function(numTrainMin,numTrainMax,numTestMin,numTestMax)
{
  train <- numTrainMin:numTrainMax
  test <- numTestMin:numTestMax
  performance <- matrix(1:(length(train)*length(test)))
  dim(performance) <- c(length(train),length(test))
  for(i in train)
  {
    for(j in test)
    {
      performance[i-numTrainMin+1,j-numTestMin+1] <- avgGLM(i,j)
    }
  }
  
  ggplot(as.data.frame(performance), mapping = aes(x = train,y=V2)) 
  + geom_point()
  
  return(performance)
  
}
















