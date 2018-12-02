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
  count <- c(0,0,0)
  for(i in 1:forNum)
  {
    data_train <- sample_n(data1,numTrain)
    data_test <- sample_n(data1,numTest)
    
    data_glm_temp <- glm(data=data_train, openNext ~ open+openRatioNext*close)
    
    prediction <- predict(data_glm_temp,newdata = data_test,type="response")
    
    table_10 <- table((prediction/data_test$openNext)>0.9 & 
                     (prediction/data_test$openNext)<1.1)
    table_05 <- table((prediction/data_test$openNext)>0.95 & 
                        (prediction/data_test$openNext)<1.05)
    table_01 <- table((prediction/data_test$openNext)>0.99 & 
                        (prediction/data_test$openNext)<1.01)
    
    count[1] <- count[1] + as.numeric(sum(table_10[1])/(sum(table_10))*100)
    count[2] <- count[2] + as.numeric(sum(table_05[1])/(sum(table_05))*100)
    count[3] <- count[3] + as.numeric(sum(table_01[1])/(sum(table_01))*100)
    
  }
  return(count[1]/forNum)
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
  
  persp3D(z=performance,theta=65)
  return(performance)
  
}
















