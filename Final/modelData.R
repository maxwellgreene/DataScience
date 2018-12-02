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

#Arguments:
#NumTrain -> number of objects to use in training set
#NumTest  -> number of objects to use in testing set
#percents -> vector of numbers with which to check prediction accuracy 
#formulachar -> formula object used to generate model
#filename -> character object for name of ggplot file save
#forNum   -> main loop number, new data sets and models each loop, default 100
#data1    -> data obtained from EDA
glmPlot <- function(numTrain,numTest,percents,formulachar,filename,forNum=100,data1=data)
{
  count <- matrix(0,1,length(percents))
  predictvect <- NULL
  valuevect <- NULL
  for(i in 1:forNum)
  {
    data_train <- sample_n(data1,numTrain)
    data_test <- sample_n(data1,numTest)
    
    formula <- as.formula(formulachar)
    
    data_glm_temp <- glm(data=data_train, formula = formula)
    
    prediction <- predict(data_glm_temp,newdata = data_test,type="response")
    
    predictvect <- c(predictvect,prediction)
    valuevect <- c(valuevect,data_test$openNext)
    
    for(i in 1:length(percents))
    {
      temp <- table((prediction/data_test$openNext)>(1-percents[i]/100) &
                          (prediction/data_test$openNext)<(1+percents[i]/100))
      count[i] <- count[i] + as.numeric(sum(temp[length(temp)])/(sum(temp))*100)
    }
  }
  myplot <- ggplot(data=data.frame(result=as.vector(count/forNum),value=tempvect)) + 
    geom_point(mapping = aes(x=value,y=result)) + ggtitle(formtitle)
  ggsave(filename = paste(filename,".png"),plot=myplot)
  #return(count/forNum)
}

glmVect <- function(numTrain,numTest,percents,formulachar,forNum=100,data1=data)
{
  count <- matrix(0,1,length(percents))
  predictvect <- NULL
  valuevect <- NULL
  for(i in 1:forNum)
  {
    data_train <- sample_n(data1,numTrain)
    data_test <- sample_n(data1,numTest)
    
    formula <- as.formula(formulachar)
    formtitle <- paste("Formula: ",format(formula))
    
    data_glm_temp <- glm(data=data_train, formula = formula)
    
    prediction <- predict(data_glm_temp,newdata = data_test,type="response")
    
    predictvect <- c(predictvect,prediction)
    valuevect <- c(valuevect,data_test$openNext)
    
    for(i in 1:length(percents))
    {
      temp <- table((prediction/data_test$openNext)>(1-percents[i]/100) &
                      (prediction/data_test$openNext)<(1+percents[i]/100))
      count[i] <- count[i] + as.numeric(sum(temp[length(temp)])/(sum(temp))*100)
    }
  }
  #myplot <- ggplot(data=data.frame(result=as.vector(count/forNum),value=tempvect)) + 
  #  geom_point(mapping = aes(x=value,y=result)) + ggtitle(formtitle)
  #ggsave(filename = paste(filename,".png"),plot=myplot)
  return(count/forNum)
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