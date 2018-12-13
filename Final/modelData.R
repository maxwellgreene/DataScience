formulaListGLM <- function(formchar)
{
  percents <- seq(.1,10,.05)
  
  resultsdf <- data.frame(percents=NA,formula=NA,results=NA)
  
  for(i in 1:length(formchar))
  {
    result <- as.vector(
      glmVect(1000,200,percents,as.formula(formchar[i]),forNum=50,data=dataRatio))
    
    resultsdf <- rbind(resultsdf,
                       data.frame(percents=percents,
                                  formula=rep(formchar[i],length(percents)),
                                  results=result))
  }
  return(resultsdf)
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
    valuevect <- c(valuevect,data_test$highNext)
    
    for(i in 1:length(percents))
    {
      temp <- table((prediction/data_test$highNext)>(1-percents[i]/100) &
                          (prediction/data_test$highNext)<(1+percents[i]/100))
      count[i] <- count[i] + as.numeric(sum(temp[length(temp)])/(sum(temp))*100)
    }
  }
  myplot <- ggplot(data=data.frame(result=as.vector(count/forNum),value=tempvect)) + 
    geom_point(mapping = aes(x=value,y=result)) + ggtitle(formtitle)
  ggsave(filename = paste(filename,".png"),plot=myplot)
}



glmVect <- function(numTrain,numTest,percents,formulachar,forNum=100,data=dataRatio)
{
  count <- matrix(0,1,length(percents))
  predictvect <- NULL
  valuevect <- NULL
  for(i in 1:forNum)
  {
    data_train <- sample_n(data,numTrain)
    data_test <- sample_n(data,numTest)
    
    formula <- as.formula(formulachar)
    formtitle <- paste("Formula: ",format(formula))
    
    glm_temp <- glm(data=data_train, formula = formula)
    
    prediction <- predict(glm_temp,newdata = data_test,type="response")
    
    predictvect <- c(predictvect,prediction)
    valuevect <- c(valuevect,data_test$closeNext)
    
    for(i in 1:length(percents))
    {
      temp <- table((prediction/data_test$closeNext)>(1-percents[i]/100) &
                      (prediction/data_test$closeNext)<(1+percents[i]/100))
      count[i] <- count[i] + as.numeric(sum(temp[length(temp)])/(sum(temp))*100)
    }
  }
  return(count/forNum)
}

#==========================================
#========== NO LONGER FUNCTIONAL ==========
#==========================================

#This funciton used to present a graphical representation of the accuracy 
#of a linear model against the paremeters of the number of 
#training and test sets used to fit the model.
#The output was a 3D plot where the x and y axes were number of training 
#sets and number of tests, and the z was the performance of the model 
#given those two sets.
#Not surprisingly, the model accuracy increased until a certain point
#then levelled off with respect to both training and test set number.

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
      performance[i-numTrainMin+1,j-numTestMin+1] <- glmVect(i,j)
    }
  }
  
  persp3D(z=performance,theta=65)
}