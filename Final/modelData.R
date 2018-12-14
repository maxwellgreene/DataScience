#========== AVERAGE GLM PERFORMANCE LIST OF FORMULAS ==========

formulaListGLM <- function(formchar)
{
  #This function takes an input of a vector of 
  #character strings in the form of formulas, and 
  #returns a dataframe of the results from running 
  #the function glmVect() on these formulas. These
  #results are used to plot many of the 'percent vs. percent'
  #plots that I have in my visualizing data section.
  percents <- seq(.1,10,.05)
    #set vector of percents to evaluate in glmPlot
  
  resultsdf <- data.frame(percents=NA,formula=NA,results=NA)
    #define dataframe structure for results
  
  for(i in 1:length(formchar))
  {
    result <- as.vector(
      glmVect(1000,200,percents,as.formula(formchar[i]),forNum=50,data=dataRatio))
      #get results from glmPlot with the formula at formula[i]
    
    resultsdf <- rbind(resultsdf,
                       data.frame(percents=percents,
                                  formula=rep(formchar[i],length(percents)),
                                  results=result))
      #add the results vector to a dataframe with the indicated percent
      #value from the percents vector, the corospondign vale returned from
      #glmPlot, and the formula used for each one.
  }
  return(resultsdf)#return dataframe
}

#========== AVERAGE GLM PERFORMANCE PLOT==========
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
    #set format for vector counting number 
    #of predictions within certain percentage
  predictvect <- NULL
  valuevect <- NULL
  for(i in 1:forNum)
  {
    data_train <- sample_n(data1,numTrain)
    data_test <- sample_n(data1,numTest)
      #Create train and test datasets (new set for each loop)
    
    formula <- as.formula(formulachar)
      #convert formula character to formula object
    
    data_glm_temp <- glm(data=data_train, formula = formula)
      #create glm with the train set from this current loop
    
    prediction <- predict(data_glm_temp,newdata = data_test,type="response")
      #give the value of the prediction on the test set to a vector
      
    predictvect <- c(predictvect,prediction)
    valuevect <- c(valuevect,data_test$highNext)
      #(I can't remember why I did this)
    
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

#========== AVERAGE GLM PERFORMANCE RETURNS VECTOR==========
#Arguments:
#NumTrain -> number of objects to use in training set
#NumTest  -> number of objects to use in testing set
#percents -> vector of numbers with which to check prediction accuracy 
#formulachar -> formula object used to generate model
#forNum   -> main loop number, new data sets and models each loop, default 100
#data    -> data obtained from EDA
glmVect <- function(numTrain,numTest,percents,formulachar,forNum=100,data=dataRatio)
{
  count <- matrix(0,1,length(percents))
  predictvect <- NULL
  valuevect <- NULL
  for(i in 1:forNum)
  {
    data_train <- sample_n(data,numTrain)
    data_test <- sample_n(data,numTest)
      #Create train and test datasets
  
    formula <- as.formula(formulachar)
      #convert character to formula object
    
    glm_temp <- glm(data=data_train, formula = formula)
      #create glm out of the current train set 
    
    prediction <- predict(glm_temp,newdata = data_test,type="response")
      #predict using the glm on the test set
      
    predictvect <- c(predictvect,prediction)
    valuevect <- c(valuevect,data_test$closeNext)
    
    for(i in 1:length(percents))
    {
      temp <- table((prediction/data_test$closeNext)>(1-percents[i]/100) &
                      (prediction/data_test$closeNext)<(1+percents[i]/100))
        #create a table for the number of predictions that were within 
        #percents[i] of the correct value in test set
      count[i] <- count[i] + as.numeric(sum(temp[length(temp)])/(sum(temp))*100)
        #Keep track of the count[i] so that it can be averged at the end.
        #The count number is effectively the way of 
        #averaging all of the models at the end.
        #So, the returned value is really:
          #A vector of the average percent of predictions 
          #within the corrosponding percent of the true value
          #averaged over the number of models given by forNum
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