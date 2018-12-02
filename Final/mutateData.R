#========== MOVING AVERAGE ==========
mutateRoll <- function(data,column,nums)
{
  for(i in 1:length(nums))
  {
    varname = paste("ma",i,sep="")
    data <- mutate(data,!!varname := rollmean(column,nums[i],na.pad=TRUE))
  }
  return(data)
}

#========== Yesterday ==========
mutateLast <- function(data,column,varname)
{
  #This function adds a column to the dataset that is similar to the input column where inputCol[i]=outputCol[i-1], 
  #which corrosponds to the "next day" (next row) value of that column.
  return(mutate(data, !!varname := c(NA,head(column,-1))))
}

mutateLastRatio <- function(data,denomCol,numerCol,varname)
{
  if(length(denomCol) != length(numerCol))# | length(denomCol != length(data[,1])))
  {stop("Denominator and Numerator are not the same length.")}
  quotient <- denomCol/numerCol
  return(mutate(data, !!varname := c(NA,head(quotient,-1))))
}

#========== Tomorrow ==========
mutateNext <- function(data,column,varname)
{
  #This function adds a column to the dataset that is similar nput column where putCol[i]=outputCol[i+1], 
  #which corrosponds to the "next day" (next row) value of that column.
  return(mutate(data, !!varname := c(column[-1],column[1])))
}

mutateNextRatio <- function(data,denomCol,numerCol,varname)
{
  #This function adds a column to the dataset that is similar to the input column where inputCol[i]=outputCol[i+1], 
  #which corrosponds to the "next day" (next row) value of that column.
  if(length(denomCol) != length(numercol))# | length(denomCol != length(data[1,])))
    {stop("Denominator and Numerator are not the same length.")}
  quotient <- denomCol/numerCol
  return(mutate(data, !!varname := c(quotient[-1],quotient[1])))
}

#========== Convert df to Ratio ==========
convertRatio <- function(data, column, colnames)
{
  colnums <- which(colnames(data) %in% colnames)
  for(i in colnums)
  {
      try(data[i] <- data[i]/column)
  }
  return(data)
}





