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
  return(mutate(data, !!varname := c(NA,head(column,-1))))
}

#========== Tomorrow ==========
mutateNext <- function(data,column,varname)
{
  return(mutate(data, !!varname := c(column[-1],column[1])))
}