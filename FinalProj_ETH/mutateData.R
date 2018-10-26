#========== MOVING AVERAGE ==========
mutateRoll <- function(x,y,nums)
{
  for(i in 1:length(nums))
  {
    varname = paste("ma",i,sep="")
    x <- mutate(x,!!varname := rollmean(y,nums[i],na.pad=TRUE))
  }
  return(x)
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





