#========== PLOT DATA ==========
plotOpen <- function(x)
{
  x <- mutateRoll(x,x$open,c(10,50))
  ggplot(data = filter(x, date > '2017-01-01')) + 
    geom_line(mapping = aes(x = date, y = open)) + 
    geom_line(mapping = aes(x = date, y = ma1, color = "red")) + 
    geom_line(mapping = aes(x = date, y = ma2, color = "green")) + 
    ggtitle("Ethereum Open Price w/ 10 & 50 Day Moving Avg.") + 
    xlab("Date, 1/1/17 to Present") + 
    ylab("Open Price, USD")
}
plotClose <- function(x)
{
  x <- mutateRoll(x,x$close,c(10,50))
  ggplot(data = filter(x, date > '2017-01-01')) + 
    geom_line(mapping = aes(x = date, y = close)) + 
    geom_line(mapping = aes(x = date, y = ma1, color = "red")) + 
    geom_line(mapping = aes(x = date, y = ma2, color = "green")) + 
    ggtitle("Ethereum Close Price w/ 10 & 50 Day Moving Avg.") + 
    xlab("Date, 1/1/17 to Present") + 
    ylab("Close Price, USD")
}
plotHigh <- function(x)
{
  x <- mutateRoll(x,x$high,c(10,50))
  ggplot(data = filter(x, date > '2017-01-01')) + 
    geom_line(mapping = aes(x = date, y = high)) + 
    geom_line(mapping = aes(x = date, y = ma1, color = "red")) + 
    geom_line(mapping = aes(x = date, y = ma2, color = "green")) + 
    ggtitle("Ethereum High Price w/ 10 & 50 Day Moving Avg.") + 
    xlab("Date, 1/1/17 to Present") + 
    ylab("High Price, USD")
}
plotLow <- function(x)
{
  x <- mutateRoll(x,x$low,c(10,50))
  ggplot(data = filter(x, date > '2017-01-01')) + 
    geom_line(mapping = aes(x = date, y = low)) + 
    geom_line(mapping = aes(x = date, y = ma1, color = "red")) + 
    geom_line(mapping = aes(x = date, y = ma2, color = "green")) + 
    ggtitle("Ethereum Low Price w/ 10 & 50 Day Moving Avg.") + 
    xlab("Date, 1/1/17 to Present") + 
    ylab("Low Price, USD")
}