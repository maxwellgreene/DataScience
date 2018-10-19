#========== LOAD PACKAGES ==========
loadCrypto <- function()
{
  library(crypto)
  library(tidyverse)
}

#========== GET HISTORY ==========
GetCoinHistory <- function(x)
{
  data <- crypto_history(coin = x);
  data$slug <- NULL
  data$symbol <- NULL
  data$name <- NULL
  data$ranknow <- NULL
  
  coindata <<- data
  
  data <- dplyr::mutate(data,
                        ma2 = rollmeanr(data$open,2,na.pad=TRUE),
                        ma3 = rollmeanr(data$open,3,na.pad=TRUE),
                        ma4 = rollmeanr(data$open,4,na.pad=TRUE))
  
  rollcoindata <<- data
}

#========== GET CURRENT ==========
GetCurrentData <- function(x)
{
  temp <- filter(crypto_prices(), symbol == x);
  temp$id <- NULL
  temp$name <- NULL
  temp$rank <- NULL
  temp <- dplyr::rename(temp,pc1h = percent_change_1h)
  temp <- dplyr::rename(temp,pc24h = percent_change_24h)
  temp <- dplyr::rename(temp,pc7d = percent_change_7d)
  temp <- dplyr::rename(temp,ETH_USD = price_usd)
  temp <- dplyr::rename(temp,ETH_BTC = price_btc)
  
  coindatarn <<- temp
}

#========== MOVING AVERAGE ==========

mutateMovingAvg <- function(x,n=5)
{
  return(filter(x,rep(1/n,n),sides==1))
}

#========== TEMP STUFF ==========

tempfunc <- function(x)
{
  length <- length(x$open)
  
  ggplot(data=x, mapping = aes(x = 1:length,y = open)) + geom_line()
  
  coinopen1 <- x$open[1:length-1]
  coinopen2 <- x$open[2:length]
  
  avgChange <<- mean(coinopen2-coinopen1)*length
}