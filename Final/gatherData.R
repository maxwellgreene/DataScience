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
  return(data)
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


#========== TEMP STUFF ==========

tempfunc <- function(x)
{
  length <- length(x$open)
  
  ggplot(data=x, mapping = aes(x = 1:length,y = open)) + geom_line()
  
  coinopen1 <- x$open[1:length-1]
  coinopen2 <- x$open[2:length]
  
  avgChange <<- mean(coinopen2-coinopen1)*length
}