#========== LOAD PACKAGES ==========
loadCrypto <- function()
{
  library(crypto)
  library(tidyverse)
}

#========== GET HISTORY ==========
GetCoinHistory <- function(x)
{
  #This function is meant to return slightly neater version 
  #of the dataset returned from the crypto_history() function
  #in the 'crypto' package
  #Input: a character of the desired coin to return
  #NOTE* specifying coin here is the data used for the whole project
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
  #This funciton is actually unused in my project
  #I decided to omit my section on currect 
  #price predictions for this project
  
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