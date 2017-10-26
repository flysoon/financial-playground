library(dplyr)
library(rvest)
library(quantmod)

valuation <- function(symbol) {
  symbol <- toupper(symbol)
  price <-as.data.frame(getSymbols(Symbols = symbol, 
                                   src = "google", 
                                   from = "2005-01-01",
                                   to = "2018-01-01",
                                   env = NULL))
  price$date <- rownames(price)
  price$date <- as.Date(price$date,'%Y-%m-%d')
  price <- price[,c(6,4)]
  names(price) <- c('date','price')
  
  url <- paste0('https://ycharts.com/companies/',symbol,'/eps')
  eps <- read_html(url)
  df <- data.frame(raw = as.character(eps %>%
                                        html_nodes("td") %>%
                                        html_text() ))
  
  df <- data.frame(raw=gsub('\n| ','',df$raw))
  grepl("[0-9]",df[99,]) == 1
  
  for (i in 1:nrow(df)) {
    if (grepl("[0-9]",df[i,]) == 1) {
      n_df <- i
    } else break
  }
  
  df <- head(df,n_df)
  
  index1 <- (1:(n_df/2))*2 - 1
  index2 <- (1:(n_df/2))*2
  
  df1 <- data.frame(date = as.character(df$raw[index1]),
                    eps = as.numeric(as.character(df$raw[index2])))
  df1$date <- gsub('\\.','',df1$date)
  df1$date <- gsub('Sept','Sep',df1$date)
  df1$date <- as.Date(df1$date,'%b%d,%Y')
  df1 <- df1[order(df1$date),]
  
  par(mfrow=c(1,3))
  plot(df1$eps~df1$date,type='b',xlim=c(as.Date('2005-12-31'),as.Date('2017-09-30')),ylim=c(-.5,max(df1$eps)))
  plot(price$price~price$date,type='l',xlim=c(as.Date('2005-12-31'),as.Date('2017-09-30')))
  
  price_m <- price %>% group_by(month = substring(date,1,7)) %>% summarise(price_avg = mean(price))
  df1$month <- substring(df1$date,1,7)
  price_m <- left_join(price_m,df1)
  price_m$pe <- price_m$price_avg/price_m$eps/4
  price_m <- price_m[!is.na(price_m$eps),]
  plot(price_m$pe~price_m$date, type='b',xlim=c(as.Date('2005-12-31'),as.Date('2017-09-30')),ylim=c(-20,min(120,max(price_m$pe))))  
}

valuation('cmg')