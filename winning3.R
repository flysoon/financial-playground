library(TTR) # to get yahoo data
library(xts) # to transfrom time series to data frame
library('scales') # for percentage format
library(ggplot2)

get_ticker<-function(tickers){
  raw_data <- paste0('name\n',tickers)
  raw_data<-textConnection((raw_data))
  list <- read.table(raw_data,header=TRUE)
  list1 <- list['name']
  list1 <- unique(list1)
  return(list1)
}

list1 <- get_ticker('qqq')
list1

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

streak <- function(tick,start=19900101,end=20171231){
  stock <- getYahooData(tick, start, end)
  stock <-data.frame(date=index(stock), coredata(stock))
  stock$Unadj.Close<-NULL
  stock$Div<-NULL
  stock$Split<-NULL
  stock$Adj.Div<-NULL
  stock$Vol_MA5<-SMA(stock$Volume,n=5)
  stock$Vol_Perc<-stock$Volume/stock$Vol_MA5
  stock$MA200<-SMA(stock$Close,n=min(200,nrow(stock)-10))
  stock$MA200_d<- stock$Close - stock$MA200
  stock$MA200_dperc<- stock$MA200_d/stock$MA200
  stock$MA50<-SMA(stock$Close,n=min(50,nrow(stock)-10))
  stock$MA50_d<- stock$Close - stock$MA50
  stock$MA50_dperc<- stock$MA50_d/stock$MA50
  stock$ema9<-EMA(stock$Close,n=9)
  stock$ema9_dperc<- stock$Close/stock$ema9-1
  stock$ema21<-EMA(stock$Close,n=21)
  stock$ema21_dperc<- stock$Close/stock$ema21-1
  stock$Target<- (stock$Close+stock$MA200)/2
  stock$RSI<-RSI(stock$Close)
  stock$dec <- 0
  stock$inc <- 0
  stock$inc_perc <- 0
  stock$Change <- 0
  for (i in 1:(nrow(stock)-1)) {
    stock$Change[i+1] <- stock$Close[i+1]/stock$Close[i]-1
    stock$inc_perc[i+1] <- stock$inc_perc[i] + stock$Close[i+1]/stock$Close[i]-1
    if (stock$Close[i+1]<stock$Close[i]) {
      stock$dec[i+1] <- stock$dec[i]+1
    }
    if (stock$Close[i+1]>stock$Close[i]) {
      stock$inc[i+1] <- stock$inc[i]+1
    }
  }
  for (i in 1:(nrow(stock))) {
    if (stock$inc[i]==0) {
      stock$inc[i] <- -stock$dec[i]
    }
  }
  stock$ema9_above <- 0
  for (i in 1:(nrow(stock)-1)) {
    if (is.na(stock$ema9_dperc[i])==0 & stock$ema9_dperc[i+1]>=0 & stock$ema9_dperc[i]>=0) {
      stock$ema9_above[i+1] <- stock$ema9_above[i]+1
    }
    if (is.na(stock$ema9_dperc[i])==0 & stock$ema9_dperc[i+1]<0 & stock$ema9_dperc[i]<0) {
      stock$ema9_above[i+1] <- stock$ema9_above[i]-1
    }
  } 
  stock$ema21_above <- 0
  for (i in 1:(nrow(stock)-1)) {
    if (is.na(stock$ema21_dperc[i])==0 & stock$ema21_dperc[i+1]>=0 & stock$ema21_dperc[i]>=0) {
      stock$ema21_above[i+1] <- stock$ema21_above[i]+1
    }
    if (is.na(stock$ema21_dperc[i])==0 & stock$ema21_dperc[i+1]<0 & stock$ema21_dperc[i]<0) {
      stock$ema21_above[i+1] <- stock$ema21_above[i]-1
    }
  } 
  stock$ema9_above_ema21 <- 0
  for (i in 1:(nrow(stock)-1)) {
    if (is.na(stock$ema21_dperc[i])==0 & stock$ema9[i+1]>=stock$ema21[i+1] & stock$ema9[i]>=stock$ema21[i]) {
      stock$ema9_above_ema21[i+1] <- stock$ema9_above_ema21[i]+1
    }
    if (is.na(stock$ema21_dperc[i])==0 & stock$ema9[i+1]<stock$ema21[i+1] & stock$ema9[i]<stock$ema21[i]) {
      stock$ema9_above_ema21[i+1] <- stock$ema9_above_ema21[i]-1
    }
  } 
  stock$ticker <- toupper(tick)
  return(stock)
}

golden_cross <- function(tick,...) {
  table1 <- streak(tick,...)
  
  p1 <- qplot(table1$date, table1$ema9_above_ema21, geom ='line',xlab='ema9 ema21 cross')
  p2 <- qplot(table1$date, log(table1$Close), geom ='line',xlab=tick)
  p3 <- qplot(table1$date, table1$ema9_above, geom ='line',xlab='above ema9')
  p4 <- qplot(table1$date, table1$ema21_above, geom ='line',xlab='above ema21')
  
  multiplot(p1, p2, p3, p4, cols = 2)
  return(table1)
}

table1 <- golden_cross('qqq',19800101)

table1$retrace <- 0
for (i in 2001:(nrow(table1)-1)) {
  print(i)
  for (j in 2000:i) {
    if ((table1$Close[i] / table1$Close[j] <= 0.97) & (i-j <= 200)) {table1$retrace[i] <- 1}
  }
}

p1 <- qplot(tail(table1$date,2000), tail(table1$inc_perc,2000), geom ='line')
p2 <- qplot(tail(table1$date,2000), tail(table1$retrace,2000), geom ='line')

multiplot(p1, p2, cols = 1)
