#test
library(TTR)
library(ggplot2)
library(xts)
library(dygraphs)

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


winning <- function(ticker){
  
  # Get IBM and Linkedin stock data from Yahoo Finance
  
  
  aapl_url <- paste0("http://real-chart.finance.yahoo.com/table.csv?s=",ticker,"&a=07&b=24&c=2010&d=07&e=24&f=2016&g=d&ignore=.csv")
  
  #aapl_url <- "http://real-chart.finance.yahoo.com/table.csv?s=baba&a=07&b=24&c=2010&d=07&e=24&f=2016&g=d&ignore=.csv"
  
  yahoo.read <- function(url){
    dat <- read.table(url,header=TRUE,sep=",")
    df <- dat[,c(1,7)]
    names(df) <- c('Date','Close')
    df$Date <- as.Date(as.character(df$Date))
    return(df)}
  
  aapl <- yahoo.read(aapl_url)
  # aapl$Close[aapl$Date<'2014-6-9'] <-   aapl$Close[aapl$Date<'2014-6-9']/7
  aapl <- aapl[order(aapl$Date),]
  aapl$rsi <- RSI(aapl$Close)
  sort(aapl$rsi)
  
  # ggplot(ibm,aes(Date,Close)) + 
  #   geom_line(aes(color="ibm")) +
  #   geom_line(data=lnkd,aes(color="lnkd")) +
  #   labs(color="Legend") +
  #   scale_colour_manual("", breaks = c("ibm", "lnkd"),
  #                       values = c("blue", "brown")) +
  #   ggtitle("Closing Stock Prices: IBM & Linkedin") + 
  #   theme(plot.title = element_text(lineheight=.7, face="bold"))
  
  p1 <- ggplot(aapl,aes(Date,Close)) + geom_line(aes(color=ticker)) + geom_line(col='red') 
  
  p2 <- ggplot(aapl,aes(Date,rsi)) + geom_line(aes(color=ticker)) + geom_line(col='black')
  multiplot(p1, p2)
}
# 
# S&P 500 Index	-0.51%
# Consumer Discretionary (XLY)		+0.51%
# Consumer Staples (XLP)	-0.21%s
# Energy (XLE)	-0.16%
# Financial Services (XLFS)	-0.17%
# Financials (XLF)	-0.55%
# Health Care (XLV)	-1.56%
# Industrials (XLI)	-0.58%
# Materials (XLB)	-0.59%
# Real Estate (XLRE)	-0.95%
# Technology (XLK)	-0.87%
# Utilities (XLU)		+0.60%


winning('xly')
winning('xlp')
winning('xle')
winning('xlf')
winning('xlv')
winning('xli')
winning('xlb')
winning('xlk')
winning('xlu')


winning('fb')
winning('lnkd')
winning('amzn')
winning('googl')
winning('nflx')
winning('ibb')
winning('gild')

winning('baba')
winning('bidu')
winning('jd')
winning('ctrp')
winning('sina')
winning('sohu')
winning('wb')
winning('edu')
winning('wuba')
winning('tcehy')
winning('ashr')
winning('fxi')

winning('aal')
winning('jblu')


winning('gpro')
winning('fit')
winning('nvda')
winning('cmg')
winning('intc')
winning('amd')
winning('grpn')
winning('mnst')
winning('mww')

winning('ebay')
winning('v')
winning('pypl')
winning('w')

winning('shop')

winning('qiwi')

# get the maximual continuous decrease/increase days
streak <- function(tick,start=20110101,end=20161231){
  aapl <- getYahooData(tick, start, end)
  # names(aapl)
  # plot(aapl$Close)
  aapl <-data.frame(date=index(aapl), coredata(aapl))
  aapl$dec <- 0
  aapl$inc <- 0
  for (i in 1:(nrow(aapl)-1)) {
    if (aapl$Close[i+1]<aapl$Close[i]) {
      aapl$dec[i+1] <- aapl$dec[i]+1
    }
    if (aapl$Close[i+1]>aapl$Close[i]) {
      aapl$inc[i+1] <- aapl$inc[i]+1
    }
  }
  for (i in 1:(nrow(aapl))) {
    if (aapl$inc[i]==0) {
      aapl$inc[i] <- -aapl$dec[i]
    }
  }
  # plot(aapl$dec,type='l')
  qplot(aapl$date,aapl$inc,geom='line')
}

streak('aapl',20150101,20161231)
streak('googl',20150101,20161231)
streak('fb')
streak('nflx')
streak('amzn')

streak('qqq')
streak('spy')
streak('dia')
streak('iwm')

streak('ashr')
streak('fxi')