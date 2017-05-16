library("caTools")
library("fields")

scan('http://finance.google.com/finance/info?client=ig&q=wb',"",sep="\n",quiet=TRUE)[c(6,12)]

download.file("http://image.sinajs.cn/newchartv5/usstock/min/wb.gif",'win.gif', mode = 'wb')
jj <- read.gif("win.gif", verbose=TRUE, flip=TRUE)
image(jj$image, col=jj$col, main=jj$comment, asp=1)

plot(0:1,0:1,type="n",ann=FALSE,axes=FALSE)
rasterImage(jj,0,0,1,1)

library(TTR) # to get yahoo data
library(xts) # to transfrom time series to data frame
library('scales') # for percentage format
library(ggplot2)

raw_data <- "
name
aapl
meet
panw
data
crm
acia
nvda
amd
x
aal
w
dang
ddd
amba
jmei
yy
nq
nflx
gme
xnet
zpin
spy
googl
fb
amzn
baba
jd
fit
gpro
tsla
ibm
hd
dg
feye
grpn
z
dia
qqq
ctrp
qunr
twtr
wuba
cmg
lnkd
ntes
dis
sohu
sina
wb
hlf
mbly
yelp
csiq
qiwi
cmcm
vips
tour
himx
yrd
xom
tedu
lc
twtr
aal
axp
gs
msft
ms
bita
athm
intc
amd
sfun
shop
twlo
ln
acia
leju
jd
t
vz
spy
qqq
dia
xly
xlp
xle
xlf
xlv
xli
xlb
xlk
xlu
momo
vnet
lc
scty
fslr
bidu
"

raw_data <- "name
qqq"

# raw_data <- "
# name
# sina
# wb
# jd
# baba
# spy
# w
# yy
# "

get_ticker<-function(){
  raw_data<-textConnection((raw_data))
  list <- read.table(raw_data,header=TRUE)
  list1 <- list['name']
  list1 <- unique(list1)
  return(list1)
}

raw_data <- "name
qqq"
list1 <- get_ticker()

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

win <- function(){
  table<-data.frame()
  getp <- function(tk){
    url <- paste0('http://finance.google.com/finance/info?client=ig&q=',tk)
    raw_json <- scan(url,"",sep="\n",quiet=TRUE)
    row <- c(paste0(toupper(tk),raw_json[6],raw_json[12],raw_json[15]))
    row <- gsub(",\"l\" : \"",",",row)
    row <- gsub("\",\"lt_dts\" : \"",",",row)
    row <- gsub("Z\",\"cp\" : \"",",",row)
    row <- gsub("\"",",",row)
    row <- strsplit(row,",")
    row <- do.call(rbind,row)
    #table <- c(table,row)
    #table<-rbind(table,c(paste0(toupper(tk),raw_json[6],raw_json[12])))
  }
  
  for (i in 1:nrow(list1) ) {
    list2 <- as.character(list1[i,1])
    table<-rbind(table,getp(list2))
  }
  
  names(table)<-c('name','price','time','change')
  return(table)
}

# run this 1
table<-win()
table$price <- as.numeric(as.character(table$price))
table$change <- (as.numeric(as.character(table$change))/100)
table<-table[order(table$change,decreasing=TRUE),]
table$change <- percent(table$change)

# output
View(table)

streak <- function(tick,start=19900101,end=20171231){
  aapl <- getYahooData(tick, start, end)
  aapl <-data.frame(date=index(aapl), coredata(aapl))
  aapl$Unadj.Close<-NULL
  aapl$Div<-NULL
  aapl$Split<-NULL
  aapl$Adj.Div<-NULL
  aapl$Vol_MA5<-SMA(aapl$Volume,n=5)
  aapl$Vol_Perc<-aapl$Volume/aapl$Vol_MA5
  aapl$MA200<-SMA(aapl$Close,n=min(200,nrow(aapl)-10))
  aapl$MA200_d<- aapl$Close - aapl$MA200
  aapl$MA200_dperc<- aapl$MA200_d/aapl$MA200
  aapl$MA50<-SMA(aapl$Close,n=min(50,nrow(aapl)-10))
  aapl$MA50_d<- aapl$Close - aapl$MA50
  aapl$MA50_dperc<- aapl$MA50_d/aapl$MA50
  aapl$ema9<-EMA(aapl$Close,n=9)
  aapl$ema9_dperc<- aapl$Close/aapl$ema9-1
  aapl$ema21<-EMA(aapl$Close,n=21)
  aapl$ema21_dperc<- aapl$Close/aapl$ema21-1
  aapl$Target<- (aapl$Close+aapl$MA200)/2
  aapl$RSI<-RSI(aapl$Close)
  aapl$dec <- 0
  aapl$inc <- 0
  aapl$inc_perc <- 0
  for (i in 1:(nrow(aapl)-1)) {
    aapl$inc_perc[i+1] <- aapl$inc_perc[i] + aapl$Close[i+1]/aapl$Close[i]-1
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
  aapl$ema9_above0 <- 0
  for (i in 1:(nrow(aapl)-1)) {
    if (is.na(aapl$ema9_dperc[i+1])==0 & is.na(aapl$ema9_dperc[i])==0 & aapl$ema9_dperc[i+1]>=0 & aapl$ema9_dperc[i]>=0) {
      aapl$ema9_above0[i+1] <- aapl$ema9_above0[i]+1
    }
    if (is.na(aapl$ema9_dperc[i+1])==0 & is.na(aapl$ema9_dperc[i])==0 & aapl$ema9_dperc[i+1]<0 & aapl$ema9_dperc[i]<0) {
      aapl$ema9_above0[i+1] <- aapl$ema9_above0[i]-1
    }
  }  
  # aapl$EMA21_above0 <- 0
  # for (i in 1:(nrow(aapl)-1)) {
  #   if (is.na(aapl$EMA21_dperc[i+1])==0 & is.na(aapl$EMA21_dperc[i])==0 & aapl$EMA21_dperc[i+1]>=0 & aapl$EMA21_dperc[i]>=0) {
  #     aapl$EMA21_above0[i+1] <- aapl$EMA21_above0[i]+1
  #   }
  #   if (is.na(aapl$EMA21_dperc[i+1])==0 & is.na(aapl$EMA21_dperc[i])==0 & aapl$EMA21_dperc[i+1]<0 & aapl$EMA21_dperc[i]<0) {
  #     aapl$EMA21_above0[i+1] <- aapl$EMA21_above0[i]-1
  #   }
  # }
  aapl$ticker <- toupper(tick)
  return(aapl)
}

# run this 2
table1<-data.frame()
for (i in 1:nrow(list1) ) {
  list2 <- as.character(list1[i,1])
  table1<-rbind(table1,streak(list2))
}


#table1<-table1[,c(16,c(1:15))]

table2<-table1[table1$date==max(table1$date),]

# output
View(table2[order(table2$RSI),])
View(table2[order(-table2$Vol_Perc),])
View(table2[order(table2$MA200_dperc),])
#table2[order(table2$ticker),]

View(table1[order(-table1$MA200_dperc),])
View(table1[order(-table1$MA50_dperc),])
View(table1[order(-table1$RSI),])

Ticker <- toupper('qqq')

View(table1[table1$ticker==Ticker,][order(-table1$MA200_dperc[table1$ticker==Ticker]),])
View(table1[table1$ticker==Ticker,][order(-table1$MA50_dperc[table1$ticker==Ticker]),])
View(table1[table1$ticker==Ticker,][order(-table1$RSI[table1$ticker==Ticker]),])


Ticker <- toupper('fit')
multiplot(qplot(table1$date[table1$ticker==Ticker],table1$MA200[table1$ticker==Ticker],color=Ticker,geom='line'),
          qplot(table1$date[table1$ticker==Ticker],table1$Close[table1$ticker==Ticker],color=Ticker,geom='line'),
          qplot(table1$date[table1$ticker==Ticker],table1$MA200_dperc[table1$ticker==Ticker],color=Ticker,geom='line'),
          qplot(table1$date[table1$ticker==Ticker],table1$MA50_dperc[table1$ticker==Ticker],color=Ticker,geom='line'),
          cols=2)

#Ticker <- toupper('bita')

multiplot(qplot(table1$date[table1$ticker==Ticker],table1$MA50[table1$ticker==Ticker],color=Ticker,geom='line'),
          qplot(table1$date[table1$ticker==Ticker],table1$Close[table1$ticker==Ticker],color=Ticker,geom='line'),
          qplot(table1$date[table1$ticker==Ticker],table1$MA50_dperc[table1$ticker==Ticker],color=Ticker,geom='line'),
          qplot(table1$date[table1$ticker==Ticker],table1$RSI[table1$ticker==Ticker],color=Ticker,geom='line'),
          cols=2)

qplot(table1$date[table1$ticker==Ticker],table1$ema9_above0[table1$ticker==Ticker],color=Ticker,geom='line')

names(table1)
View(table1[c('date','Close','ema9','ema9_above0')])

Ticker <- toupper('wb')
multiplot(ggplot(data=table1[table1$ticker==Ticker,]) + geom_line(aes(x=date, y=Close))+ geom_line(aes(x=date, y=ema9,color=Ticker)) + geom_line(aes(x=date, y=ema21), colour='blue'),
          qplot(table1$date[table1$ticker==Ticker],table1$ema9_above0[table1$ticker==Ticker],color=Ticker,geom='line'),
          qplot(table1$date[table1$ticker==Ticker],table1$ema9_dperc[table1$ticker==Ticker],color=Ticker,geom='line'),
          cols=2)

View(table1[table1$ticker=='WB',])

table1[table1$ema9_above0 == max(table1$ema9_above0),]

table3 <- table1[order(-table1$ema9_above0),]

unique(table3$ticker[1:500])
table3$ema9_above0[1:500]

qplot(table1$ema9_above0)

Ticker <- toupper('fb')
multiplot(qplot(table1$date[table1$ticker==Ticker],table1$Close[table1$ticker==Ticker],color=Ticker,geom='line'),
          qplot(table1$date[table1$ticker==Ticker],table1$ema21_dperc[table1$ticker==Ticker],color=Ticker,geom='line'),
          qplot(table1$date[table1$ticker==Ticker],table1$ema21[table1$ticker==Ticker],color=Ticker,geom='line'),
          cols=2)

Ticker <- toupper('wb')
multiplot(qplot(table1$date[table1$ticker==Ticker],table1$inc_perc[table1$ticker==Ticker],geom='line',color='red'),
          qplot(table1$date[table1$ticker==Ticker],table1$Close[table1$ticker==Ticker],geom='line',color='red'),
          qplot(table1$date[table1$ticker==Ticker],table1$inc[table1$ticker==Ticker],geom='line',color='red'), cols=2)

View(table1[table1$ticker==Ticker,])

#plot(table1$date[table1$ticker==Ticker],table1$MA200_d[table1$ticker==Ticker],type='l')

# cbind(table1$Close[table1$ticker=='JD'],SMA(table1$Close[table1$ticker=='JD'],n=200),table1$Close[table1$ticker=='JD']-SMA(table1$Close[table1$ticker=='JD'],n=200))

# temp query
tick='aapl'
View(table1[table1$ticker==toupper(tick),])
qplot(table1[table1$ticker==toupper(tick),]$date,table1[table1$ticker==toupper(tick),]$Close,geom='line')

scan('http://finance.google.com/finance/info?client=ig&q=z',"",sep="\n",quiet=TRUE)[c(6,12)]
scan('http://finance.google.com/finance/info?client=ig&q=ntes',"",sep="\n",quiet=TRUE)[c(6,12)]

for (i in 1:1000) {
  print(scan('http://finance.google.com/finance/info?client=ig&q=wb',"",sep="\n",quiet=TRUE)[c(6,12)])
  print(scan('http://finance.google.com/finance/info?client=ig&q=spy',"",sep="\n",quiet=TRUE)[c(6,12)])
  Sys.sleep(30)
  cat("\014") 
}

# get pe of each stock
tablepe <- read.csv(paste0('http://finance.yahoo.com/d/quotes.csv?s=',paste0(list1$name,collapse='+'),'&f=snd1l1t8cj1yerr7p5s7'),header=FALSE,col.names=c('tick','name','date','price','target','change','mcap','div','earning','pe','pef','ps','shortratio'))

read.csv('http://finance.yahoo.com/d/quotes.csv?s=aapl&f=aa5bb6jkl',header=FALSE) #,col.names=c('ask','ask size','bid','bid size'))

tablepe$pe<-as.numeric(as.character(tablepe$pe))
tablepe$pef<-as.numeric(as.character(tablepe$pef))
View(tablepe[order(tablepe$pe),])

list1 <- get_ticker()
names_pb <- paste0(list1$name,collapse="+")
pb <- read.csv(paste0('http://finance.yahoo.com/d/quotes.csv?s=',names_pb,'&f=sf6j1p6p5rr6r7r5jkl1t8ee7e8e9'),header=FALSE)
names(pb) <- c('ticker','float','mcap','pb','ps','pe','pe_current','pe_next','peg','yearl','yearh','price','target','eps','eps_c','eps_n','eps_nq')
pb$float <- as.numeric(as.character((pb$float)))
pb$pb <- as.numeric(as.character((pb$pb)))
pb$pe <- as.numeric(as.character((pb$pe)))
pb$ps <- as.numeric(as.character((pb$ps)))
pb$pe_current <- as.numeric(as.character((pb$pe_current)))
pb$pe_next <- as.numeric(as.character((pb$pe_next)))
pb$yearl <- as.numeric(as.character((pb$yearl)))
pb$yearh <- as.numeric(as.character((pb$yearh)))
pb$target <- as.numeric(as.character((pb$target)))
pb$yrange <- pb$yearh/pb$yearl
pb$ratio_l <- pb$price/pb$yearl
pb$ratio_h <- pb$price/pb$yearh
pb$ratio_target <- pb$price/pb$target
pb$floatcap <- pb$float*pb$price/1000000000
pb$mcap1 <- 0
pb$mcap1[grep('B', as.character((pb$mcap)))] <- round(as.numeric(gsub('B', '', as.character((pb$mcap[grep('B', as.character((pb$mcap)))])))),2)
pb$mcap1[grep('M', as.character((pb$mcap)))] <- round(0.001*as.numeric(gsub('M', '', as.character((pb$mcap[grep('M', as.character((pb$mcap)))])))),2)
pb$floatperc <- round(pb$floatcap/pb$mcap1,2)
pb <- pb[c(1,2,24,22,23,c(3:21))]
View(pb[order(-pb$pb),])

n_ma <- function(x) {
  raw_data <- paste0('name\n',x)
  return(raw_data)
}
n_ma('qqq')
rm(raw_data)

names(table1)

raw_data <- paste0('name\n','qqq')
list1 <- get_ticker()
table1<-data.frame()
for (i in 1:nrow(list1) ) {
  list2 <- as.character(list1[i,1])
  table1<-rbind(table1,streak(list2))
}

# MA50_dperc, MA200_dperc, ema21_dperc, ema9_dperc
ttt <- table1[c('date','ema21_dperc','inc_perc')]
names(ttt)[2] <-'ema'
ttt$n <- 0

for (i in 1:(nrow(ttt)-1)) {
  if (is.na(ttt$ema[i])==0 & ttt$ema[i] >=0 & ttt$ema[i+1] >=0) {ttt$n[i+1] <- ttt$n[i] +1}
  if (is.na(ttt$ema[i])==0 & ttt$ema[i] <0 & ttt$ema[i+1] <0) {ttt$n[i+1] <- ttt$n[i] -1}
}

#qplot(tail(ttt$date,1000), tail(ttt$n,1000), geom='line')

# par(mfrow=c(1,1))

# library(cowplot)

# detach("package:packageToUnload", unload=TRUE)
# library(ggplot2)

p1 <- qplot(tail(ttt$date,2000), tail(ttt$n,2000), geom ='line')
p2 <- qplot(tail(ttt$date,2000), tail(ttt$inc_perc,2000), geom ='line')

multiplot(p1, p2, cols = 1)

p1 <- qplot(ttt$date, ttt$n, geom ='line')
p2 <- qplot(ttt$date, ttt$inc_perc, geom ='line')

multiplot(p1, p2, cols = 1)

# 
# start.time <- Sys.time()
# for (i in 1:100000) {
#   print(2^i)
# }
# end.time <- Sys.time()
# end.time - start.time

# 
# ggplot(mpg, aes(displ, hwy)) + 
#   geom_point() + 
#   scale_y_continuous(
#     "mpg (US)", 
#     sec.axis = sec_axis(~ . * 1.20, name = "mpg (UK)")
#   )


plot(tail(table1$ema9,500),type='l')
lines(tail(table1$ema21,500),col='red')