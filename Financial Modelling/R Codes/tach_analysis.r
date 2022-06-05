library(TTR)        
library(quantmod)  ## Getting the Data from Yahoo Finance, also has time related functions.  
library(PerformanceAnalytics)

getSymbols("PYPL")

stock_dt <- na.omit(PYPL)
head(stock_dt)
tail(stock_dt)

#We use Adjusted Price only as it's base is more uniform for analysis.
# Extracting the required features. 
AdjClose <- Ad(stock_dt) # Gets the Adjusted Price of the Stock 
Volume <- Vo(stock_dt)   # Gets the Volume of the Stock. 


week_aggr_vol <- apply.weekly(Volume, sum)
week_aggr_vol

month_aggr_vol <- apply.monthly(Volume,median)

plot(month_aggr_vol)

# Bar Chart 
chartSeries(stock_dt,
            type="bar",
            subset='2015::2020', ## Get a subset of Date Range. 
            theme=chartTheme('white'))

# Box Plot 
chartSeries(stock_dt,
            type="candlesticks",
            subset='2018',
            theme=chartTheme('white'))

## Green color in these charts mean that the prices have gone up and Red means gone down. 
## Using Performance Indicators to Find the Short Term and Long Term Performance of the Stock

sma <- SMA(AdjClose,n=20)  ## Last 20-Day per Month Simple Moving Average. 
plot(sma)

ema <- EMA(AdjClose, n=20)
plot(ema)

bb <- BBands(AdjClose,n = 20,sd = 2) ## Calculate Bolinger Bands 

chartSeries(stock_dt$PYPL.Adjusted,
            type="candlesticks",
            subset='2020',
            theme=chartTheme('white'))
addSMA(n=20,on=1,col="blue")   ## Adds the moving Averages to chart
addEMA(n=20,on=1,col="red")    ## Adds the Exponential Moving Average to the chart 
addBBands(n=20,sd=2)           ## Adds Bolinger Bands to the charts. 

########################################################################################
####### Automating the Portfolio Valuation and make Buy Sell Decision ##################
#Evaluating Trading Rules

price <- Cl(stock_dt) # close price
r <- log(price/Lag(price)) # % price change
head(price)
head(lag(price))
delta <-0.005 #threshold
signal <-c(0) # first date has no signal

#Loop over all trading days (except the first)
for (i in 2: length(price)){
  if (r[i] > delta){
    signal[i]<- 1
  } else
    signal[i]<- 0
}
signal<-reclass(signal,price)
tail(signal, n=3)
chartSeries(stock_dt,
            type = 'line',
            subset="2020",
            theme=chartTheme('white'))
addTA(signal,type='S',col='red')
trade <- Lag(signal,1)
ret<-dailyReturn(stock_dt)*trade
names(ret)<-"filter"
charts.PerformanceSummary(ret, main="Naive Buy Rule")

#Simple fiter buy-sell
signal <-c(NA) # first signal is NA
for (i in 2: length(Cl(stock_dt))){
  if (r[i] > delta){
    signal[i]<- 1
  } else if (r[i]< -delta){
    signal[i]<- -1
  } else
    signal[i]<- 0
}
signal<-reclass(signal,Cl(stock_dt))
trade1 <- Lag(signal)
ret1<-dailyReturn(stock_dt)*trade1
names(ret1) <- 'Naive'
charts.PerformanceSummary(ret1)

#Trading Rules based on RSI
day <-14
price <- Cl(stock_dt)
signal <- c()                    #initialize vector
rsi <- RSI(price, day)     #rsi is the lag of RSI
signal [1:day+1] <- 0            #0 because no signal until day+1

for (i in (day+1): length(price)){
  if (rsi[i] < 30){             #buy if rsi < 30
    signal[i] <- 1
  }else if(rsi[i] > 70)  {                       
    signal[i] <- -1
  }
  else {
    signal[i] <- 0
  }
}
signal<-reclass(signal,Cl(stock_dt))
trade2 <- Lag(signal)
ret1 <- dailyReturn(stock_dt)*trade1
names(ret1) <- 'Naive'
ret2 <- dailyReturn(stock_dt)*trade2
names(ret2) <- 'RSI'
retall <- cbind(ret1, ret2)
charts.PerformanceSummary(retall,main="Naive v.s. RSI")
########################################################################################

