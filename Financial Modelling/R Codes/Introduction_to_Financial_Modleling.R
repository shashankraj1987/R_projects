library(FinCal)
library(FinancialMath)

options(scipen = 10)

# Present Value, Future Value, Annuity Amount
# Exactly one of pv, fv, n, or i must be NA (unknown)
FVS<-c()
for (years in 5:20) {
  x<-TVM(pv=10000,fv=NA,n=years,i=0.08,ic=1,plot=TRUE)[2]
  FVS<-c(FVS,x)
}
TVM(pv=10000,fv=30000,n=10,i=NA,ic=4,plot=TRUE)
# Computations relating to level annuity
# Setting imm = FALSE resulting in Immediate Annuity where as setting it to TRUE gives Annuity Due
annuity.level(pv=1000000,fv=NA,n=240,pf=12,pmt=NA,imm = TRUE, i=.10, ic = 12,plot = FALSE)
# Uneven cash flows Future Values
fv.uneven(r=0.1, cf=c(-1000, -500, 0, 4000, 3500, 2000))
perpetuity.level(pv=100,pmt=NA,i=0.05,ic=1,pf=2,imm=TRUE)

#Loan Amortizations
amort.table(Loan=1000000,n = 240,i=.10,ic=12,pf=12,plot=TRUE)
amort.period(Loan=1000000,n=240,pmt=NA,i = 0.10,ic=12,pf=12,t = 60)


#Efective Annual Rate
ear(r=0.08,m=4)
ear.continuous(r=0.1)

# NPV and IRR on an investment
NPV(cf0=100,cf=c(50,-60,7,120),times=c(1,5,7,9),i=0.39998073)
FinancialMath::IRR(cf0 = 100,cf=c(50,60,7,120),times=c(1,5,7,9))

#Arithmetic and Geometric Annuities and Perpetuities
annuity.arith(pv=1000000,fv=NA,n=240,p=NA,q=100,i=.10,ic=12,pf=12)
annuity.geo(pv=1000000,fv=NA,n=240,p=NA,k = 0.01,i=.10,ic=12,pf=12)

# Bond Valuation
library(BondValuation)
library(RQuantLib)
x<-BondVal.Price(YtM =18.5, SETT = Sys.Date(),
              Em = as.Date("2016-08-06"), Mat = as.Date("2021-08-06"), CpY = 1,
              RV = 100, Coup = 9.45, DCC = 1)
BondVal.Yield(CP = 100.02, SETT = Sys.Date(),
              Em = as.Date("2019-04-11"), Mat = as.Date("2031-04-11"), CpY = 1,
              RV = 100, Coup = 6.5, DCC = 1)
x$DP

# Impact on the price based on D & C -- -1*Duration*Change in Interest Rate + 0.5*Convexity*Change in IR^2

Yield<-seq(5,15,0.1)
DP<-c()

for (i in seq(5,15,0.1)) {
  DP1<-BondVal.Price(YtM =Yield, SETT = Sys.Date(),
                Em = as.Date("2016-08-06"), Mat = as.Date("2021-08-06"), CpY = 1,
                RV = 100, Coup = 10.15, DCC = 1)
  DP<-c(DP,DP1$DP)
}

params <- list(tradeDate=Sys.Date()-2,
               settleDate=Sys.Date(),
               dt=.25,
               interpWhat="discount",
               interpHow="loglinear")
setEvaluationDate(Sys.Date())
tsQuotes <- list(d3m = 0.0331,
                 d6m = 0.0349,
                 d1y = 0.03749,
                 s2y = 0.04485,
                 s3y = 0.05045,
                 s4y = 0.05366,
                 s5y = 0.05527,
                 s6y = 0.05873,
                 s7y = 0.06233,
                 s8y = 0.06333,
                 s9y = 0.06266,
                 s10y = 0.06097,
                 s12y = 0.06525,
                 s15y = 0.06655,
                 s20y = 0.06457,
                 s25y = 0.06706,
                 s30y = 0.06750)
times <- seq(0,20,.1)
discountCurve <- DiscountCurve(params, tsQuotes, times)
plot(discountCurve,setpar=FALSE)
# price a fixed rate coupon bond
bond <- list(settlementDays=2, issueDate=as.Date("2019-04-11"),
             faceAmount=100, dayCounter='ActualActual')
schedule <- list(effectiveDate=as.Date("2019-04-11"),
                 maturityDate=as.Date("2031-04-11"),
                 period='Annual',
                 calendar='India')
calc<-list(dayCounter='ActualActual', compounding='Compounded',
          freq='Annual')
rates <- c(0.0650)
price<-FixedRateBond(bond, rates, schedule, calc, discountCurve=discountCurve)

#Actual Price in the market
LTP<-100.0250
DC1<-discountCurve
creditRisk<-c()
for (i in seq(0.001,0.1,0.001)) {
  DC1$table$zeroRates<-discountCurve$table$zeroRates+(i)
  price<-FixedRateBond(bond, rates, schedule, calc, discountCurve=DC1)
  creditRisk<-c(creditRisk,(price$cleanPrice-LTP)^2)
}

seq(0.001,0.1,0.001)[which(creditRisk == min(creditRisk))]

# Equity Valuation
# Using Time Series
library(ggplot2)
library(forecast)
library(urca)
DRREDDYs <- read.csv("D:/BITS/2020 S1/Advanced Financial Modelling/DRREDDYs.csv")
DRREDDYs$Date<-as.Date(as.character(DRREDDYs$Date), format = "%d-%b-%y")
TS_Data<-ts(DRREDDYs[,c(5)])
plot(TS_Data)
plot(diff(TS_Data))
summary(ur.df(TS_Data,type = "none", selectlags = "AIC"))
summary(ur.df(diff(TS_Data),type = "none", selectlags = "AIC"))
acf(TS_Data)
pacf(TS_Data)
ets_model = ets(TS_Data, allow.multiplicative.trend = TRUE)
summary(ets_model)
ets_forecast = forecast(ets_model, 10)
AutoArimaModel=auto.arima(TS_Data,max.p = 5, max.q = 5, trace = TRUE, max.order = 10, stepwise = FALSE, approximation = FALSE)
summary(AutoArimaModel)
checkresiduals(AutoArimaModel)
forecast(AutoArimaModel,7)

# Relative Valuation
#Fundamental Analytics
Equity_Valuation <- read_excel("D:/BITS/2020 S1/Advanced Financial Modelling/Equity Valuation.xlsx")
summary(Equity_Valuation)
Equity_Valuation$ICR<-NULL
Equity_Valuation<-Equity_Valuation[Equity_Valuation$PE>0,]
Equity_Valuation<-Equity_Valuation[Equity_Valuation$PB>0,]
Equity_Valuation<-na.omit(Equity_Valuation)

# Relative Valuation
Data_Less_DRReddys<-Equity_Valuation[Equity_Valuation$Company_ID!= "Dr Reddys Labs",]
PE_Mean<-aggregate(PE~Year,data =Data_Less_DRReddys, FUN = "median" )
PB_Mean<-aggregate(PB~Year,data =Data_Less_DRReddys, FUN = "median")
PS_Mean<-aggregate(PS~Year,data =Data_Less_DRReddys, FUN = "median")
EVEDITA<-aggregate(EVEBIDTA~Year, data =Data_Less_DRReddys, FUN = "median")

PE_Mean[PE_Mean$Year == 2020,2]*Equity_Valuation$EPS[Equity_Valuation$Company_ID == "Dr Reddys Labs" & Equity_Valuation$Year== 2020]
PB_Mean[PB_Mean$Year == 2020,2]*Equity_Valuation$BVPS[Equity_Valuation$Company_ID == "Dr Reddys Labs" & Equity_Valuation$Year== 2020]
PS_Mean[PS_Mean$Year == 2020,2]*1085


library(plm)
library(AER)
library(foreign)
library(gplots)
Equity_Valuation <- read_excel("D:/BITS/2020 S1/Advanced Financial Modelling/Equity Valuation.xlsx")
coplot(EPS ~ Year|Company_ID, type="l", data=Equity_Valuation) 
Desc(Equity_Valuation$MV)
E <- pdata.frame(Equity_Valuation, index=c("Company_ID","Year"), drop.index=TRUE, row.names=TRUE)
plotmeans(GPM ~ Company_ID, main="Heterogeineity across companies", data=Equity_Valuation)
plotmeans(GPM ~ Year, main="Heterogeineity across companies", data=Equity_Valuation)

# Simple OLS
ols <-lm(MV~EPS+BVPS+GPM+OPM+NPM+ROE+ROCE+ROA+CR+QR+DE+ATR+ITR+CAGRS3+CAGRNP3, data=E)
summary(ols)

Equity.pool<-plm(MV~EPS+BVPS, data = E, model = "pooling")
Equity.fe<-plm(MV~EPS+BVPS, data = E, model = "within")
Equity.re<-plm(MV~EPS+BVPS, data = E, model = "random")
Equity.fd<-plm(MV~EPS+BVPS, data = E, model = "fd")

cor(Equity_Valuation$EPS,Equity_Valuation$BVPS)

summary(Equity.pool)
summary(Equity.fe)
summary(Equity.re)
summary(Equity.fd)

fixef(Equity.fe)
ranef(Equity.re)

model_full_pool<-plm(BoxCox(MV,-0.4600238)~BoxCox(EPS,0.356859)+BoxCox(BVPS,-0.610576)+GPM+OPM+NPM+ROE+ROCE+ROA+CR+QR+DE+ATR+ITR+CAGRS3+CAGRNP3, data=E, model="pooling")
model_full_fe<-plm(BoxCox(MV,-0.4600238)~BoxCox(EPS,0.356859)+BoxCox(BVPS,-0.610576)+GPM+OPM+NPM+ROE+ROCE+ROA+CR+QR+DE+ATR+ITR+CAGRS3+CAGRNP3, data=E, model="within")
model_full_re<-plm(BoxCox(MV,-0.4600238)~BoxCox(EPS,0.356859)+BoxCox(BVPS,-0.610576)+GPM+OPM+NPM+ROE+ROCE+ROA+CR+QR+DE+ATR+ITR+CAGRS3+CAGRNP3, data=E, model="random")
model_full_fd<-plm(BoxCox(MV,-0.4600238)~BoxCox(EPS,0.356859)+BoxCox(BVPS,-0.610576)+GPM+OPM+NPM+ROE+ROCE+ROA+CR+QR+DE+ATR+ITR+CAGRS3+CAGRNP3, data=E, model="fd")

summary(model_full_pool)
summary(model_full_fe)
summary(model_full_re)
summary(model_full_fd)

fixef(model_full_fe)
ranef(model_full_re)

BoxCox.lambda(E$MV)
BoxCox.lambda(E$EPS)
BoxCox.lambda(E$BVPS)

model_full_pool<-plm(PB~BoxCox(EPS,0.356859)+BoxCox(BVPS,-0.610576)+GPM+OPM+NPM+ROE+ROCE+ROA+CR+QR+DE+ATR+ITR+CAGRS3+CAGRNP3, data=E, model="pooling")
model_full_fe<-plm(PB~BoxCox(EPS,0.356859)+BoxCox(BVPS,-0.610576)+GPM+OPM+NPM+ROE+ROCE+ROA+CR+QR+DE+ATR+ITR+CAGRS3+CAGRNP3, data=E, model="within")
model_full_re<-plm(PB~BoxCox(EPS,0.356859)+BoxCox(BVPS,-0.610576)+GPM+OPM+NPM+ROE+ROCE+ROA+CR+QR+DE+ATR+ITR+CAGRS3+CAGRNP3, data=E, model="random")
model_full_fd<-plm(PB~BoxCox(EPS,0.356859)+BoxCox(BVPS,-0.610576)+GPM+OPM+NPM+ROE+ROCE+ROA+CR+QR+DE+ATR+ITR+CAGRS3+CAGRNP3, data=E, model="fd")

# Technical Analysis
library(quantmod)
library(TTR)
library(PerformanceAnalytics)
getSymbols("RELIANCE.NS",from='2010-01-01',to='2020-08-31')
RELIANCE.NS<-na.omit(RELIANCE.NS)
head(RELIANCE.NS)
Open <- Op(RELIANCE.NS)   #Open Price
High <- Hi(RELIANCE.NS)    # High price
Low <- Lo(RELIANCE.NS)  # Low price
Close<- Cl(RELIANCE.NS)   #Close Price
Volume <- Vo(RELIANCE.NS)   #Volume
AdjClose <- Ad(RELIANCE.NS) # Adjusted close

WeekVoYa<- apply.weekly(Vo(RELIANCE.NS),sum)
# sum from Monday to Friday
MonthVoYa <- apply.monthly(Vo(RELIANCE.NS),sum)
# sum to month
QuarterVoYa <- apply.quarterly(Vo(RELIANCE.NS),sum)
# sum to quarter
YearVoYa <- apply.yearly(Vo(RELIANCE.NS),sum)
# sum to year

chartSeries(RELIANCE.NS,
            type="line",
            subset='2020',
            theme=chartTheme('white'))
chartSeries(RELIANCE.NS,
            type="bar",
            subset='2020',
            theme=chartTheme('white'))
chartSeries(RELIANCE.NS,
            type="candlesticks",
            subset='2020',
            theme=chartTheme('white'))
sma <-SMA(Cl(RELIANCE.NS),n=20)
tail(sma,n=5)
ema <-EMA(Cl(RELIANCE.NS),n=20)
tail(ema,n=5)
bb <-BBands(Cl(RELIANCE.NS),s.d=2)
tail(bb,n=5)
ROC <- ROC(Cl(RELIANCE.NS),n=2)
# 2-day ROC
head(ROC,n=5)
macd <- MACD(Ad(RELIANCE.NS), nFast=12, nSlow=26,
             nSig=9, maType=EMA)
tail(macd,n=50)
plot(macd)
par(mfrow = c(1,1))
rsi = RSI(Cl(RELIANCE.NS), n=14)
tail(rsi,n=5)
write.csv(merge(RELIANCE.NS,rsi),"D:/BITS/2020 S1/Advanced Financial Modelling/rsi.csv")

chartSeries(RELIANCE.NS,
            subset='2020',
            theme=chartTheme('white'))
addSMA(n=30,on=1,col = "blue")
addSMA(n=200,on=1,col = "red")
addEMA(n=30,on=1,col = "green")

chartSeries(RELIANCE.NS,
            subset='2020',
            theme=chartTheme('white'))
addBBands(n=20,sd=2)
addROC(n=7)
addMACD(fast=12,slow=26,signal=9,type="EMA")
addRSI(n=14,maType="EMA")

#Evaluating Trading Rules
price <- Cl(RELIANCE.NS) # close price
r <- log(price/Lag(price)) # % price change
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
chartSeries(RELIANCE.NS,
            type = 'line',
            subset="2020",
            theme=chartTheme('white'))
addTA(signal,type='S',col='red')
trade <- Lag(signal,1)
ret<-dailyReturn(RELIANCE.NS)*trade
names(ret)<-"filter"
charts.PerformanceSummary(ret, main="Naive Buy Rule")

#Simple fiter buy-sell
signal <-c(NA) # first signal is NA
for (i in 2: length(Cl(RELIANCE.NS))){
  if (r[i] > delta){
    signal[i]<- 1
  } else if (r[i]< -delta){
    signal[i]<- -1
  } else
    signal[i]<- 0
}
signal<-reclass(signal,Cl(RELIANCE.NS))
trade1 <- Lag(signal)
ret1<-dailyReturn(RELIANCE.NS)*trade1
names(ret1) <- 'Naive'
charts.PerformanceSummary(ret1)

#Trading Rules based on RSI
day <-14
price <- Cl(RELIANCE.NS)
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
signal<-reclass(signal,Cl(RELIANCE.NS))
trade2 <- Lag(signal)
ret1 <- dailyReturn(RELIANCE.NS)*trade1
names(ret1) <- 'Naive'
ret2 <- dailyReturn(RELIANCE.NS)*trade2
names(ret2) <- 'RSI'
retall <- cbind(ret1, ret2)
charts.PerformanceSummary(retall,main="Naive v.s. RSI")

#Portfolio Analysis
# Load the necessary Packages
library(PerformanceAnalytics)
library(quantmod)
library(xts)
#Load the necessary stocks data
Britannia <- getSymbols("BRITANNIA.NS", from = "2010-01-01", auto.assign = FALSE)
HDFCBank <- getSymbols("HDFCBANK.NS", from = "2010-01-01",  auto.assign = FALSE)
Nestle <- getSymbols("NESTLEIND.NS",from = "2010-01-01", auto.assign = FALSE)
ZEE <- getSymbols("ZEEL.NS", from = "2010-01-01",  auto.assign = FALSE)
LT <- getSymbols("LT.NS", from = "2010-01-01", auto.assign = FALSE)
# Clean the Data
Britannia <- na.omit(Britannia)
HDFCBank <- na.omit(HDFCBank)
Nestle <- na.omit(Nestle)
ZEE <- na.omit(ZEE)
LT <- na.omit(LT)
#Combine the records
prices <- merge(Britannia[,6],HDFCBank[,6])
prices <- merge(prices,Nestle[,6])
prices <- merge(prices,ZEE[,6])
prices <- merge(prices, LT[,6])
#Verify the data
head(prices)
tail(prices)
# Create the variable returns using Return.calculate()  
returns <- Return.calculate(prices,method = "log")
head(returns)
tail(returns)
# Remove the first row of returns
returns <- returns[-1, ]
# Create the weights
eq_weights <- c(0.2, 0.2, 0.2, 0.2, 0.2)
# Create a portfolio using buy and hold
pf_bh <- Return.portfolio(returns, weights = eq_weights, verbose = TRUE)
tail(pf_bh$EOP.Value)
# Create a portfolio rebalancing monthly 
pf_rebal <- Return.portfolio(returns, weights = eq_weights, rebalance_on = "months", verbose = TRUE)
tail(pf_rebal$EOP.Value)
# Plot the time-series
par(mfrow = c(2, 1), mar = c(2, 4, 2, 2))
plot.zoo(pf_bh)
plot.zoo(pf_rebal)
# Create eop_weight_bh
eop_weight_bh <- pf_bh$EOP.Weight
head(eop_weight_bh)
# Create eop_weight_rebal
eop_weight_rebal <- pf_rebal$EOP.Weight
head(eop_weight_rebal)
# Plot end of period weights
par(mfrow = c(2, 1), mar = c(2, 4, 2, 2))
plot.zoo(eop_weight_bh$BRITANNIA.NS.Adjusted)
plot.zoo(eop_weight_rebal$BRITANNIA.NS.Adjusted)

# Analyzing the performance
#Exploring the monthly Nifty returns
Nifty <- getSymbols("^NSEI", from = "2010-01-01", auto.assign = FALSE)
Nifty <- na.omit(Nifty)
Nifty_Returns <- Return.calculate(Nifty$NSEI.Adjusted)
head(Nifty_Returns)
plot.zoo(Nifty_Returns)
# Produce the year x month table
table.CalendarReturns(Nifty_Returns)
#Daily mean and volatility
Nifty_Returns <- na.omit(Nifty_Returns)
colnames(Nifty_Returns) <- "Nifty"
# Compute the mean monthly returns
mean(Nifty_Returns)
mean.geometric(Nifty_Returns)
sd(Nifty_Returns)
#Excess returns and the portfolio's Sharpe ratio
# Risk Free Rate of Return
bonds <- read.csv("D:\\BITS\\2020 S1\\Advanced Financial Modelling\\Risk_Free.csv")
bonds <- bonds[,1:2]
names(bonds)[1] <- "Date"
bonds$Date <- as.Date(as.character(bonds$Date),format = "%b %d, %Y")
rf <- as.xts(bonds[, 2]/25000, order.by = bonds$Date)
merged <- merge(Nifty_Returns,rf)
merged <- na.omit(merged)
head(merged)
merged <- na.locf(merged)
head(merged)
# Compute the annualized risk free rate
annualized_rf <- (1 + merged$rf)^250 - 1
# Plot the annualized risk free rate
plot.zoo(annualized_rf)
# Compute the series of excess portfolio returns
merged$Nifty_Excess <- merged$Nifty - merged$rf
head(merged)
sapply(merged, "mean")
Nifty_sharpe_Ratio <- mean(merged$Nifty_Excess)/sd(merged$Nifty_Excess)
#Detecting non-normality using skewness and kurtosis
skewness(merged$Nifty)
skewness(merged$Nifty_Excess)
kurtosis(merged$Nifty)

#Correlation between the stocks
chart.Correlation(returns)
chart.RollingCorrelation(returns$BRITANNIA.NS.Adjusted, returns$HDFCBANK.NS.Adjusted, width = 250)
chart.RollingCorrelation(returns$NESTLEIND.NS.Adjusted, returns$ZEEL.NS.Adjusted, width = 250)
chart.RollingCorrelation(returns$HDFCBANK.NS.Adjusted, returns$LT.NS.Adjusted, width = 250)

#Making a risk-reward scatter diagram
means <- apply(returns, 2, "mean")
sds <- apply(returns, 2, "sd")
# Create a scatter plot
plot(sds, means)
text(sds, means, labels = colnames(returns), cex = 0.7)

#Finding the mean-variance efficient portfolio

library(PortfolioAnalytics)
library(DEoptim)
library(ROI)
pspec <- portfolio.spec(assets=colnames(returns))

#Sum of Weights Constraint
pspec <- add.constraint(portfolio=pspec,
                        type="weight_sum",
                        min_sum=1,
                        max_sum=1)
#Box Constraint
pspec <- add.constraint(portfolio=pspec,
                        type="box",
                        min=c(-0.5,-0.5,-0.5,-0.5,-0.5),
                        max=c(0.5, 0.5, 0.5, 0.5,0.5))
#Group Constraint
pspec <- add.constraint(portfolio=pspec, type="group",
                        groups=list(groupA=c(1, 3),
                                    grouB=c(2,4,5)),
                        group_min=c(0.1, 0.15),
                        group_max=c(0.85, 0.55))
#Position Limit Constraint
pspec <- add.constraint(portfolio=pspec, type="position_limit", max_pos=4)
#Diversification Constraint
pspec <- add.constraint(portfolio=pspec, type="diversification", div_target=0.7)
#Target Return Constraint
pspec <- add.constraint(portfolio=pspec, type="return", return_target=0.0005)

#Adding Objectives
pspec <- add.objective(portfolio=pspec, type="risk", name="ES",arguments=list(p=0.975))
pspec <- add.objective(portfolio=pspec, type="quadratic_utility", risk_aversion=0.25)

#Solvers

opt_maxret <- optimize.portfolio(R=returns, portfolio=pspec,optimize_method="ROI", trace=TRUE)
opt_maxret$weights
opt_maxret$objective_measures
opt_maxret$portfolio


# Portfolio Performance Evaluation

library(readxl)
library(zoo)
library(quantmod)
library(PerformanceAnalytics)
HDFC_MF <- read_excel("D:/BITS/2020 S1/Advanced Financial Modelling/HDFC_MF.xlsx")
HDFC_MF$`NAV date`<-as.Date(HDFC_MF$`NAV date`)
names(HDFC_MF)[1]<-"Date"
HDFC_MF<-read.zoo(HDFC_MF)

# Create the variable returns using Return.calculate()  
returns <- Return.calculate(HDFC_MF,method = "log")
head(returns)
tail(returns)
# Remove the first row of returns
returns <- returns[-1, ]

# Risk Free Rate of Return
bonds <- read.csv("D:\\BITS\\2020 S1\\Advanced Financial Modelling\\Risk_Free.csv")
bonds <- bonds[,1:2]
names(bonds)[1] <- "Date"
bonds$Date <- as.Date(as.character(bonds$Date),format = "%b %d, %Y")
rf <- as.xts(bonds[, 2]/25000, order.by = bonds$Date)
merged <- merge(Nifty_Returns,rf)
merged <- na.omit(merged)
head(merged)
merged <- na.locf(merged)
head(merged)
merged<-as.zoo(merged)

x<-merge(returns,merged,all = FALSE)
x <- na.locf(x)
x<-as.xts(x)
charts.PerformanceSummary(x[,1:5],x[,"rf"])
charts.PerformanceSummary(x[,1:5],x[,"rf"],methods="HistoricalVaR")
t(table.CalendarReturns(x[,1:5]))
table.Stats(x[,1:5])
chart.Boxplot(x[,1:5])
chart.RiskReturnScatter(x[,1:5])


# Tables
table.Variability(x[,1:6])
table.Distributions(x[,1:5])
table.AnnualizedReturns(x[,1:5], Rf = x[,7])
table.InformationRatio(x[,1:5], x[,6])
table.CAPM(x[,1:5],x[,6])
table.SpecificRisk(x[,1:5], x[,6])
table.DownsideRisk(x[,1:5], Rf = x[,7])
table.DownsideRiskRatio(x[,1:5])
table.Drawdowns(x[,5])
table.DrawdownsRatio(x[,1:5],Rf = 0.04/250)


library(FinancialMath)


# option Trading Strategies
option.call(S = 393.50,K = 400,r = 0.031972,t = 26/365,sd = 0.5892,price=17.65,position = "short",plot=TRUE)
option.put(S = 393.50,K = 400,r = 0.031972,t = 26/365,sd = 0.5892,price=22.85,position = "short",plot=TRUE)

covered.call(S = 393.50,K = 400,r = 0.031972,t = 26/365,sd = 0.5892,price=17.65,plot=TRUE)
covered.put(S = 100,K = 110,r = 0.03,t = 1,sd = 0.20,price=NA,plot=TRUE)
protective.put(S = 393.50,K = 400,r = 0.031972,t = 26/365,sd = 0.5892,price=22.85,plot=TRUE)

#Spread Strategies (2 calls/2 puts)
bear.call(S = 393.50,K1 = 390,K2 = 410,0.031972,t = 26/365,price1 = 22.55,price2 = 13.95,plot=TRUE)
#bear.call.bls(S = 100,K1 = 70,K2 = 130,r = 0.03,t = 1,sd = 0.20,plot=TRUE)
bull.call(S = 393.50,K1 = 390,K2 = 410,0.031972,t = 26/365,price1 = 22.55,price2 = 13.95,plot=TRUE)
#bull.call.bls(S = 115,K1 = 100,K2 = 145,r = 0.03,t = 1,sd = 0.20,plot=TRUE)

# 1 Call, 1 Put
straddle(S = 393.5,K = 400,r = 0.031972,t =26/365,price1 = 17.65,price2 = 22.85,position = "short",plot=TRUE)
#straddle.bls(S = 100,K = 110,r = 0.03,t = 1,sd = 0.20,position = "short",plot=TRUE)
strangle(S = 393.50,K1 = 330,K2 = 470,r = 0.03,t = 1,price1 = 2.60,price2 = 2.70,plot=TRUE)
#strangle.bls(S = 105,K1= 100,K2 = 110,r = 0.03,t = 1,sd = 0.20,plot=TRUE)

#More than 2 calls/puts (4 calls or 4 puts)
butterfly.spread(S = 400,K1 = 370,K2 = 400, K3 = 430,r = 0.031972,t =26/365,price1 = 34.75,price2 =17.65,price3 = 8.15,plot=TRUE)
butterfly.spread.bls(S = 100,K1 = 75,K3 = 125,r = 0.03,t = 1,sd = 0.20,plot=TRUE)

collar(S = 100,K1 = 90,K2 = 110,r = 0.05,t = 1,price1 = 5,price2 = 15,plot=FALSE)
collar.bls(S = 100,K1 = 90,K2 = 110,r = 0.05,t = 1,sd = 0.20,plot=TRUE)

#Black Scholes Option Pricing
BS_Price<-c()
for (i in seq(1800,2400,20)) {
  BS_Price<-c(BS_Price,GBSOption(TypeFlag = "p", S = 2064.35, X = i, Time = 25/365, r = 0.031972,
            b = 0.031972, sigma = 0.4853)@price)
}
GBSOption(TypeFlag = "c", S = 26798.95, X = 27000, Time = 5/365, r = 0.031735,
          b = 0.031735, sigma = 0.4422)
GBSCharacteristics(TypeFlag = "p", S = 26798.95, X = 26000, Time = 5/365, r = 0.031735,
                   b = 0.031735, sigma = 0.3067931)
GBSVolatility(price = 297.95, TypeFlag = "c", S = 26798.95, X = 27000, Time = 5/365, r = 0.031735,
              b = 0.031735, maxiter = 1000)

# Binomial Tree Option Valuation

library("fOptions")
CRRBinomialTreeOption(TypeFlag = "pa", S = 26798.95, X = 26000,
                      Time = 5/365, r = 0.031735, b = 0.031735, sigma = 0.4422, n = 5)
JRBinomialTreeOption(TypeFlag = "pe", S = 26798.95, X = 26000,
                     Time = 5/365, r = 0.031735, b = 0.031735, sigma = 0.4422, n = 5)
TIANBinomialTreeOption(TypeFlag = "pe", S = 26798.95, X = 26000,
                       Time = 5/365, r = 0.031735, b = 0.031735, sigma = 0.4422, n = 5)
Option_Premium<-c()
for(n in 2:50){
  Option_Premium<-c(Option_Premium,CRRBinomialTreeOption(TypeFlag = "pe", S = 26798.95, X = 26000,
                                                         Time = 5/365, r = 0.031735, b = 0.031735, sigma = 0.4422, n = n)@price)
}
plot(Option_Premium, type = "l")
##to plot an option tree (for put american)
CRRTree = BinomialTreeOption(TypeFlag = "pe", S = 26798.95, X = 26000,
                             Time = 5/365, r = 0.031735, b = 0.031735, sigma = 0.4422, n = 5)
BinomialTreePlot(CRRTree, dy = 1, cex = 0.8, ylim = c(-6, 7),
                 xlab = "n", ylab = "Option Value")
title(main = "Option Tree")

# Introduction to Exotic Options
library(fExoticOptions)
GeometricAverageRateOption(TypeFlag = "p", S = 80, X = 85,
                           Time = 0.25, r = 0.05, b = 0.08, sigma = 0.20)
LevyAsianApproxOption(TypeFlag = "c", S = 100, SA = 100, X = 105,
                      Time = 0.75, time = 0.50, r = 0.10, b = 0.05, sigma = 0.15)
StandardBarrierOption(TypeFlag = "cdo", S = 100, X = 90,
                      H = 95, K = 3, Time = 0.5, r = 0.08, b = 0.04, sigma = 0.25)
CashOrNothingOption(TypeFlag = "p", S = 100, X = 80, K = 10,
                    Time = 9/12, r = 0.06, b = 0, sigma = 0.35)
FloatingStrikeLookbackOption(TypeFlag = "c", S = 120,
                             SMinOrMax = 100, Time = 0.5, r = 0.10, b = 0.10-0.06,
                             sigma = 0.30)
FixedStrikeLookbackOption(TypeFlag = "c", S = 100,
                          SMinOrMax = 100, X = 105, Time = 0.5, r = 0.10, b = 0.10,
                          sigma = 0.30)
TwoAssetCorrelationOption(TypeFlag = "c", S1 = 52, S2 = 65,
                          X1 = 50, X2 = 70, Time = 0.5, r = 0.10, b1 = 0.10, b2 = 0.10,
                          sigma1 = 0.2, sigma2 = 0.3, rho = 0.75)

