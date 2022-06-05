library(ggplot2)
library(forecast)
library(urca)

setwd("D:/Users/shash/Documents/BITS/Semester 4/Advanced Financial Modelling (S1-20_BAZC418)")

dr_rddy = read.csv("DRREDDYs_Data_Equity_Valuation.csv")

summary(dr_rddy)
any(is.null(dr_rddy))

dr_rddy$Date <- as.Date(as.character(dr_rddy$Date),format = "%d-%b-%y")
TS_Data <- ts(dr_rddy[,c(5)]) ## Taking the Time Series plot against closing price
plot(TS_Data)
acf(TS_Data)
pacf(TS_Data)

AutoArimamodel <- auto.arima(TS_Data,max.p = 5,max.q = 5,max.order = 10,stepwise = FALSE,approximation = FALSE, trace = TRUE)
AutoArimamodel <- auto.arima(TS_Data,max.p = 5,max.q = 5,max.order = 10,stepwise = FALSE,approximation = FALSE)
### Trace = true is option in the ARIMA and it shows all the possible combinations, this model has tried. 

summary(AutoArimamodel)

pd <- ur.df(TS_Data,type="none",selectlags = "AIC")
summary(pd)

forecast(AutoArimamodel,10)
