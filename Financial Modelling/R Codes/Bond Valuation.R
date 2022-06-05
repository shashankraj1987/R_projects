library(BondValuation)


YTM <- seq(7,14, by=.69)
Y <- seq()
dp<-0
for (i in YTM){
  dp <- (BondVal.Price(YtM=i,SETT = as.Date("2020-10-04"),Em = as.Date("2016-10-04"),Mat = as.Date("2021-10-04"),Coup = 10.15,RV = 100,DCC = 1,CpY = 1)[3])
  print(dp)
  append(Y,dp)
}

dp


plot(YTM,Y)


###### ------------------------------   Teacher's Codes -------------------------------------------
###### --------------------------------------------------------------------------------------------

for (i in seq(5,10,0.1)) {
  x<-BondVal.Price(YtM =i, SETT = Sys.Date(),
                     Em = as.Date("2016-08-06"), Mat = as.Date("2021-08-06"), CpY = 1,
                     RV = 100, Coup = 10.15, DCC = 1)
  print(c("For Year ",i))
  print(c("Dirty Price is ",x$DP))
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



###### ------------------------------------ End ---------------------------------------------------
###### --------------------------------------------------------------------------------------------

BondVal.Yield(CP = 100.02, SETT = Sys.Date(),
              Em = as.Date("2019-04-11"), Mat = as.Date("2023-04-11"), CpY = 1,
              RV = 100, Coup = 5, DCC = 1)

BondVal.Yield(CP = 100.02, SETT = Sys.Date(),
              Em = as.Date("2019-04-11"), Mat = as.Date("2023-04-11"), CpY = 0,
              RV = 100, Coup = 5, DCC = 1)


