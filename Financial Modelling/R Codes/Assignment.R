library(TTR)
library(PerformanceAnalytics)
library(quantmod)
library(xts)
options("getSymbols.warning4.0" = FALSE)

# Load the Symbols
# Top 5 traded Stocks by Volume as of 30-Nov-2020
NTPC <- getSymbols("NTPC.NS", from = "2015-01-01", auto.assign = FALSE)
Kotak <- getSymbols("KOTAKBANK.NS", from = "2015-01-01", auto.assign = FALSE)
CIPLA <- getSymbols("CIPLA.NS", from = "2015-01-01", auto.assign = FALSE)
WIPRO <- getSymbols("WIPRO.NS", from = "2015-01-01", auto.assign = FALSE)
ONGC <- getSymbols("ONGC.NS", from = "2015-01-01", auto.assign = FALSE)

# Check for NA Values 
any(is.na(NTPC))
any(is.na(Kotak))
any(is.na(CIPLA))
any(is.na(WIPRO))
any(is.na(ONGC))

# Removing the NA Values in from the Stocks.
NTPC <- na.omit(NTPC)
Kotak <- na.omit(Kotak)
WIPRO <- na.omit(WIPRO)
CIPLA <- na.omit(CIPLA)
ONGC <- na.omit(ONGC)
head(ONGC)

# Merging the Adjusted Closing Price of all the stocks into one.
stk_prices <- merge(NTPC[, 6], Kotak[, 6])
stk_prices <- merge(stk_prices, WIPRO[, 6])
stk_prices <- merge(stk_prices, CIPLA[, 6])
stk_prices <- merge(stk_prices, ONGC[, 6])

# Checking the Merged Stock Prices.
head(stk_prices)
any(is.na(stk_prices))

# Checking to see if the charts have some strong co-relation among them.
chart.Correlation(stk_prices)

# Finding the Logarithmic return of all the portfolios as they are spanning
# multiple time-series and base will be different every period.
returns <- Return.calculate(stk_prices, method = "log")
head(returns)
tail(returns)
returns <- returns[-1,]
dim(returns)

# Create the weights
# We Are planning to invest equally in all the Portfolios.
eq_weights <- c(0.2, 0.2, 0.2, 0.2, 0.2)

# Create a portfolio using "Buy and Hold".
pf_bh <- Return.portfolio(returns, weights = eq_weights, verbose = TRUE)
tail(pf_bh$EOP.Value, 2)
tail(pf_bh$EOP.Weight, 2)

# Create a portfolio rebalancing monthly 
#pf_rebal <- Return.portfolio(returns, weights = eq_weights, rebalance_on = "months", verbose = TRUE)
pf_rebal <- Return.portfolio(returns, weights = eq_weights, verbose = TRUE)
tail(pf_bh$EOP.Value, 2)
tail(pf_bh$EOP.Weight, 2)

# Plot the time-series   -----------------  ERROR ---------------
par(mfrow = c(2.00, 1.00), mar = c(2.00, 4.00, 2.00, 2.00))
#plot.zoo(pf_bh)
#plot.zoo(pf_rebal)

# Create eop_weight_bh
eop_weight_bh <- pf_bh$EOP.Weight
head(eop_weight_bh)

# Create eop_weight_rebal
eop_weight_rebal <- pf_rebal$EOP.Weight
head(eop_weight_rebal)

# Plot end of period weights
par(mfrow = c(2, 1), mar = c(2, 4, 2, 2))
plot.zoo(eop_weight_bh$CIPLA.NS.Adjusted)
plot.zoo(eop_weight_bh$KOTAKBANK.NS.Adjusted)

# Analyzing the performance
#Exploring the monthly Nifty returns
Nifty <- getSymbols("^NSEI", from = "2015-01-01", auto.assign = FALSE)
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
#bonds <- read.csv("D:\\BITS\\2020 S1\\Advanced Financial Modelling\\Risk_Free.csv")
bonds <- readxl::read_excel("D:/Users/shash/Documents/BITS/Semester 4/Advanced Financial Modelling (S1-20_BAZC418)/Final Assignment/IN_Risk_Free.xlsx")
bonds <- bonds[, 1:2]
head(bonds)
#bonds$Date <- as.Date(as.character(bonds$Date),format = "%b %d, %Y")
rf <- as.xts(bonds[, 2] / 25000, order.by = bonds$Date)
head(rf)
merged <- 1
merged <- merge(Nifty_Returns, rf)
merged <- na.omit(merged)

summary(merged)
dim(merged)

head(merged)
merged <- na.locf(merged)
head(merged)

# Compute the annualized risk free rate
annualized_rf <- (1 + merged$Price)^250 - 1
head(annualized_rf)
# Plot the annualized risk free rate
#plot.zoo(annualized_rf)

# Compute the series of excess portfolio returns
merged$Nifty_Excess <- merged$Nifty - merged$Price
head(merged)
sapply(merged, "mean")
Nifty_sharpe_Ratio <- mean(merged$Nifty_Excess) / sd(merged$Nifty_Excess)

#Detecting non-normality using skewness and kurtosis
skewness(merged$Nifty)
skewness(merged$Nifty_Excess)
kurtosis(merged$Nifty)

#Correlation between the stocks
chart.Correlation(returns)
chart.RollingCorrelation(returns$NTPC.NS.Adjusted, returns$ONGC.NS.Adjusted, width = 250)

#Making a risk-reward scatter diagram
means <- apply(returns, 2, "mean")
sds <- apply(returns, 2, "sd")

# Create a scatter plot
plot(sds, means)
text(sds, means, labels = colnames(returns), cex = 0.7)


#Finding the mean-variance efficient portfolio
#####################################################################################################

library(PortfolioAnalytics)
library(DEoptim)
library(ROI)
require(ROI.plugin.glpk)
require(ROI.plugin.quadprog)
pspec <- portfolio.spec(assets = colnames(returns))

#Sum of Weights Constraint
pspec <- add.constraint(portfolio = pspec,
                        type = "weight_sum",
                        min_sum = 1,
                        max_sum = 1)
#Box Constraint
pspec <- add.constraint(portfolio = pspec,
                        type = "box",
                        min = c(-0.5, -0.5, -0.5, -0.5, -0.5),
                        max = c(0.5, 0.5, 0.5, 0.5, 0.5))
#Group Constraint
pspec <- add.constraint(portfolio = pspec, type = "group",
                        groups = list(groupA = c(1, 3),
                                      grouB = c(2, 4, 5)),
                        group_min = c(0.1, 0.15),
                        group_max = c(0.85, 0.55))
#Position Limit Constraint
pspec <- add.constraint(portfolio = pspec, type = "position_limit", max_pos = 4)
#Diversification Constraint
pspec <- add.constraint(portfolio = pspec, type = "diversification", div_target = 0.7)
#Target Return Constraint
pspec <- add.constraint(portfolio = pspec, type = "return", return_target = 0.0005)

#Adding Objectives
pspec <- add.objective(portfolio = pspec, type = "risk", name = "ES", arguments = list(p = 0.975))
pspec <- add.objective(portfolio = pspec, type = "quadratic_utility", risk_aversion = 0.25)

#Solvers
opt_maxret <- optimize.portfolio(R = returns, portfolio = pspec, optimize_method = "ROI", trace = TRUE)
opt_maxret$weights
opt_maxret$objective_measures
opt_maxret$portfolio


# Portfolio Performance Evaluation
#################################################################################

library(readxl)
library(zoo)
library(quantmod)
library(PerformanceAnalytics)
HDFC_MF <- read_excel("D:/BITS/2020 S1/Advanced Financial Modelling/HDFC_MF.xlsx")
HDFC_MF$`NAV date` <- as.Date(HDFC_MF$`NAV date`)
names(HDFC_MF)[1] <- "Date"
HDFC_MF <- read.zoo(HDFC_MF)

# Create the variable returns using Return.calculate()
returns <- Return.calculate(HDFC_MF, method = "log")
head(returns)
tail(returns)
# Remove the first row of returns
returns <- returns[-1,]

# Risk Free Rate of Return
bonds <- read.csv("D:\\BITS\\2020 S1\\Advanced Financial Modelling\\Risk_Free.csv")
bonds <- bonds[, 1:2]
names(bonds)[1] <- "Date"
bonds$Date <- as.Date(as.character(bonds$Date), format = "%b %d, %Y")
rf <- as.xts(bonds[, 2] / 25000, order.by = bonds$Date)
merged <- merge(Nifty_Returns, rf)
merged <- na.omit(merged)
head(merged)
merged <- na.locf(merged)
head(merged)
merged <- as.zoo(merged)

x <- merge(returns, merged, all = FALSE)
x <- na.locf(x)
x <- as.xts(x)
charts.PerformanceSummary(x[, 1:5], x[, "rf"])
charts.PerformanceSummary(x[, 1:5], x[, "rf"], methods = "HistoricalVaR")
t(table.CalendarReturns(x[, 1:5]))
table.Stats(x[, 1:5])
chart.Boxplot(x[, 1:5])
chart.RiskReturnScatter(x[, 1:5])


# Tables
table.Variability(x[, 1:6])
table.Distributions(x[, 1:5])
table.AnnualizedReturns(x[, 1:5], Rf = x[, 7])
table.InformationRatio(x[, 1:5], x[, 6])
table.CAPM(x[, 1:5], x[, 6])
table.SpecificRisk(x[, 1:5], x[, 6])
table.DownsideRisk(x[, 1:5], Rf = x[, 7])
table.DownsideRiskRatio(x[, 1:5])
table.Drawdowns(x[, 5])
table.DrawdownsRatio(x[, 1:5], Rf = 0.04 / 250)

