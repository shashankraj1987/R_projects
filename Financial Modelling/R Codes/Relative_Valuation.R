# Title     : Relative Valuation
# Objective : Find the Relative Valuation of Dr. Reddy's
# Created by: shash
# Created on: 11/13/2020

# Load the related Libraries
library("readxl")

install.packages("dash")
# Loading the Datasets.
ev <- read_excel("D:/Users/shash/Documents/BITS/Semester 4/Advanced Financial Modelling (S1-20_BAZC418)/Equity_Valuation_Pharma.xlsx")
summary(ev)
ev

any(is.na(ev))
ev[is.na(ev)] <- 0

val_ratios <- c("Company_ID","Year","MV","PE","PB","EVEBIDTA","PS")

Relative_Valuation <- subset(ev, select = val_ratios)
unique(Relative_Valuation$Company_ID)

# Ensure we have non-zero Valuation Ratios as 0's are not useful Valuation Ratios.
Relative_Valuation <- Relative_Valuation[Relative_Valuation$PE > 0,]

# Objective : Find the Relative Valuation of Dr. Reddy's Using Relative Valuation approach.
###########################################################################################
# Step 1: Identify the Competitors of Dr. Reddy's. ( We can Use Cluster Analysis for the same). In this dataset, we assume that all the other companies are the Competitors of Dr. Reddy's.
# Step 2: Find the Mean or Median of Relative Valuation Approach of these Competitors.
# Step 3: Perform Relative Valuation against the Mean or Median of these companies.

# Step 1: Get all the Competitors in one Data Frame
###########################################################################################

R_V_Comp <- Relative_Valuation[Relative_Valuation$Company_ID!="Dr Reddys Labs",]
R_V_Comp
R_V_Tgt <- Relative_Valuation[Relative_Valuation$Company_ID=="Dr Reddys Labs",]
R_V_Tgt

# Step 2: Find the Mean or Median of Relative Valuation Approach of these Competitors.
###########################################################################################

## Mean Based Valuation ##

aggregate(EVEBIDTA~Year,data=R_V_Tgt, FUN = "mean")

PE_Aggr <- aggregate(PE~Year,data = R_V_Comp, FUN = "mean")
PE_Aggr
PB_Aggr <- aggregate(PB~Year, data = R_V_Comp, FUN = "mean")
PB_Aggr
PS_Aggr <- aggregate(PS~Year, data = R_V_Comp, FUN = "mean")
PS_Aggr
EBIT_Aggr <- aggregate(EVEBIDTA~Year, data = R_V_Comp, FUN = "mean")
EBIT_Aggr

## Median Based Valuation ##

aggregate(PE~Year,data=R_V_Tgt, FUN = "median")

PE_Aggr <- aggregate(PE~Year,data = R_V_Comp, FUN = "median")
PE_Aggr
PB_Aggr <- aggregate(PB~Year, data = R_V_Comp, FUN = "median")
PB_Aggr
PS_Aggr <- aggregate(PS~Year, data = R_V_Comp, FUN = "median")
PS_Aggr
EBIT_Aggr <- aggregate(EVEBIDTA~Year, data = R_V_Comp, FUN = "median")
EBIT_Aggr

# Step 3: Perform Relative Valuation against the Mean or Median of these companies.
#####################################################################################

# Calculating the worth using P/E Ratio and means as aggregation method.
# price = Industry (P/E) ratio for a year x EPS of Dr. Reddy's for that Year.

EPS_2020_reddy <- ev[ev$Company_ID == "Dr Reddys Labs" & ev$Year == 2020, 'EPS']
PE_Aggr <- aggregate(PE~Year,data = R_V_Comp, FUN = "mean")
PE_Aggr <- PE_Aggr[PE_Aggr$Year==2020,'PE']
calc_worth <- PE_Aggr * EPS_2020_reddy
act_worth <- ev[ev$Company_ID == "Dr Reddys Labs" & ev$Year == 2020,'MV']
ifelse(calc_worth>act_worth, "UnderPriced, Decision to Buy", "OverPriced, Decision to Sell")

# Similarly, we can do valuation based on other combinations. For P/S Ratio, we need the number of shares
##########################################################################################################

# P/B Ratio
###########

EPS_2020_reddy <- ev[ev$Company_ID == "Dr Reddys Labs" & ev$Year == 2020, 'BVPS']
PE_Aggr <- aggregate(PB~Year,data = R_V_Comp, FUN = "mean")
PE_Aggr<- PE_Aggr[PE_Aggr$Year==2020,'PB']
calc_worth <- PE_Aggr * EPS_2020_reddy
act_worth <- ev[ev$Company_ID == "Dr Reddys Labs" & ev$Year == 2020,'MV']
ifelse(calc_worth>act_worth, "UnderPriced, Decision to Buy", "OverPriced, Decision to Sell")

### For Median Values

EPS_2020_reddy <- ev[ev$Company_ID == "Dr Reddys Labs" & ev$Year == 2020, 'BVPS']
PE_Aggr <- aggregate(PB~Year,data = R_V_Comp, FUN = "median")
PE_Aggr <- PE_Aggr[PE_Aggr$Year==2020,'PB']
calc_worth <- PE_Aggr * EPS_2020_reddy
act_worth <- ev[ev$Company_ID == "Dr Reddys Labs" & ev$Year == 2020,'MV']
ifelse(calc_worth>act_worth, "UnderPriced, Decision to Buy", "OverPriced, Decision to Sell")