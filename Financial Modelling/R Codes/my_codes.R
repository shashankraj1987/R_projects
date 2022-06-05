library(FinCal)
library(FinancialMath)
library(BondValuation)
library(RQuantLib)
options(digits = 6,scipen = 5)

#PayDay Loans Solution 
# When the interest is compounded every 14 days 
 
TVM(pv=100,fv=115,n=14/365,i=NA)
TVM(pv=100,fv=115,n=14/365,i=NA)[[4]]*100

# Calculating Compound Interest for a different period of times. 

iter <- 1
yrs <- c(1,5,10,20,50,100)

while (iter <=6){
  f_val <- (TVM(pv=1000,fv=NA,i=.10,ic =1 ,n= yrs[iter])[2])
  iter <- iter+1
  print(f_val)
  
}
## Misc Functions ## 

## CAGR 
CAGR = function(en_amt, init_amt, yrs){
  return((((en_amt/init_amt)^(1/yrs))-1)*100)
}


### Simple Rate of Return ###

APR = function(pv,fv){
  
  # This function Calculates the Annual Percentage Rate 
  # pv = "Present Value", fv = "Future Value"
  # Usage APR(100,115)
  # output -> 0.15
  
  ## Initial Checks 
  
  all=list(pv,fv)
  #NULL
  if(any(lapply(all,is.null)==T)) stop("Cannot input any variables as NULL.")
  #Length
  if(any(lapply(list(pv,fv),length) != 1)==T) stop("pv and fv must be of length 1.")
  #Numeric
  if(!is.numeric(pv)) stop("pv must be numeric.")
  if(!is.numeric(fv)) stop("fv must be numeric.")
  #NA
  if(any(is.na(pv)) | is.na(fv)) stop("Cannot input NA for any variables.")
  #Infinite
  if(any(pv==Inf) | fv==Inf) stop("Cannot input infinite for any variables.")
  #Logical
  if(length(pv)==0 | length(fv)==0 ) stop("Not enough information.")
  
  ## Returning the Percentage 
  
  return((fv/pv -1))
}

APR(100,115)

#################################
# Amortization Table 

amort.table(Loan = 160000,36,i=NA,ic=1,pf=36)

# Monthly payment of 5586, for 3 years, pv = 160000,find the interest rate (InCorrect)
annuity.level(pv=160000,fv=NA,n=3,i=.1550374,ic = 12,imm=TRUE)

# Monthly payment of 1000 for 12 years at 5% interest rate (InCorrect)
annuity.level(pv=NA,fv=NA,n=5,pmt=1000,i=.05,ic=1,imm=TRUE)

# 100$ paid annually for 5 years, at the interest rate of 9%, annuity immediate (Correct) 
annuity.level(pmt = 100,n=5,ic=1,i=.09 )


annuity.level(pv=160000,fv=NA,n=36,pmt=5586,i=.1550374,ic = 12,imm=TRUE)

annuity.level(pv=160000,fv=NA,n=36,pmt=5586,i=NA,ic = 12,imm=TRUE)[4]*100*12
amort.table(Loan = 160000,n=36,i=.1550374,ic=12,pf=12)


### Credit Card Payment ###
# Question: I have Credit Card outstanding of 134893.2 
# for which the Credit Card company charges 3.6% per month and 43.2% annually. 
# I have a mutual fund investment of Rs 177635 on which I get 10.99% CAGR and 4.02% ABS return. 
# Should I pay the credit card bills in EMI or Liquidate the Mutual Fund and pay the Credit Card Bill at once?

profit <- TVM(pv=134893.2,fv=NA,n=1,i=.1099,ic=1)[2]
profit
amort.table(Loan = 160000,n=36,i=.1550374,ic=12,pf=12)

##################################################################
######################### Relative Valuation #####################
##################################################################






