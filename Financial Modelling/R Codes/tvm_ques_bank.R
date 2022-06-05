library(FinCal)
library(FinancialMath)
#### Questions on TVM ##### 

#one bank has 7% annual rate of interest per annum, another bank has 3% rate of interest compounded quarterly.
#which bank is better?

TVM(pv=50000,fv=NA,n=1,i=.0303392,ic=4)
TVM(pv=50000,fv=NA,n=1,i=.07,ic=1)

ear(r=0.03,m=4)


# Deposit 9000 in a Bank, which pays interest semi annually, and quarterly for 9 years
# The rate of interest is 5% 

TVM(pv=9000,fv=NA,n=9,i=.05,ic=2)
TVM(pv = 9000,fv=NA,n=9,i=0.05,ic=4)


TVM(pv=130000,fv=NA,n=1,i=0.0827,ic=1)
