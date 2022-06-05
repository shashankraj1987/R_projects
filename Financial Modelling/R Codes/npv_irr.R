## NPV and IRR Function 
library(dplyr)

# Defining the Present Value 
calc_pv <- function(fv,r,n){
  pv <- fv/(1+r)^n
  return (pv)
}

# If NPV is positive, IRR is more than the Discount Rate. Vice-Versa. 
# Defining a Function for NPV 
calc_npv <- function(cashflows, r) {
  n <- 0:(length(cashflows) - 1)
  npv <- sum( calc_pv(cashflows, r, n) )
  npv
}


# Define IRR function: calc_irr
calc_irr <- function(cashflows) {
  uniroot(calc_npv,interval = c(0, 1),cashflows = cashflows)$root
}
