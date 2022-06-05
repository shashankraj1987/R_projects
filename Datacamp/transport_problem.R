f1 <- c(10,0,20,11,20)
f2 <- c(12,7,9,20,25)
f3 <- c(0,14,16,18,15)
Dmd <- c(10,15,15,20,0)
finval <- 0

tpt <- matrix(c(f1,f2,f3,Dmd),nrow=4,ncol = 5, byrow = TRUE)

rownames(tpt) <- c("f1","f2","f3","Supply")
colnames(tpt) <- c("w1","w2","w3","w4","Demand")
tpt
tpt["f1","w1"]

cols <- ncol(tpt)
rows <- nrow(tpt)
cols
i <- 1
dmd <- 0

while (i <= cols-1){
  j <- 1
  dmd <- tpt[rows,i]
  tmp <- 0
  diff <- 0
  while (j <= rows-1) {
    print("==============================================")
    print(c("Value of j,i is -->",tpt[j,i]))
    tmp <- tpt[j,i]
    print(c(tmp, dmd))
    if (tmp < dmd){
      diff <- dmd - tmp
      # Write some logic here.
    }
    print("==============================================")
    j <- j + 1
  }
  i <- i + 1
}
