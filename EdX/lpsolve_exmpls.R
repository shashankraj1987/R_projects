library(lpSolve)

cost.mat <- matrix(nrow =  3,ncol =  4)
cost.mat[1,] <- c(3,5,7,6)
cost.mat[2,] <- c(2,5,8,2)
cost.mat[3,] <- c(3,6,9,2)

cost.mat

row.signs <- rep("==",3)
col.signs <- rep("==",4)
row.rhs <- c(50,75,25)
col.rhs <- c(20,20,50,60)

solution <- lp.transport(cost.mat = cost.mat,direction = "min",
                         row.signs = row.signs,
                         row.rhs = row.rhs,
                         col.signs = col.signs,
                         col.rhs = col.rhs)
solution
solution$solution
