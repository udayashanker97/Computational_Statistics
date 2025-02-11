n <- 10000000
set.seed(123)

time1<-system.time({
  U1 <- runif(n)
  U2 <- runif(n)
  
  Z1 <- sqrt(-2 * log(U1)) * cos(2 * pi * U2)
  Z2 <- sqrt(-2 * log(U1)) * sin(2 * pi * U2)
  
  X1 <- sqrt(0.6) * Z1
  X2 <- sqrt(0.6) * Z2
})

# Part B

library(mvtnorm)
n <- 10000000
mean_vector <- c(0, 0)
cov_matrix <- matrix(c(0.6, 0, 0, 0.6), ncol = 2)

set.seed(123)
time2<-system.time({
  sample <- rmvnorm(n, mean_vector, cov_matrix)
})


print(time1)
print(time2)