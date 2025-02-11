myvar <- function(x) {
  n <- length(x)
  var1 <- sum(x^2)
  var2 <- sum(x)
  variance <- ((1/(n-1)) * (var1 - ((1/n) * (var2)^2)))
  return(variance)
}
x <- c(1,2,3,4,5,6,7,8,9,1)
cat("Choosen vector x : ",x)
variance <- myvar(x)
cat("Variance for given x is ",variance)

set.seed(123456)
rm_vec = rnorm(n = 10000, mean = 10^8, sd = 1)
print("Summary of the generated vector")
summary(rm_vec)

y <- numeric(length(rm_vec))
Xi <- numeric(length(rm_vec))
for (i in 1:length(rm_vec)) {
  Xi[i] <- rm_vec[1:i]
  y[i] <- myvar(Xi[i]) - var(Xi[i])
}

plot(1:length(rm_vec), y, type = "l", col = "blue", lwd = 2,
     xlab = "Subset size", ylab = "Difference",
     main = "Difference between myvar and var for subsets of Xi")

welford_variance <- function(x) {
  if (length(x) <= 1){
    stop("For calcultions we need alteast two values in dataset")
  }
  mean_value <- 0
  sd <- 0
  for (i in 1:length(x)) {
    d1 <- x[i] - mean_value
    mean_value <- mean_value+(d1/i)
    d2 <- x[i]-mean_value
    sd <- sd + (d1*d2)
  }
  # Bessel's correction,because of this standard deviation is divided by length(x)-1
  variance <- sd / (length(x) - 1)
  return(variance)
}

k <- numeric(length(rm_vec))
#for welford variance calculation we need atleast two data in each subset
for (i in 2:length(rm_vec)) {
  Xi <- rm_vec[1:i]
  k[i] <- welford_variance(Xi) - var(Xi)
}

plot(2:length(rm_vec), k[2:length(rm_vec)], type = "l", col = "blue", lwd = 2,
     xlab = "Subset size (i)", ylab = "Difference",
     main = "Difference between Welford's algorithm and var for subsets of x")
