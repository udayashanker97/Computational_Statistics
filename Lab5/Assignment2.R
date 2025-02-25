library(BSDA)
c <- log(log(2))
gumbel_fn <- function(median,n = 13) {
  Y <- runif(n)
  X <- -log(-log(Y)) + median + c
  return(X)
}
median_values <- seq(0, 2, length.out = 50)
len <- length(median_values)
power <- numeric()
for (j in seq_along(median_values)) {
  median <- median_values[j]
  count <- 0
  for (i in 1:1000) {
    data <- gumbel_fn(median)
    test <- SIGN.test(data)
    if(test$p.value < 0.05){
      count <- count + 1
    }
  }
  power[j] <- count / 1000
}
cat("Median Value: ", median_values[1],"  Power:",power[1])
cat("Median Value: ", median_values[14],"  Power:",power[14])
cat("Median Value: ", median_values[26],"  Power:",power[26])
cat("Median Value: ", median_values[38],"  Power:",power[38])
cat("Median Value: ", median_values[50],"  Power:",power[50])

plot(median_values, power, type = "b", pch = 19, col = "blue",
     xlab = "Median", ylab = "Power", main = "Power Curve")
