fx <- function(x){
  if(x>0){
    res <- 120 * x^5 * exp(-x)
  }
  else{
    res <- 0
  }
  return(res)
} 
set.seed(101)
n <- 10000

first_distribution <- numeric(n)
#start value is set to 1
first_distribution[1] <- 1
first_accepted_rate <- 0
for (i in 2:n) {
  value <- rnorm(1, mean=first_distribution[i-1], sd=0.1)
  check_value <- fx(value)/fx(first_distribution[i-1])
  if (runif(1) < check_value) {
    first_distribution[i] <- value
    first_accepted_rate <- first_accepted_rate + 1
  } else {
    first_distribution[i] <- first_distribution[i-1]
  }
}
compare_df_a <- data.frame(
  Distribution = "Normal (SD = 0.1)",
  Acceptance_Rate = first_accepted_rate,
  E_X = mean(first_distribution)
)

plot(first_distribution, type='l', main='Normal Distribution with Standard Deviation 0.1',
     xlab='Iteration', ylab='Sampled Value')
hist(first_distribution, breaks=30, main='Histogram : Normal Distribution with Standard Deviation 0.1',
     xlab='Value', ylab='Iteration')


cat("Acceptance Rate of the First Distribution Sample :", first_accepted_rate)

second_distribution <- numeric(n)
second_distribution[1] <- 1
second_acceptance_rate <- 0
for (i in 2:n) {
  value <- rchisq(1,df=floor(second_distribution[i-1])+1)
  check_value <- fx(value)/fx(second_distribution[i-1])
  if (runif(1) < check_value) {
    second_distribution[i] <- value
    second_acceptance_rate <- second_acceptance_rate + 1
  } else {
    second_distribution[i] <- second_distribution[i-1]
  }
}
compare_df_b <- data.frame(
  Distribution = "Chi-Square (df = floor(X_t) + 1)",
  Acceptance_Rate = second_acceptance_rate,
  E_X = mean(second_distribution)
)
plot(second_distribution, type='l', main='Chi Square Distribution',
     xlab='Iteration', ylab='Sampled Value')

hist(second_distribution, breaks=30, main='Histogram : Chi Square Distribution',
     xlab='Value', ylab='Iteration')


cat("Acceptance Rate:", second_acceptance_rate)

third_distribution <- numeric(n)
#start value is set to 1
third_distribution[1] <- 1
third_accepted_rate <- 0
for (i in 2:n) {
  value <- rnorm(1, mean=third_distribution[i-1], sd=0.7)
  check_value <- fx(value)/fx(third_distribution[i-1])
  if (runif(1) < check_value) {
    third_distribution[i] <- value
    third_accepted_rate <- third_accepted_rate + 1
  } else {
    third_distribution[i] <- third_distribution[i-1]
  }
}
compare_df_c <- data.frame(
  Distribution = "Normal (SD = 0.7)",
  Acceptance_Rate = third_accepted_rate,
  E_X = mean(third_distribution)
)

plot(third_distribution, type='l', main='Normal Distribution with Standard Deviation 0.7',
     xlab='Iteration', ylab='Sampled Value')

hist(third_distribution, breaks=30, main='Histogram : Normal Distribution with Standard Deviation 0.7',
     xlab='Value', ylab='Iteration')

cat("Acceptance Rate:", third_accepted_rate)

compare_df <- rbind(compare_df_a,compare_df_b,compare_df_c)
library(pander)
pander(compare_df)

cat("First Dirtibution- Normal Distribution with Standard Deviation 0.1 = ",mean(first_distribution))
cat("Second Dirtibution- Chi- Square Distribution = ",mean(second_distribution))
cat("First Dirtibution- Normal Distribution with Standard Deviation 0.7 = ",mean(third_distribution))

compare_df$`theoretical E(X)` <- 6

pander(compare_df)
