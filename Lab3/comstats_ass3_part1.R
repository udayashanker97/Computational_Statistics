# Question 1: Sampling triangular distribution
# Part A
# Rejection sampling
rejection_sampling <- function(n) {
  f <- function(x) {
    ifelse(x < -1 | x > 1, 0, ifelse(x <= 0, x + 1, 1 - x))
  }
  samples <- numeric(n)
  count <- 0
  while (count < n) {
    x <- runif(1, -1, 1)
    u <- runif(1)
    if (u <= f(x)) {
      count <- count + 1
      samples[count] <- x
    }
  }
  return(samples)
}
# Histogram Plot for Rejection sampling
set.seed(123)
samples_rs <- rejection_sampling(10000)
hist(samples_rs, breaks = 50, main = "Rejection Sampling", 
     xlab = "x", freq = FALSE)

# Part B
# Composition sampling
composition_sampling <- function(n) {
  generate_Y <- function(n) {
    u <- runif(n)
    return(1 - sqrt(1 - u))
  }
  samples <- numeric(n)
  for (i in 1:n) {
    if (runif(1) < 0.5) {
      samples[i] <- generate_Y(1)
    } else {
      samples[i] <- -generate_Y(1)
    }
  }
  return(samples)
}
# Histogram Plot for Composition sampling
set.seed(123)
samples_cs <- composition_sampling(10000)
hist(samples_cs, breaks = 50, main = "Composition Sampling", 
     xlab = "x", freq = FALSE)

# Part C
# Difference uniform sampling
difference_uniform_sampling <- function(n) {
  u1 <- runif(n)
  u2 <- runif(n)
  x <- u1 - u2
  return(x)
}
# Histogram Plot for Difference uniform sampling
set.seed(123)
samples_dus <- difference_uniform_sampling(10000)
hist(samples_dus, breaks = 50, main = "Difference of Uniforms", 
     xlab = "x", freq = FALSE)

# Part D
# Executing time for rejection sampling
time_rs <- system.time(rejection_sampling(10000))
cat("Rejection Sampling Time: \n")
time_rs

# Executing time for composition sampling
time_cs <- system.time(composition_sampling(10000))
cat("Composition Sampling Time: \n")
time_cs

# Executing time for difference of uniforms
time_dus <- system.time(difference_uniform_sampling(10000))
cat("Difference of Uniforms Time: \n")
time_dus

# Comparision
# 
# Rejection Sampling Time:
# user  system elapsed
# 0.00    0.00    0.11
# 
# 
# Composition Sampling Time:
# user  system elapsed
# 0.00    0.00    0.02
# 
# 
# Difference of Uniforms Time:
# user  system elapsed
# 0       0       0
# 
# Choosing the Best Method:
# Rejection Sampling is slower compared to the other methods because 
# it involvesan accept-reject step. Many proposed samples are discarded, 
# leading to inefficiencies and a longer execution time. However, it is a 
# flexible method that can be applied to more complex distributions where 
# direct sampling is difficult.
# 
# Composition Sampling is efficient and provides accurate results since 
# it relies on the inverse cumulative distribution function. However, it 
# requires prior knowledge of the inverse CDF, which may not always be easy to 
# derive for complex distributions. Once the inverse CDF is known, the method 
# is straightforward and computationally efficient.
# 
# Difference of Uniforms is the simplest and fastest method since it only 
# requires generating two independent uniform random numbers and taking their
# difference. It has minimal computational overhead and does not require any 
# accept-reject steps or knowledge of the inverse CDF, making it the preferred 
# choice for efficiency.
# 
# The difference of uniforms method is the most efficient and easiest to 
# implement. Based on execution time and simplicity, the difference of uniforms 
# method is the preferred choice for generating samples of X.

# Variance for Difference uniform sampling
variance_dus <- var(samples_dus)
variance_dus
# [1] 0.1682124
