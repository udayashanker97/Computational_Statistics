# Question: EM Algorithm
library(ggplot2)

# Part A
# EM Algorithm for 3 components
emalg_3_comp <- function(dat, eps = 0.000001) {
  n <- length(dat)
  pi1 <- rep(NA, n)  # initialize vector for prob. to belong to group 1
  pi2 <- rep(NA, n)  # initialize vector for prob. to belong to group 2
  pi3 <- rep(NA, n)  # initialize vector for prob. to belong to group 3
  
  # Define reasonable starting values for parameters
  p1 <- 1/3          # starting value for mixing parameter of group 1
  p2 <- 1/3          # starting value for mixing parameter of group 2
  p3 <- 1/3          # starting value for mixing parameter of group 3
  sigma1 <- sd(dat) * 2/3  # starting value for standard deviation in group 1
  sigma2 <- sigma1         # starting value for standard deviation in group 2
  sigma3 <- sigma1         # starting value for standard deviation in group 3
  mu1 <- mean(dat) - sigma1  # starting value for mean of group 1
  mu2 <- mean(dat)           # starting value for mean of group 2
  mu3 <- mean(dat) + sigma1  # starting value for mean of group 3
  pv <- c(p1, p2, p3, mu1, mu2, mu3, sigma1, sigma2, sigma3)  # parameter vector
  
  # Store parameter paras
  para <- list(p1 = c(), p2 = c(), p3 = c(), mu1 = c(), mu2 = c(), mu3 = c(), sigma1 = c(), sigma2 = c(), sigma3 = c())
  
  cc <- eps + 100  # initialize convergence criterion
  while (cc > eps) {
    pv1 <- pv  # save previous parameter vector
    
    ### E step ###
    for (j in 1:n) {
      pi1_j <- p1 * dnorm(dat[j], mean = mu1, sd = sigma1)
      pi2_j <- p2 * dnorm(dat[j], mean = mu2, sd = sigma2)
      pi3_j <- p3 * dnorm(dat[j], mean = mu3, sd = sigma3)
      total <- pi1_j + pi2_j + pi3_j
      pi1[j] <- pi1_j / total
      pi2[j] <- pi2_j / total
      pi3[j] <- pi3_j / total
    }
    
    ### M step ###
    p1 <- mean(pi1)
    p2 <- mean(pi2)
    p3 <- mean(pi3)
    mu1 <- sum(pi1 * dat) / (p1 * n)
    mu2 <- sum(pi2 * dat) / (p2 * n)
    mu3 <- sum(pi3 * dat) / (p3 * n)
    sigma1 <- sqrt(sum(pi1 * (dat - mu1)^2) / (p1 * n))
    sigma2 <- sqrt(sum(pi2 * (dat - mu2)^2) / (p2 * n))
    sigma3 <- sqrt(sum(pi3 * (dat - mu3)^2) / (p3 * n))
    
    pv <- c(p1, p2, p3, mu1, mu2, mu3, sigma1, sigma2, sigma3)
    
    # Part B
    cc <- sqrt(sum((pv - pv1)^2)) / sqrt(sum(pv1^2))  # scale-independent convergence criterion
    
    # Storing parameter values
    para$p1 <- c(para$p1, p1)
    para$p2 <- c(para$p2, p2)
    para$p3 <- c(para$p3, p3)
    para$mu1 <- c(para$mu1, mu1)
    para$mu2 <- c(para$mu2, mu2)
    para$mu3 <- c(para$mu3, mu3)
    para$sigma1 <- c(para$sigma1, sigma1)
    para$sigma2 <- c(para$sigma2, sigma2)
    para$sigma3 <- c(para$sigma3, sigma3)
  }
  
  # Part D
  par(mfrow = c(3, 3))
  for (param in names(para)) {
    plot(para[[param]], type = "l", main = param, xlab = "Iteration", ylab = "Value")
  }
  pv
}

# Data
load("C:/Users/dhanu/OneDrive/Documents/Computational Stats/Lab 6/threepops.Rdata")

# Part C
# Histogram
hist(dat3p, breaks = 20, main = "Histogram of dat3p", xlab = "Value")

# Fit the mixture model
results <- emalg_3_comp(dat3p)
cat("Final parameter estimates: \n",results)