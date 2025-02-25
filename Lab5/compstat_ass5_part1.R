# Question 1: Bootstrap for regression
# Part A 
# Load the dataset
data <- read.table("C:/Users/dhanu/OneDrive/Documents/Computational_Statistics/Lab5/kresseertrag.dat", header = TRUE)
colnames(data) <- c("observation", "concentration", "yield")

# Fit a cubic regression model
model <- lm(yield ~ poly(concentration, 3, raw = TRUE), data = data)

# Display model summary
summary(model)

# Part B
# The coefficients and 95%-confidence intervals
coefficients <- coef(model)
conf_intervals <- confint(model, level = 0.95)
cat("95%-Confidence Interval Analytical (lm): \n")
cat(conf_intervals[2, ])
# -78.17667 211.605

# Plot 
plot(data$concentration, data$yield, main = "Yield vs. Concentration", 
     xlab = "Concentration (%)", ylab = "Yield (mg)")
curve(coefficients[1] + coefficients[2]*x + coefficients[3]*x^2 + 
        coefficients[4]*x^3, add = TRUE, col = "blue")

# Part C 
set.seed(123)
b0 <- 10000

#  bootstrap resampling function
bootstrap <- function(data, b0, parameter_index) {
  bs_beta1 <- numeric(b0)
  for (i in 1:b0) {
    bs_data <- data[sample(nrow(data), replace = TRUE), ]
    bs_model <- lm(yield ~ poly(concentration, 3, raw = TRUE), data = bs_data)
    bs_beta1[i] <- coef(bs_model)[2]
  }
  return(bs_beta1)
}
bs_beta1 <- bootstrap(data, b0, 2)

# 95%-bootstrap confidence interval 95% percentile interval
bs_ci <- quantile(bs_beta1, probs = c(0.025, 0.975))
cat("95%-Bootstrap Confidence Interval Bootstrap Percentile (manual): \n")
cat(bs_ci)
# -61.1966 198.2414

# Plot histogram of bootstrap distribution
hist(bs_beta1, main = "Bootstrap Distribution for beta 1", xlab = "beta 1",
     breaks = 50)

# Part D
library(boot)

# Function to fit the model and extract the parameter of interest
bs_fn <- function(data, indices) {
  bs_data <- data[indices, ]
  bs_model <- lm(yield ~ poly(concentration, 3, raw = TRUE), data = bs_data)
  return(coef(bs_model)[2])
}
bs_result <- boot(data, statistic = bs_fn, R = b0)

# Percentile and BCa confidence intervals
ci_perc_bca <- boot.ci(bs_result, type = c("perc", "bca"))
cat("95%-Percentile and 95%-BCa Confidence Interval: \n")
ci_perc_bca
# BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
# Based on 10000 bootstrap replicates
# 
# CALL : 
#   boot.ci(boot.out = bs_result, type = c("perc", "bca"))
# 
# Intervals : 
#   Level     Percentile            BCa          
# 95%   (-63.34, 199.09 )   (-59.78, 200.63 )  
# Calculations and Intervals on Original Scale

# Part E
# Comparision:
# Analytical vs. Bootstrap Approaches:
# (1)The analytical confidence interval (from lm) is wider than all the bootstrap-based intervals.
# (2)The analytical method assumes normality of residuals, which might not be valid if the error distribution is skewed or has outliers.
# 
# Bootstrap Methods (Percentile and BCa):
# (1)The manual percentile method and boot package percentile method give almost the same interval, which suggests that the bootstrap implementation is correct.
# (2)The BCa (bias-corrected and accelerated) interval is slightly different from the percentile intervals, particularly at the lower bound (-59.78 vs -63.34). This suggests that the bootstrap distribution is slightly skewed, and the BCa method accounts for this bias.
# 
# Width of Confidence Intervals:
#   
# (1)The analytical CI [-78.17667, 211.60502] is the widest.
# (2)The BCa method [-59.78, 200.63] is slightly narrower than the other bootstrap CIs, adjusting for bias and skewness.
# (3)The bootstrap CIs provide more precise estimates than the analytical CI, which suggests that the normality assumption in the analytical approach might not hold perfectly.
# 
# Conclusion:
# (1)The bootstrap confidence intervals are more reliable because they do not rely on normality assumptions and directly estimate variability from resampling.
# (2)The BCa interval is likely the best choice, as it adjusts for potential bias and skewness in the bootstrap distribution.
# (3)The analytical method overestimates uncertainty, possibly due to non-normal residuals or influential points.