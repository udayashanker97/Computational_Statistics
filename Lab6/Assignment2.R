#part a
# Load necessary libraries
library(ggplot2)
load("bankdata.Rdata")

set.seed(123)
len <- nrow(bankdata)
dataset <- bankdata[sample(len, 22), ]

ggplot(bankdata, aes(x = age, y = balance)) +
  geom_point() +
  geom_point(data = dataset, color = "red", size = 3) +
  labs(title = "Plotting dataSet", x = "Age", y = "Log Balance")

#part b
source("bankcrit.r")
annealing_fn <- function(data, initial_subset, initial_temperature, cooling_rate, beta, iteration) {
  present_indices <- sample(1:nrow(data), 22)
  current_temp <- initial_temperature
  criterion_values <- numeric()
  
  while (current_temp > 0.206 * initial_temperature) {
    print(current_temp)
    for (i in 1:iteration) {
      if(i %% 100 == 0){
        print(i)
      }
      # 1. Generate a new candidate subset by swapping one element
      temp_indices <- present_indices
      select_index <- sample(1:22, 1)
      remaining_indices <- setdiff(1:nrow(data), temp_indices)
      new_index <- sample(remaining_indices, 1)
      temp_indices[select_index] <- new_index
      
      # 2. Compute acceptance probability
      crit_value1 <- crit(bankdata, present_indices)
      crit_value2 <- crit(bankdata, temp_indices)
      h_value <- exp((crit_value1 - crit_value2) / current_temp)
      
      # 3. Accept new subset based on probability
      if (runif(1) < min(h_value, 1)) {
        present_indices <- temp_indices
      }
      criterion_values <- c(criterion_values, crit_value1)
    }
    # 4. Update temperature and iteration parameters
    current_temp <- cooling_rate * current_temp
    iteration <- beta * iteration
  }
  return(list(selected_dataset = data[present_indices, ], criterion_values = criterion_values))
}

#part c

res_1 <- annealing_fn(bankdata, dataset, initial_temperature = 1000,
                      cooling_rate = 0.99, beta = 1, iteration = 1000)
plot(res_1$criterion_values, type = "l", col = "red", 
     xlab = "Iteration", ylab = "Criterion Value", main = "Criterion Values for Scenerio 1")

res_2 <- annealing_fn(bankdata, dataset, initial_temperature = 100,
                      cooling_rate = 0.7, beta = 1.5, iteration = 150)
plot(res_2$criterion_values, type = "l", col = "blue", 
     xlab = "Iteration", ylab = "Criterion Value", main = "Criterion Values for Scenerio 2")
