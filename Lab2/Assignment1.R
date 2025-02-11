fx <- function(x, y) {
  sin(x + y) + (x - y)^2 - 1.5*x + 2.5*y + 1
}

x_range <- seq(-1.5, 4, length.out = 100)
y_range <- seq(-3, 4, length.out = 100)

z_range <- outer(x_range, y_range, fx)

contour(x_range, y_range, z_range, main = "Contour Plot of f(x, y)", 
        xlab = "x", ylab = "y", nlevels =100)

grad_function <- function(x, y) {
  d1 <- cos(x + y) + 2 * (x - y) - 1.5
  d2 <- cos(x + y) - 2 * (x - y) + 2.5
  d <- c(d1,d2)
  return(d)
}
hess_function <- function(x, y) {
  d1 <- -sin(x + y) + 2
  d2 <- -sin(x + y) + 2
  d3 <- -sin(x + y) - 2
  d <- matrix(c(d1, d3, d3, d2), nrow = 2, byrow = TRUE)
  return(d)
}
newton_method <- function(x0, y0) {
  tol = 1e-6
  max_iter = 100
  x <- x0
  y <- y0
  count <- 0
  for (i in 1:max_iter) {
    count <- count + 1
    grad <- grad_function(x, y)
    hess <- hess_function(x, y)
    newton_step <- -solve(hess) %*% grad
    temp_x <- x + newton_step[1]
    temp_y <- y + newton_step[2]
    x <- temp_x
    y <- temp_y
    if (sqrt(sum(newton_step^2)) < tol) {
      break;
    }
  }
  return(list(start_x=x0, start_y =y0, res = c(x, y), iterations = count))
}
st_pts <- list(
  c(-0.5, -1.5),
  c(1, 1),
  c(2, 3),
  c(3, -1),
  c(2.7, 2),
  c(-0.5, 2),
  c(-1, 3),
  c(3, 3)
)
res_list <- lapply(st_pts, function(p) newton_method(p[1], p[2]))
res_df <- data.frame(
  x = sapply(res_list, function(res) res$start_x),
  y = sapply(res_list, function(res) res$start_y),
  x_new = sapply(res_list, function(res) res$res[1]),
  y_new = sapply(res_list, function(res) res$res[2]),
  iterations = sapply(res_list, function(res) res$iterations)
)
colnames(res_df) <- c("x", "y" ,"x*", "y*", "Iterations")

print("Solutions:")
print(res_df)
classification <- function(x, y) {
  hess_value <- hess_function(x, y)
  e_values <- eigen(hess_value)$values
  if (all(e_values < 0)) {
    return("Local maximum")
  } else if (all(e_values > 0)) {
    return("Local minimum")
  } else {
    return("Saddle point")
  }
}

res_df$type <- mapply(classification, res_df$`x*`, res_df$`y*`)
res_df$fx <- mapply(fx, res_df$`x*`, res_df$`y*`)

print("Solutions and their respective Classifications:")
print(res_df)


# # Define the function f(x, y)
# f <- function(x, y) {
#   sin(x + y) + (x - y)^2 - 1.5*x + 2.5*y + 1
# }
# 
# # Define the range for x and y
# x_range <- seq(-1.5, 4, length.out = 100)
# y_range <- seq(-3, 4, length.out = 100)
# 
# # Create a matrix to store function values
# z_range <- outer(x_range, y_range, f)
# 
# # optimize_function <- function(p) {
# #   f(p[1], p[2])  # Call f with separate x and y
# # }
# # 
# # # Run optimization with two separate parameters (x0, y0)
# # x0 <- 0
# # y0 <- 0
# # opt_result <- optim(c(x0, y0), fn = optimize_function, method = "BFGS")
# 
# # Display the results
# # opt_result$par  # Optimal (x, y) values
# # opt_result$value  
# 
# # part a
# 
# # Plot the contour
# contour(x_range, y_range, z_range, main = "Contour Plot of f(x, y)", 
#         xlab = "x", ylab = "y", nlevels =100)
# 
# 
#  
# 
# 
# #part b
# 
# grad_function <- function(x, y) {
#   d1 <- cos(x + y) + 2 * (x - y) - 1.5
#   d2 <- cos(x + y) - 2 * (x - y) + 2.5
#   d <- c(d1,d2)
#   return(d)
# }
# 
# # Define the Hessian matrix
# hess_function <- function(x, y) {
#   d1 <- -sin(x + y) + 2
#   d2 <- -sin(x + y) + 2
#   d3 <- -sin(x + y) - 2
#   d <- matrix(c(d1, d3, d3, d2), nrow = 2, byrow = TRUE)
#   return(d)
# }
# 
# # part c
# 
# newton_method <- function(x0, y0) {
#   tol = 1e-6
#   max_iter = 100
#   x <- x0
#   y <- y0
#   count <- 0
#   for (i in 1:max_iter) {
#     count <- count + 1
#     grad <- grad_function(x, y)
#     hess <- hess_function(x, y)
#     newton_step <- -solve(hess) %*% grad
#     temp_x <- x + newton_step[1]
#     temp_y <- y + newton_step[2]
#     x <- temp_x
#     y <- temp_y
#     if (sqrt(sum(newton_step^2)) < tol) {
#       break;
#     }
#   }
#   return(list(start_x=x0, start_y =y0, res = c(x, y), iterations = count))
# 
# }
# #part d
# 
# 
# st_pts <- list(
#   # # c(-1, -2),
#   # # c(6,5),
#   # c(1, 1), 
#   # c(2, 3), 
#   # c(3, -1), 
#   # c(-0.5, 2), 
#   # c(4, -3)
#   
#   # c(-0.5,-1.5),
#   # c(2.7,2)
#   
#   c(-0.5,-1.5),
#   c(-1.5, -2.5), 
#   c(1, 1), 
#   c(7, 3), 
#   c(5, -1), 
#   c(3.7,2),
#   c(-5.8, 8.3),
#   c(4, -1)
# )
# res_list <- lapply(st_pts, function(p) newton_method(p[1], p[2]))
# res_df <- data.frame(
#   x = sapply(res_list, function(res) res$start_x),
#   y = sapply(res_list, function(res) res$start_y),
#   x_new = sapply(res_list, function(res) res$res[1]),
#   y_new = sapply(res_list, function(res) res$res[2]),
#   iterations = sapply(res_list, function(res) res$iterations)
# )
# colnames(res_df) <- c("x", "y" ,"x*", "y*", "Iterations")
# 
# print("Solutions:")
# print(res_df)
# 
# # part e
# 
# classification <- function(x, y) {
#   hess_value <- hess_function(x, y)
#   e_values <- eigen(hess_value)$values
#   if (all(e_values < 0)) {
#     return("Local maximum")
#   } else if (all(e_values > 0)) {
#     return("Local minimum")
#   } else {
#     return("Saddle point")
#   }
# }
# 
# res_df$type <- mapply(classification, res_df$`x*`, res_df$`y*`)
# res_df$f <- mapply(f, res_df$`x*`, res_df$`y*`)
# 
# print("Solutions and their respective Classifications:")
# print(res_df)

# unique_results$type <- apply(res_df, 1, function(row)
#   classify_critical_point(row[3], row[4]))
# 
# # Find function values at each critical point
# unique_results$f_value <- apply(res_df, 1, function(row)
#   f(as.numeric(row[3]), as.numeric(row[4])))
# 
# local_minima <- unique_results %>% filter(type == "Local minimum")
# global_min <- local_minima %>% filter(f_value == min(f_value))
# 
# # Print results
# print("Candidate solutions and classification:")
# print(unique_results)
# 
# if (nrow(global_min) > 0) {
#   print("lobal minimum found:")
#   print(global_min)
# } else {
#   print(" No global minimum found. Try adding more starting points.")
# }