#1 and #2
x <- c(73.2, 75.9, 82.5, 79, 79.3, 83.7, 107, 109, 109, 96.5)
y <- c(32.22, 29.31, 64.75, 55.48, 81.29, 75.08, 166, 192, 137, 144)


n <- length(x)


mean_x <- 0
for (i in 1:n) {
  mean_x <- mean_x + x[i]
}
mean_x <- mean_x / n


mean_y <- 0
for (i in 1:n) {
  mean_y <- mean_y + y[i]
}
mean_y <- mean_y / n


numerator <- 0
denominator <- 0

for (i in 1:n) {
  numerator <- numerator + (x[i] - mean_x) * (y[i] - mean_y)
  denominator <- denominator + (x[i] - mean_x)^2
}

beta1 <- numerator / denominator
beta0 <- mean_y - beta1 * mean_x


cat("Regression Equation: y =", beta0, "+", beta1, "* x\n")


numerator_corr <- 0
denominator_x <- 0
denominator_y <- 0

for (i in 1:n) {
  numerator_corr <- numerator_corr + (x[i] - mean_x) * (y[i] - mean_y)
  denominator_x <- denominator_x + (x[i] - mean_x)^2
  denominator_y <- denominator_y + (y[i] - mean_y)^2
}

correlation <- numerator_corr / sqrt(denominator_x * denominator_y)


cat("Correlation Coefficient:", correlation, "\n")
#3
x <- c(73.2, 75.9, 82.5, 79, 79.3, 83.7, 107, 109, 109, 96.5)
y <- c(32.22, 29.31, 64.75, 55.48, 81.29, 75.08, 166, 192, 137, 144)
plot(x, y, main = "Scatter Plot", xlab = "x", ylab = "y", pch = 20)
#4
x <- c(73.2, 75.9, 82.5, 79, 79.3, 83.7, 107, 109, 109, 96.5)
y <- c(32.22, 29.31, 64.75, 55.48, 81.29, 75.08, 166, 192, 137, 144)
mean_x <- mean(x)
mean_y <- mean(y)
beta1 <- sum((x - mean_x) * (y - mean_y)) / sum((x - mean_x)^2)
beta0 <- mean_y - beta1 * mean_x
plot_range <- range(0, x, y)
plot(1, type = "n", xlim = c(min(x) - 10, max(x) + 10), ylim = plot_range, xlab = "X-axis", ylab = "Y-axis", main = "Scatter Plot with Regression Line")
points(x, y, pch = 19)
x_vals <- seq(min(x) - 10, max(x) + 10, length.out = 100)
y_vals <- beta0 + beta1 * x_vals
lines(x_vals, y_vals, col = "green", lty = 2)
