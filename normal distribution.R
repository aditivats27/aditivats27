

mean <- 20
variance <- 4


std_deviation <- sqrt(variance)

x <- seq(mean - 4 * std_deviation, mean + 4 * std_deviation, length.out = 100)


cdf_values <- pnorm(x, mean, std_deviation)

plot(x, cdf_values, type = "l", xlab = "X-axis", ylab = "CDF Value",
     main = "Cumulative Distribution Function for Normal Distribution")
grid()

mean <- 20
std_deviation <- 4


num_samples <- 1000
simulated_data <- rnorm(num_samples, mean, std_deviation)
hist(simulated_data, breaks = 30, main = "Simulated Normal Distribution",
     xlab = "Value", ylab = "Frequency", col = "lightblue")
abline(v = mean, col = "red", lwd = 2)
z_values <- seq(-3,3,by=0.1)
cumulative_probs<-pnorm(z_values)
z_table<- data.frame(Z_Value= z_values,Cumulative_Probability = cumulative_probs)
print(z_table)