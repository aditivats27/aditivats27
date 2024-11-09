n <- 10
p <- 0.3
x <- 0:n
pmf <- dbinom(x, size =n, prob =p)
barplot(pmf, names.arg = x, xlab = "Number of successes", ylab = "Probability", main = "Binomial PMF", col = "blue")
n <- 10
p <- 0.5
x <- 0:n
cdf <- pbinom(x, size=n, prob = p)
plot(x, cdf , type = "s", xlab = "Number of sucesses", ylab = "Cumulative Probability", "Binomial CDF", col = "blue")
num_rolls <- 64
sum_of_sevens <- sum(replicate(num_rolls, {
  die1 <- sample(1:6, 1, replace = TRUE)
  die2 <- sample(1:6, 1, replace = TRUE)
  if (die1 + die2 == 7) 1 else 0
}))
cat("Number of times the sum of 7 appered:", sum_of_sevens)
n <- 100
p <- 0.5
num_samples <- 10
random_numbers <- rbinom(num_samples, size = n, prob = p)
print(random_numbers)
lambda <- 3
x <- 0:10
pmf <- dpois(x, lambda)
barplot(pmf, names.arg = x, xlab = "Number of Events", ylab = "Probability PMF", col = "blue")
lambda <- 3
x <- 0:10
cdf <- ppois(x, lambda)
plot(x, cdf, type = "s", xlab = "Number of Events", ylab = "Cumulative Probability", main = "Poisson CDF", col = "blue")
lambda <- 3
num_samples <- 100
random_samples <- rpois(num_samples, lambda)
print(random_samples)
lambda <- 3
target_value <- 2
num_trials <- 1000
occurence_count <- 0
for (i in 1:num_trials) {
  random_sample <- rpois(1,lambda)
  if (random_sample == target_value) {
    occurence_count <- occurence_count + 1
  }
}
cat("Number of times", target_value, "appeared:", occurence_count, "\n")
n_binom <- 10
p_binom <- 0.3
lambda_poisson <- 3
x <- 0:15
pmf_binom <- dbinom(x, size = n_binom, prob = p_binom)
pmf_poisson <- dpois(x, lambda_poisson)
barplot(cbind(pmf_binom, pmf_poisson), beside = TRUE,
        names.arg = c(paste("Binomial (n=", n_binom, ", p=", p_binom, ")"),
                      paste("Poisson (lambda=", lambda_poisson, ")")),
        xlab = "Number of Events", ylab = "Probability",
        main = "Binomial vs. Poisson PMF", col = c("blue", "red"),
        legend.text = c("Binomial", "Poisson"))
legend("topright", legend = c("Binomial", "Poisson"), fill = c("blue", "red"))