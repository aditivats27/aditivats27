# Install and load necessary packages
if (!requireNamespace("DanielBiostatistics10th", quietly = TRUE)) {
  install.packages("DanielBiostatistics10th")
}

if (!requireNamespace("e1071", quietly = TRUE)) {
  install.packages("e1071")
}

if (!requireNamespace("moments", quietly = TRUE)) {
  install.packages("moments")
}

library(DanielBiostatistics10th)
library(e1071)
library(moments)

# Load datasets
data_set_1 <- LDS_C02_NCBIRTH800
data_set_2 <- LDS_C10_RISKFACT

# (A)

# Mean
print("The MEAN of the given datasets are as follows: ")
sapply(data_set_1[, c("mage", "weeks", "gained", "tounces", "tgrams")], FUN = mean, na.rm = TRUE)

# Median
print("The MEDIAN of the given datasets are as follows: ")
sapply(data_set_1[, c("mage", "weeks", "gained", "tounces", "tgrams")], FUN = median, na.rm = TRUE)

# Standard deviation
print("The STANDARD DEVIATIONS of the given datasets are as follows: ")
sapply(data_set_1[, c("mage", "weeks", "gained", "tounces", "tgrams")], FUN = sd, na.rm = TRUE)

# IQR
print("The IQR of the given datasets are as follows: ")
sapply(data_set_1[, c("mage", "weeks", "gained", "tounces", "tgrams")], FUN = IQR, na.rm = TRUE)

# Range
print("The RANGE of the given datasets are as follows: ")
sapply(data_set_1[, c("mage", "weeks", "gained", "tounces", "tgrams")], FUN = function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE))

# (B)

# Histograms
hist(data_set_1$mage, col = "Green")
hist(data_set_1$weeks, col = "Pink")
hist(data_set_1$gained, col = "yellow")
hist(data_set_1$tounces, col = "red")
hist(data_set_1$tgrams, col = "Cyan")

# (C)
print("It is pretty evident that for both the TOUNCES & TGRAMS data sets the data is skewed to the left i.e., NEGATIVELY SKEWED as their mean values are less than their median values.")

# (D)

# Box & Whisker Plots
boxplot(data_set_1$mage, main = "Box & Whisker Plot for mothers' ages (in years) [MAGE]", col = "Green")
boxplot(data_set_1$weeks, main = "Box & Whisker Plot for individual gestation period (in weeks) [WEEKS]", col = "blue")
boxplot(data_set_1$gained, main = "Box & Whisker Plot for weight gained during pregnancy (in pounds) [GAINED]", col = "Green")
boxplot(data_set_1$tounces, main = "Box & Whisker Plot for weight of child born (in ounces) [TOUNCES]", col = "red")
boxplot(data_set_1$tgrams, main = "Box & Whisker Plot for weight of child born (in grams) [TGRAMS]", col = "Cyan")

# (E)

boxplot(tounces ~ smoke, data_set_1, xlab = "Smoking Status (0 -> Nonsmoker/1 -> Smoker)", ylab = "Weight of child (in ounces)", col = "Green")
boxplot(tgrams ~ marital, data_set_1, xlab = "Marital Status (1 -> Married/2 -> Not Married)", ylab = "Weight of child (in grams)", col = "green")

# (G)

# Skewness & Kurtosis
print("The SKEWNESS of the given datasets are as follows: ")
sapply(data_set_1[, c("mage", "weeks", "gained", "tounces", "tgrams")], FUN = skewness, na.rm = TRUE)
print("The SKEWNESS of MAGE dataset is close to zero which means that the data is approximately normally distributed.")
print("The SKEWNESS of WEEKS dataset is negative which means that the MEAN is less than the MEDIAN.")
print("The SKEWNESS of GAINED dataset is positive which means that the MEAN is greater than the MEDIAN.")
print("The SKEWNESS of TOUNCES dataset is negative which means that the MEAN is less than the MEDIAN.")
print("The SKEWNESS of TGRAMS dataset is negative which means that the MEAN is less than the MEDIAN.")

# Note: Make sure you have a proper definition for the 'kurtosis' function or use a package that provides it.

# 2:

# (A)
male_percentage <- sum(data_set_1$sex == 1, na.rm = TRUE) / nrow(data_set_1)
standard_error <- sqrt(male_percentage * (1 - male_percentage) / nrow(data_set_1))
confidence_95 <- qnorm(c(0.025, 0.975)) * standard_error + male_percentage
cat("The 95 % confidence interval for the percentage of male children born is: [", confidence_95[1], ", ", confidence_95[2], "].\n")

# (B)
mean_age <- mean(data_set_1$mage, na.rm = TRUE)
standard_error <- sd(data_set_1$mage, na.rm = TRUE) / sqrt(nrow(data_set_1))
confidence_95 <- qnorm(c(0.025, 0.975)) * standard_error + mean_age
cat("The 95 % confidence interval for the mean age of a mother giving birth is: [", confidence_95[1], ", ", confidence_95[2], "].\n")

# (C)
mean_weight_gained <- mean(data_set_1$gained, na.rm = TRUE)
standard_error <- sd(data_set_1$gained, na.rm = TRUE) / sqrt(nrow(data_set_1))
confidence_95 <- qnorm(c(0.025, 0.975)) * standard_error + mean_weight_gained
cat("The 95 % confidence interval for the mean weight gained by a woman during pregnancy: [", confidence_95[1], ", ", confidence_95[2], "].\n")

# (D)
smoker_percentage <- sum(data_set_1$smoke == 1, na.rm = TRUE) / nrow(data_set_1)
standard_error <- sqrt(smoker_percentage * (1 - smoker_percentage) / nrow(data_set_1))
confidence_95 <- qnorm(c(0.025, 0.975)) * standard_error + smoker_percentage
cat("The 95 % confidence interval for the percentage of mothers who admitted to be smoking during pregnancy is: [", confidence_95[1], ", ", confidence_95[2], "].\n")

# (E)
nonsmoker_mean_weight_gained <- mean(data_set_1$gained[data_set_1$smoke == 0], na.rm = TRUE)
nonsmoker_sd_weight_gained <- sd(data_set_1$gained[data_set_1$smoke == 0], na.rm = TRUE)
smoker_mean_weight_gained <- mean(data_set_1$gained[data_set_1$smoke == 1], na.rm = TRUE)
smoker_sd_weight_gained <- sd(data_set_1$gained[data_set_1$smoke == 1], na.rm = TRUE)

standard_error <- sqrt((nonsmoker_sd_weight_gained^2 / sum(data_set_1$smoke == 0, na.rm = TRUE)) +
                         (smoker_sd_weight_gained^2 / sum(data_set_1$smoke == 1, na.rm = TRUE)))

z_value <- qnorm(0.975)
confidence_95 <- c((smoker_mean_weight_gained - nonsmoker_mean_weight_gained) - (z_value * standard_error),
                   (smoker_mean_weight_gained - nonsmoker_mean_weight_gained) + (z_value * standard_error))

cat("The 95 % confidence interval for the difference in the average weight gained between smoking & nonsmoking mothers is: [", confidence_95[1], ", ", confidence_95[2], "].\n")

# (F)
married_mean_birth_weight <- mean(data_set_1$tgrams[data_set_1$marital == 1], na.rm = TRUE)
married_sd_birth_weight <- sd(data_set_1$tgrams[data_set_1$marital == 1], na.rm = TRUE)
unmarried_mean_birth_weight <- mean(data_set_1$tgrams[data_set_1$marital == 2], na.rm = TRUE)
unmarried_sd_birth_weight <- sd(data_set_1$tgrams[data_set_1$marital == 2], na.rm = TRUE)

standard_error <- sqrt((married_sd_birth_weight^2 / sum(data_set_1$marital == 1, na.rm = TRUE)) +
                         (unmarried_sd_birth_weight^2 / sum(data_set_1$marital == 2, na.rm = TRUE)))

z_value <- qnorm(0.975)
confidence_95 <- c((married_mean_birth_weight - unmarried_mean_birth_weight) - (z_value * standard_error),
                   (married_mean_birth_weight - unmarried_mean_birth_weight) + (z_value * standard_error))

cat("The 95 % confidence interval for the difference in the average birth weight in grams between married mothers & unmarried mothers is: [", confidence_95[1], ", ", confidence_95[2], "].\n")

# (G)

low_bw_percentage_married <- sum(data_set_1$low[data_set_1$marital == 1], na.rm = TRUE) / sum(data_set_1$marital == 1, na.rm = TRUE)
low_bw_percentage_unmarried <- sum(data_set_1$low[data_set_1$marital == 2], na.rm = TRUE) / sum(data_set_1$marital == 2, na.rm = TRUE)

difference <- low_bw_percentage_married - low_bw_percentage_unmarried

standard_error <- sqrt((low_bw_percentage_married + (1 - low_bw_percentage_married) / sum(data_set_1$marital == 1, na.rm = TRUE)) +
                         (low_bw_percentage_unmarried + (1 - low_bw_percentage_unmarried) / sum(data_set_1$marital == 2, na.rm = TRUE)))

z_value <- qnorm(0.975)
confidence_95 <- c(difference - (z_value * standard_error), difference + (z_value * standard_error))

cat("The 95 % confidence interval for the difference in the percentage of low birth weight babies born to married mothers & unmarried mothers is: [", confidence_95[1], ", ", confidence_95[2], "].\n")

# 3:

data_set_2 <- LDS_C10_RISKFACT
head(LDS_C10_RISKFACT)
set.seed(834)
sample_size <- 200
random_sample <- LDS_C10_RISKFACT[sample(seq_len(nrow(LDS_C10_RISKFACT)), size = sample_size), ]
model <- lm(Y ~ X1 + X2 + X3 + X4, data = random_sample)
summary(model)