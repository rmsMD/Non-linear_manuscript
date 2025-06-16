
set.seed(124)

# Simulate data
# major complications after surgery
n <- 5000 
age <- round(rnorm(n, mean = 50, sd = 12),1)
bmi <- round(rnorm(n, mean = 25, sd = 4),1) 
sex <- factor(sample(c("Male", "Female"), n, replace = TRUE))
smoking <- factor(
        sample(c("Never", "Former", "Current"), n, replace = TRUE),
        levels = c("Never", "Former", "Current")  # sets "Never" as the reference
)

# Simulate linear predictor. Age linear, BMI U-shaped, smoking increases risk.
lp <- -3 + 0.02 * age + 0.0045 * (bmi - 25.5)^2 +
        0.75 * (smoking == "Current")

# Convert linear predictor to probability and simulate major complication
p <- 1 / (1 + exp(-lp))
majorcomplication <- rbinom(n, 1, prob = p)

# Create data frame and clean environment
data <- data.frame(age = age, bmi = bmi, sex = sex, smoking = smoking, majorcomplication = majorcomplication)
rm(list = setdiff(ls(), "data"))
