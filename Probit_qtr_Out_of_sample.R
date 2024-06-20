# Load necessary libraries
library(fredr)
library(dplyr)
library(zoo)
library(sandwich)
library(lmtest)
library(ggplot2)
library(scales)
library(broom)
library(caret)  # For confusion matrix and related metrics
library(pROC)   # For ROC curve and AUC

# Set FRED API key (assuming it is already set)
# fredr_set_key("your_fred_api_key_here")

# Define the start and end dates
start_date <- as.Date("1988-01-01")
end_date <- as.Date("2021-01-01")

# Retrieve data from FRED
t10yr_data <- fredr(
  series_id = "GS10",
  observation_start = start_date,
  observation_end = end_date,
  frequency = "m"
)

t3m_data <- fredr(
  series_id = "TB3MS",
  observation_start = start_date,
  observation_end = end_date,
  frequency = "m"
)

# Retrieve NBER recession data from FRED
recession_data <- fredr(
  series_id = "USRECQ",
  observation_start = start_date,
  observation_end = end_date,
  frequency = "q"
)

# Convert monthly yields to quarterly by taking the average for each quarter
t10yr_quarterly <- t10yr_data %>%
  mutate(date = as.yearqtr(date)) %>%
  group_by(date) %>%
  summarize(t10yr = mean(value, na.rm = TRUE))

t3m_quarterly <- t3m_data %>%
  mutate(date = as.yearqtr(date)) %>%
  group_by(date) %>%
  summarize(t3m = mean(value, na.rm = TRUE))

# Prepare the recession data
recession_data <- recession_data %>%
  select(date, value) %>%
  rename(recession = value) %>%
  mutate(date = as.yearqtr(date))

# Merge the data frames to create the spread
spread_data <- left_join(t10yr_quarterly, t3m_quarterly, by = "date") %>%
  mutate(spread = t10yr - t3m)

# Merge recession data with the spread data
final_data <- left_join(spread_data, recession_data, by = "date")

# Calculate the 10th and 90th percentiles for the yield spread
percentiles <- quantile(final_data$spread, probs = c(0.1, 0.9), na.rm = TRUE)
lower_threshold <- percentiles[1]
upper_threshold <- percentiles[2]

# Create the dummy variable for extreme yield spread
final_data <- final_data %>%
  mutate(extreme_dummy = ifelse(spread <= lower_threshold | spread >= upper_threshold, 1, 0))

# Define the forecasting horizon (e.g., 4 quarters or 1 year)
forecast_horizon <- 4

# Shift the spread data to predict the recession 4 quarters ahead
final_data <- final_data %>%
  mutate(spread_lagged = lag(spread, forecast_horizon),
         extreme_dummy_lagged = lag(extreme_dummy, forecast_horizon))

# Drop NA values created by lagging
reg_data <- final_data %>% filter(!is.na(spread_lagged) & !is.na(extreme_dummy_lagged) & !is.na(recession))

# Split the data into training and testing sets
train_data <- reg_data %>% filter(date < as.yearqtr("2011-01-01"))
test_data <- reg_data %>% filter(date >= as.yearqtr("2011-01-01"))

# Run the probit regression on the training data
model <- glm(recession ~ spread_lagged, family = binomial(link = "probit"), data = train_data)

# Calculate McFadden's Pseudo R2
pseudo_r2_mcfadden <- function(model) {
  null_deviance <- model$null.deviance
  residual_deviance <- model$deviance
  pseudo_r2 <- 1 - (residual_deviance / null_deviance)
  return(pseudo_r2)
}

pseudo_r2 <- pseudo_r2_mcfadden(model)

# Print the model summary and Pseudo R2
cat("Probit Regression Model Summary (Training Data):\n")
print(summary(model))
cat("McFadden's Pseudo R2:", pseudo_r2, "\n\n")

# Add fitted probabilities and confidence intervals to the test_data
predictions <- predict(model, newdata = test_data, type = "link", se.fit = TRUE)
test_data <- test_data %>%
  mutate(fitted_prob = pnorm(predictions$fit),
         ci_lower = pnorm(predictions$fit - 1.96 * predictions$se.fit),
         ci_upper = pnorm(predictions$fit + 1.96 * predictions$se.fit))

# Confusion Matrix and related metrics
predicted_class <- ifelse(test_data$fitted_prob > 0.5, 1, 0)
conf_matrix <- confusionMatrix(factor(predicted_class), factor(test_data$recession))
cat("Confusion Matrix:\n")
print(conf_matrix)

# ROC Curve and AUC
roc_curve <- roc(test_data$recession, test_data$fitted_prob)
auc_value <- auc(roc_curve)
cat("AUC:", auc_value, "\n")

# Brier Score
brier_score <- mean((test_data$fitted_prob - test_data$recession)^2)
cat("Brier Score:", brier_score, "\n")

# RMSE
rmse <- sqrt(mean((test_data$fitted_prob - test_data$recession)^2))
cat("RMSE:", rmse, "\n")

# Convert dates for ggplot2
test_data$date <- as.Date(as.yearqtr(test_data$date))
recession_data$date <- as.Date(as.yearqtr(recession_data$date))

# Define breaks and labels for the x-axis
x_breaks <- seq(as.Date("2011-01-01"), as.Date("2021-01-01"), by = "2 years")
x_labels <- format(x_breaks, "%Y")

# Plot the data starting from the out-of-sample forecast period
ggplot() +
  geom_line(data = test_data, aes(x = date, y = fitted_prob), color = "blue", size = 1) +
  geom_ribbon(data = test_data, aes(x = date, ymin = ci_lower, ymax = ci_upper), fill = "blue", alpha = 0.2) +
  geom_line(data = test_data, aes(x = date, y = recession), color = "red", size = 1, linetype = "dashed") +
  geom_rect(data = recession_data, aes(xmin = date, xmax = date + 90,
                                       ymin = -Inf, ymax = Inf, fill = factor(recession)), alpha = 0.2) +
  scale_fill_manual(values = c("0" = "transparent", "1" = "grey")) +
  scale_x_date(limits = c(as.Date("2011-01-01"), NA), breaks = x_breaks, labels = x_labels) +
  labs(title = "Predicted Probability of Recession (Out-of-Sample)",
       x = "Date",
       y = "Probability",
       fill = "Recession") +
  theme_minimal() +
  theme(legend.position = "none")
