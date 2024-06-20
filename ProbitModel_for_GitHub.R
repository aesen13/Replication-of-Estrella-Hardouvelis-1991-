# Load necessary libraries
library(fredr)
library(dplyr)
library(zoo)
library(sandwich)
library(lmtest)
library(ggplot2)
library(scales)
library(broom)

# Set FRED API key (assuming it is already set)
# fredr_set_key("your_fred_api_key_here")

# Define the start and end dates
start_date <- as.Date("1955-01-01")
end_date <- as.Date("2021-01-01")

# Retrieve data from FRED
gnp_data <- fredr(
  series_id = "GDPC1",
  observation_start = start_date,
  observation_end = end_date,
  frequency = "q"
)

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

# Prepare the GNP data
gnp_data <- gnp_data %>%
  select(date, value) %>%
  arrange(date) %>%
  mutate(date = as.yearqtr(date))

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

# Merge GNP data with the spread data
final_data <- left_join(gnp_data, spread_data, by = "date")

# Merge recession data with the final data
final_data <- left_join(final_data, recession_data, by = "date")

# Calculate the 10th and 90th percentiles for the yield spread
percentiles <- quantile(final_data$spread, probs = c(0.01, 0.9), na.rm = TRUE)
lower_threshold <- percentiles[1]
upper_threshold <- percentiles[2]

# Create the dummy variable for extreme yield spread
final_data <- final_data %>%
  mutate(extreme_dummy = ifelse(spread <= lower_threshold | spread >= upper_threshold, 1, 0))

# ALLAH
final_data <- final_data %>%
  mutate(extreme_dummy = ifelse(spread <= lower_threshold, 1, 0))

# Define the spread lag (e.g., 4 quarters or 1 year)
forecast_horizon <- 4

# Shift the spread data to predict the recession 4 quarters ahead
final_data <- final_data %>%
  mutate(spread_lagged = lag(spread, forecast_horizon),
         extreme_dummy_lagged = lag(extreme_dummy, forecast_horizon))

# Drop NA values created by lagging
reg_data <- final_data %>% filter(!is.na(spread_lagged) & !is.na(extreme_dummy_lagged) & !is.na(recession))

# Run the probit regression
model <- glm(recession ~ spread_lagged, family = binomial(link = "probit"), data = reg_data)

# Calculate McFadden's Pseudo R2
pseudo_r2_mcfadden <- function(model) {
  null_deviance <- model$null.deviance
  residual_deviance <- model$deviance
  pseudo_r2 <- 1 - (residual_deviance / null_deviance)
  return(pseudo_r2)
}

pseudo_r2 <- pseudo_r2_mcfadden(model)

# Print the model summary and Pseudo R2
cat("Probit Regression Model Summary:\n")
print(summary(model))
cat("McFadden's Pseudo R2:", pseudo_r2, "\n\n")

# Add fitted probabilities to the reg_data
reg_data <- reg_data %>%
  mutate(fitted_prob = predict(model, type = "response"))

# Convert dates for ggplot2
reg_data$date <- as.Date(as.yearqtr(reg_data$date))
recession_data$date <- as.Date(as.yearqtr(recession_data$date))

# Define breaks and labels for the x-axis
x_breaks <- seq(as.Date("1960-01-01"), as.Date("2020-01-01"), by = "10 years")
x_labels <- format(x_breaks, "%Y")

# Plot the data
ggplot() +
  geom_line(data = reg_data, aes(x = date, y = fitted_prob), color = "blue") +
  geom_rect(data = recession_data, aes(xmin = date, xmax = date + 90,
                                       ymin = -Inf, ymax = Inf, fill = factor(recession)), alpha = 0.2) +
  scale_fill_manual(values = c("0" = "transparent", "2" = "grey")) +
  scale_x_date(breaks = x_breaks, labels = x_labels) +
  labs(title = "Predicted Probabilities of Recessions",
       x = "Date",
       y = "Probability",
       fill = "Recession") +
  theme_minimal() +
  theme(legend.position = "none")
