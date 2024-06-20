# Load necessary libraries
library(fredr)
library(dplyr)
library(zoo)
library(sandwich)
library(lmtest)
library(ggplot2)

# Set FRED API key (assuming it is already set)
# fredr_set_key("your_fred_api_key_here")

# Define the start and end dates
start_date <- as.Date("1955-01-01")
end_date <- as.Date("1988-01-01")

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

# Merge the data frames to create the spread
spread_data <- left_join(t10yr_quarterly, t3m_quarterly, by = "date") %>%
  mutate(spread = t10yr - t3m)

# Merge GNP data with the spread data
final_data <- left_join(gnp_data, spread_data, by = "date")

# Calculate the 7.5th and 92.5th percentiles for the yield spread
percentiles <- quantile(final_data$spread, probs = c(0.05, 0.95), na.rm = TRUE)
lower_threshold <- percentiles[1]
upper_threshold <- percentiles[2]

# Create the dummy variable for extreme yield spread
final_data <- final_data %>%
  mutate(extreme_dummy = ifelse(spread <= lower_threshold | spread >= upper_threshold, 1, 0))

# Define horizons
horizons <- c(4, 6)

# Initialize a list to store regression results and fitted values
regression_results <- list()

# Loop through each horizon
for (k in horizons) {
  
  # Calculate cumulative GNP growth for the given horizon
  final_data <- final_data %>%
    mutate(gdp_growth = (400 / k) * (log(lead(value, k)) - log(value)))
  
  # Drop NA values created by leading
  reg_data <- final_data %>% filter(!is.na(gdp_growth))
  
  # Run the regression
  model <- lm(gdp_growth ~ spread + spread:extreme_dummy, data = reg_data)
  
  # Predict fitted values
  reg_data <- reg_data %>%
    mutate(fitted_gdp_growth = predict(model, newdata = reg_data))
  
  # Print the model summary
  cat("Horizon:", k, "\n")
  print(summary(model))
  
  # Adjust standard errors using Newey-West correction
  nw_se <- coeftest(model, vcov = NeweyWest(model, lag = k-1))
  
  # Store the summary of the regression results and fitted values
  regression_results[[paste("Horizon", k)]] <- list(
    summary = summary(model),
    nw_se = nw_se,
    reg_data = reg_data
  )
  
  # Plot actual vs fitted GDP growth
  p <- ggplot(reg_data, aes(x = date)) +
    geom_line(aes(y = gdp_growth, color = "Actual GDP Growth")) +
    geom_line(aes(y = fitted_gdp_growth, color = "Fitted GDP Growth")) +
    labs(title = paste("Actual vs Fitted GDP Growth for Horizon", k),
         x = "Date",
         y = "GDP Growth",
         color = "Legend") +
    theme_minimal()
  
  print(p)
}

# Print the results for each horizon
for (horizon in names(regression_results)) {
  cat("Results for", horizon, ":\n")
  print(regression_results[[horizon]]$summary)
  print(regression_results[[horizon]]$nw_se)
  cat("\n\n")
}
