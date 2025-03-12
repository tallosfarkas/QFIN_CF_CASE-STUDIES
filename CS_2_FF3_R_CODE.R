# Load required packages
library(quantmod)
library(dplyr)
library(zoo)
library(lubridate)
library(ggplot2)

# ------------------------------
# 1. Get MYGN Daily Data and Convert to Weekly Prices/Returns
# ------------------------------

# Download MYGN data from Yahoo Finance (daily data)
getSymbols("MYGN", from = "2019-01-01", to = "2024-12-31")

# Use adjusted prices
mygn_adj <- Ad(MYGN)

# Create weekly prices using endpoints (the last price in each week)
ep <- endpoints(mygn_adj, on = "weeks")
mygn_weekly <- period.apply(mygn_adj, INDEX = ep, FUN = last)

# Calculate weekly arithmetic returns
mygn_weekly_returns <- weeklyReturn(mygn_weekly, type = "arithmetic")

# Convert the weekly returns xts object to a data frame with Date column
mygn_returns_df <- data.frame(Date = index(mygn_weekly_returns),
                              mygn_return = coredata(mygn_weekly_returns))

# ------------------------------
# 2. Download and Prepare Fama–French Weekly Factors
# ------------------------------

# Define URL for the Fama–French weekly factors dataset
ffFactorsWeeklyURL <- "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_weekly_CSV.zip"

# Download the ZIP file directly into the working directory
download.file(ffFactorsWeeklyURL, destfile = "F-F_Research_Data_Factors_weekly_CSV.zip", mode = "wb")

# Unzip the downloaded file into a folder named "FF_factors_weekly"
unzip("F-F_Research_Data_Factors_weekly_CSV.zip", exdir = "FF_factors_weekly")

# List files in the extracted directory (optional)
list.files("FF_factors_weekly")

# Read the CSV file, skipping the first 3 rows of header text
ffFactorsWeekly_raw <- read.csv("FF_factors_weekly/F-F_Research_Data_Factors_weekly.CSV", skip = 3)

# Remove any footer rows (keep only rows where the first column starts with a digit)
ffFactorsWeekly_raw <- ffFactorsWeekly_raw %>% 
  filter(grepl("^[0-9]", X)) %>%
  rename(Date = X)

# Convert the date column to Date type.
# Here, the date is assumed to be in YYYYMMDD format.
ffFactorsWeekly_raw$Date <- as.Date(as.character(ffFactorsWeekly_raw$Date), format = "%Y%m%d")

# Reorder columns to put Date first
ffFactorsWeekly <- ffFactorsWeekly_raw[, c("Date", setdiff(names(ffFactorsWeekly_raw), "Date"))]

ffFactorsWeekly <- ffFactorsWeekly %>% 
  mutate(`Mkt.RF` = as.numeric(as.character(`Mkt.RF`)) / 100,
         SMB      = as.numeric(as.character(SMB)) / 100,
         HML      = as.numeric(as.character(HML)) / 100,
         RF       = as.numeric(as.character(RF)) / 100)

# Optionally, display the first few rows
head(ffFactorsWeekly)

# ------------------------------
# 3. Merge MYGN Returns with Fama–French Factors
# ------------------------------

# Merge by Date (inner join ensures matching dates)
merged_data <- merge(mygn_returns_df, ffFactorsWeekly, by = "Date")

# Compute MYGN excess returns: firm return minus the risk-free rate (RF)
merged_data <- merged_data %>% mutate(mygn_excess = weekly.returns - RF)

# ------------------------------
# 4. Rolling Regression with a 104-Week Window
# ------------------------------

# We will run a rolling regression of mygn_excess on the Fama–French factors:
# Dependent variable: mygn_excess
# Independent variables: Mkt-RF (renamed as Mkt.RF), SMB, and HML
# For easier referencing in regression, rename the market factor column:
merged_data <- merged_data %>% rename(Mkt.RF = `Mkt.RF`)

# Select required columns and convert to a zoo object (excluding Date)
rolling_data <- merged_data %>% select(Date, mygn_excess, Mkt.RF, SMB, HML)
z_rolling <- zoo(rolling_data[,-1], order.by = rolling_data$Date)

# Define a function to run the regression over a window and return coefficients
rolling_regression <- function(window_data) {
  df_window <- as.data.frame(window_data)
  model <- lm(mygn_excess ~ Mkt.RF + SMB + HML, data = df_window)
  return(coef(model))
}

# Apply rollapply with a 104-week window (rolling regression aligned at the window's end)
roll_coefs <- rollapply(z_rolling, width = 104, FUN = rolling_regression,
                        by.column = FALSE, align = "right")

# Convert the result to a data frame and add the corresponding Date (last observation of each window)
roll_coefs_df <- data.frame(Date = index(roll_coefs), coredata(roll_coefs))
names(roll_coefs_df) <- c("Date", "Alpha", "Beta_Mkt", "Beta_SMB", "Beta_HML")

# ------------------------------
# 5. Output and Plotting
# ------------------------------

# Display the first few rows of the rolling coefficients
print(head(roll_coefs_df))

ggplot(roll_coefs_df, aes(x = Date, y = Beta_Mkt)) +
  geom_line() +
  labs(title = "Rolling Fama–French Market Beta for MYGN (104-week window)",
       x = "Date", y = "Market Beta")





# ------------------------------
# Compare CAPM vs. Fama-French Market Beta Estimates
# ------------------------------

# CAPM Regression: MYGN excess returns on market excess returns only
capm_model <- lm(mygn_excess ~ Mkt.RF, data = merged_data)
capm_beta <- coef(capm_model)["Mkt.RF"]
capm_ci <- confint(capm_model, "Mkt.RF", level = 0.95)

# Fama-French Regression: MYGN excess returns on market, SMB, and HML
ff_model <- lm(mygn_excess ~ Mkt.RF + SMB + HML, data = merged_data)
ff_beta <- coef(ff_model)["Mkt.RF"]
ff_ci <- confint(ff_model, "Mkt.RF", level = 0.95)

# Print the beta estimates and their 95% confidence intervals
cat("CAPM Regression:\n")
cat("Market Beta:", round(capm_beta, 4), "\n")
cat("95% CI:", round(capm_ci[1], 4), "to", round(capm_ci[2], 4), "\n\n")

cat("Fama-French Regression:\n")
cat("Market Beta:", round(ff_beta, 4), "\n")
cat("95% CI:", round(ff_ci[1], 4), "to", round(ff_ci[2], 4), "\n")

# ------------------------------
# Plot the Comparison with Error Bars
# ------------------------------

# Create a data frame for plotting
beta_df <- data.frame(
  Model = c("CAPM", "Fama-French"),
  Beta = c(capm_beta, ff_beta),
  Lower = c(capm_ci[1], ff_ci[1]),
  Upper = c(capm_ci[2], ff_ci[2])
)

# Plot using ggplot2
library(ggplot2)
ggplot(beta_df, aes(x = Model, y = Beta)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2) +
  labs(title = "Comparison of Market Beta Estimates",
       x = "Model",
       y = "Market Beta (95% CI)") +
  theme_minimal()
