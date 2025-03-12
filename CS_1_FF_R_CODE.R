# Load required packages
library(dplyr)
library(lubridate)
library(zoo)

# ------------------------------
# 1. Download and Prepare the Data
# ------------------------------

# URL for the Fama-French 3-factor CSV (monthly data)
ff_url <- "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_CSV.zip"

# Download and unzip temporary file
temp <- tempfile()
download.file(ff_url, temp, mode = "wb")

# The zipped file contains "F-F_Research_Data_Factors.CSV". 
# Read the file while skipping the first 3 header lines.
raw_data <- read.csv(unz(temp, "F-F_Research_Data_Factors.CSV"), header = TRUE, skip = 3)

# Remove the last row(s) if they do not contain proper data (the file often includes footer text)
raw_data <- raw_data %>% 
  filter(grepl("^[0-9]", X))  # Keep rows where the first column starts with a number

# Rename the first column to "Date" and convert it to character
raw_data <- raw_data %>% 
  rename(Date = X)

# Remove temporary file
unlink(temp)

# The Date column is in the format YYYYMM.
# Create year and month variables and a proper Date column (using the first day of each month).
ff_data <- raw_data %>% 
  mutate(Year = as.numeric(substr(Date, 1, 4)),
         Month = as.numeric(substr(Date, 5, 6)),
         Date = as.Date(paste(Year, Month, "01", sep = "-"))) %>%
  # Convert returns (given in percent) to decimals:
  mutate_at(vars(`Mkt.RF`, RF, SMB, HML), ~ as.numeric(as.character(.)) / 100) %>%
  # Filter data for 1927 to 2024
  filter(Year >= 1927, Year <= 2024) %>%
  arrange(Date)

# Compute the actual market return = (Mkt-RF + RF)
ff_data <- ff_data %>% 
  mutate(Market = `Mkt.RF` + RF)

# ------------------------------
# 2. Define Functions for Rolling Returns and Bootstrap
# ------------------------------

# Function to compute cumulative return over a window of length n months
# Given a vector of monthly returns, it returns: prod(1+returns) - 1.
rolling_cum_return <- function(returns, window) {
  rollapply(returns, width = window, FUN = function(x) prod(1 + x) - 1, align = "left", fill = NA)
}

# Function to annualize a cumulative return over a period of 'years'
annualize_return <- function(cum_return, years) {
  (1 + cum_return)^(1/years) - 1
}

# Bootstrap function to compute standard error of the mean of returns.
# 'x' is a vector of window returns.
bootstrap_se <- function(x, B = 1000, seed = 123) {
  set.seed(seed)
  boot_means <- replicate(B, mean(sample(x, length(x), replace = TRUE), na.rm = TRUE))
  sd(boot_means)
}

# ------------------------------
# 3. Compute Overlapping Window Returns
# ------------------------------

# Define window lengths in months for 1, 10 and 20 years:
window_lengths <- c("1-year" = 12, "10-year" = 120, "20-year" = 240)

# Create an empty list to store results for Market and RF (risk-free)
results <- list()

for(name in names(window_lengths)) {
  n_months <- window_lengths[name]
  n_years <- n_months / 12
  
  # Compute rolling cumulative returns for the market portfolio
  cum_ret_market <- rolling_cum_return(ff_data$Market, n_months)
  # Compute rolling cumulative returns for the risk-free rate (RF)
  cum_ret_rf <- rolling_cum_return(ff_data$RF, n_months)
  
  # Remove NA's that appear when the window does not fully fit
  valid_idx <- !is.na(cum_ret_market) & !is.na(cum_ret_rf)
  cum_ret_market <- cum_ret_market[valid_idx]
  cum_ret_rf <- cum_ret_rf[valid_idx]
  
  # Compute average cumulative returns
  avg_cum_market <- mean(cum_ret_market)
  avg_cum_rf <- mean(cum_ret_rf)
  
  # For 10- and 20-year windows, annualize the average cumulative return.
  # (For 1-year, the cumulative return is already annual.)
  if(n_years > 1) {
    ann_market <- annualize_return(avg_cum_market, n_years)
    ann_rf <- annualize_return(avg_cum_rf, n_years)
  } else {
    ann_market <- avg_cum_market
    ann_rf <- avg_cum_rf
  }
  
  # Bootstrap standard error for the average cumulative return of the market portfolio.
  boot_se_market <- bootstrap_se(cum_ret_market)
  # And for the annualized returns (if >1 year, we can bootstrap the annualized returns too)
  if(n_years > 1) {
    # Compute annualized returns for each window and then bootstrap
    ann_returns_market <- annualize_return(cum_ret_market, n_years)
    boot_se_market_ann <- bootstrap_se(ann_returns_market)
  } else {
    boot_se_market_ann <- NA
  }
  
  results[[name]] <- list(
    n_months = n_months,
    n_years = n_years,
    avg_cum_return_market = avg_cum_market,
    avg_cum_return_rf = avg_cum_rf,
    annualized_return_market = ann_market,
    annualized_return_rf = ann_rf,
    bootstrap_se_market_cum = boot_se_market,
    bootstrap_se_market_ann = boot_se_market_ann
  )
}

# ------------------------------
# 4. Print and Compare the Results
# ------------------------------

for(name in names(results)) {
  cat("\n----------------------------\n")
  cat("Investment Duration:", name, "\n")
  cat("Window (months):", results[[name]]$n_months, "(", results[[name]]$n_years, "years )\n")
  cat("Market Portfolio:\n")
  cat("  Average cumulative return:", round(results[[name]]$avg_cum_return_market, 4), "\n")
  if(results[[name]]$n_years > 1) {
    cat("  Annualized return:", round(results[[name]]$annualized_return_market, 4), "\n")
  }
  cat("Risk-Free Rate:\n")
  cat("  Average cumulative return:", round(results[[name]]$avg_cum_return_rf, 4), "\n")
  if(results[[name]]$n_years > 1) {
    cat("  Annualized return:", round(results[[name]]$annualized_return_rf, 4), "\n")
  }
  cat("Bootstrap Standard Errors (Market):\n")
  cat("  Cumulative return SE:", round(results[[name]]$bootstrap_se_market_cum, 4), "\n")
  if(results[[name]]$n_years > 1) {
    cat("  Annualized return SE:", round(results[[name]]$bootstrap_se_market_ann, 4), "\n")
  }
}






