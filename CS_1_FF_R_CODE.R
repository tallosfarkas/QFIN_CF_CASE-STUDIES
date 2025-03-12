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






















# Load required packages
library(quantmod)
library(xts)
library(zoo)
library(dplyr)
library(ggplot2)

### 1. Download Data for MYGN (firm) and SPY (Market Portfolio Proxy)
# Download data from 2009-01-01 to 2024-12-31 so we have extra history for rolling calculations.
start_all <- "2009-01-01"
end_all   <- "2024-12-31"
getSymbols(c("MYGN", "SPY"), from = start_all, to = end_all)

# For simplicity, we use the adjusted closing prices:
MYGN_adj <- Ad(MYGN)
SPY_adj  <- Ad(SPY)

### 2. Create Return Series at Daily, Weekly and Monthly Frequencies
# Define a function to compute compound annual return given a return series and its start/end dates.
compound_annual_return <- function(ret_series) {
  # Use the first and last dates of the return series to compute the number of years:
  start_date <- index(ret_series)[1]
  end_date   <- index(ret_series)[length(ret_series)]
  years      <- as.numeric(difftime(end_date, start_date, units = "days"))/365.25
  cum_return <- prod(1 + ret_series)  # cumulative growth factor
  annual_compound <- cum_return^(1/years) - 1
  return(annual_compound)
}

# Create return series for the estimation window 2019-01-01 to 2024-12-31:
start_est <- "2019-01-01"
end_est   <- "2024-12-31"

# Subset prices for the estimation window
MYGN_est <- MYGN_adj[paste0(start_est, "/", end_est)]
SPY_est  <- SPY_adj[paste0(start_est, "/", end_est)]

# Daily returns
MYGN_daily <- dailyReturn(MYGN_est)
SPY_daily  <- dailyReturn(SPY_est)

# Weekly returns: use endpoints to get weekly prices and then compute returns.
MYGN_weekly <- period.apply(MYGN_est, endpoints(MYGN_est, on = "weeks"), last)
SPY_weekly  <- period.apply(SPY_est, endpoints(SPY_est, on = "weeks"), last)
MYGN_weekly_returns <- weeklyReturn(MYGN_est, type = "arithmetic")
SPY_weekly_returns  <- weeklyReturn(SPY_est, type = "arithmetic")

# Monthly returns
MYGN_monthly <- period.apply(MYGN_est, endpoints(MYGN_est, on = "months"), last)
SPY_monthly  <- period.apply(SPY_est, endpoints(SPY_est, on = "months"), last)
MYGN_monthly_returns <- monthlyReturn(MYGN_est, type = "arithmetic")
SPY_monthly_returns  <- monthlyReturn(SPY_est, type = "arithmetic")

### 3. Compute Performance Metrics for 1/1/2019 - 12/31/2024
# Define trading frequencies:
freqs <- list(Daily = 252, Weekly = 52, Monthly = 12)

# Function to compute arithmetic annual return
arithmetic_annual_return <- function(ret_series, freq) {
  mean(ret_series) * freq
}

# For compound annual return, use our defined function above.
# Prepare a summary table for each frequency and each series.
metrics <- data.frame(Frequency = character(), 
                      Series = character(), 
                      Arithmetic_Return = numeric(),
                      Compound_Return = numeric(),
                      Volatility = numeric(),
                      stringsAsFactors = FALSE)

# For volatility in part (b), note the window is 1/1/2019 to 12/21/2024.
end_vol <- "2024-12-21"
MYGN_vol_est <- MYGN_est[paste0(start_est, "/", end_vol)]
SPY_vol_est  <- SPY_est[paste0(start_est, "/", end_vol)]

# Compute volatility for each frequency:
# Daily volatility annualized = sd(daily returns)*sqrt(252)
# Weekly volatility annualized = sd(weekly returns)*sqrt(52)
# Monthly volatility annualized = sd(monthly returns)*sqrt(12)

# Daily metrics:
ret_MYGN_daily <- dailyReturn(MYGN_est)
ret_SPY_daily  <- dailyReturn(SPY_est)
vol_MYGN_daily <- sd(ret_MYGN_daily) * sqrt(freqs$Daily)
vol_SPY_daily  <- sd(ret_SPY_daily) * sqrt(freqs$Daily)
metrics <- rbind(metrics,
                 data.frame(Frequency = "Daily", Series = "MYGN",
                            Arithmetic_Return = arithmetic_annual_return(ret_MYGN_daily, freqs$Daily),
                            Compound_Return = compound_annual_return(ret_MYGN_daily),
                            Volatility = vol_MYGN_daily),
                 data.frame(Frequency = "Daily", Series = "SPY",
                            Arithmetic_Return = arithmetic_annual_return(ret_SPY_daily, freqs$Daily),
                            Compound_Return = compound_annual_return(ret_SPY_daily),
                            Volatility = vol_SPY_daily)
)

# Weekly metrics:
ret_MYGN_weekly <- weeklyReturn(MYGN_est, type = "arithmetic")
ret_SPY_weekly  <- weeklyReturn(SPY_est, type = "arithmetic")
vol_MYGN_weekly <- sd(ret_MYGN_weekly) * sqrt(freqs$Weekly)
vol_SPY_weekly  <- sd(ret_SPY_weekly) * sqrt(freqs$Weekly)
metrics <- rbind(metrics,
                 data.frame(Frequency = "Weekly", Series = "MYGN",
                            Arithmetic_Return = arithmetic_annual_return(ret_MYGN_weekly, freqs$Weekly),
                            Compound_Return = compound_annual_return(ret_MYGN_weekly),
                            Volatility = vol_MYGN_weekly),
                 data.frame(Frequency = "Weekly", Series = "SPY",
                            Arithmetic_Return = arithmetic_annual_return(ret_SPY_weekly, freqs$Weekly),
                            Compound_Return = compound_annual_return(ret_SPY_weekly),
                            Volatility = vol_SPY_weekly)
)

# Monthly metrics:
ret_MYGN_monthly <- monthlyReturn(MYGN_est, type = "arithmetic")
ret_SPY_monthly  <- monthlyReturn(SPY_est, type = "arithmetic")
vol_MYGN_monthly <- sd(ret_MYGN_monthly) * sqrt(freqs$Monthly)
vol_SPY_monthly  <- sd(ret_SPY_monthly) * sqrt(freqs$Monthly)
metrics <- rbind(metrics,
                 data.frame(Frequency = "Monthly", Series = "MYGN",
                            Arithmetic_Return = arithmetic_annual_return(ret_MYGN_monthly, freqs$Monthly),
                            Compound_Return = compound_annual_return(ret_MYGN_monthly),
                            Volatility = vol_MYGN_monthly),
                 data.frame(Frequency = "Monthly", Series = "SPY",
                            Arithmetic_Return = arithmetic_annual_return(ret_SPY_monthly, freqs$Monthly),
                            Compound_Return = compound_annual_return(ret_SPY_monthly),
                            Volatility = vol_SPY_monthly)
)

# Display the summary metrics
print(metrics)

### 4. Rolling 2-Year (104-Week) Return Volatility and Correlation (Weekly Data)
# For this, we use weekly returns over a longer period from 1/1/2009 to 12/31/2024.
MYGN_weekly_full <- weeklyReturn(MYGN_adj, type = "arithmetic")
SPY_weekly_full  <- weeklyReturn(SPY_adj, type = "arithmetic")

# Align the two series over the period 2009-01-01 to 2024-12-31
MYGN_weekly_full <- MYGN_weekly_full["2009/2024-12-31"]
SPY_weekly_full  <- SPY_weekly_full["2009/2024-12-31"]

# Calculate rolling 104-week volatility (annualized) for each series.
roll_vol_MYGN <- rollapply(MYGN_weekly_full, width = 104, FUN = function(x) sd(x)*sqrt(freqs$Weekly), 
                           align = "right", fill = NA)
roll_vol_SPY  <- rollapply(SPY_weekly_full, width = 104, FUN = function(x) sd(x)*sqrt(freqs$Weekly), 
                           align = "right", fill = NA)

# Combine into a data frame for plotting volatility.
roll_vol_df <- data.frame(Date = index(roll_vol_MYGN),
                          MYGN_Vol = coredata(roll_vol_MYGN),
                          SPY_Vol  = coredata(roll_vol_SPY))

# Plot the rolling volatility
p1 <- ggplot(roll_vol_df, aes(x = Date)) +
  geom_line(aes(y = MYGN_Vol, color = "MYGN")) +
  geom_line(aes(y = SPY_Vol, color = "Market (SPY)")) +
  labs(title = "Rolling 2-Year (104 Weeks) Annualized Volatility",
       y = "Annualized Volatility", x = "Date", color = "Series")
print(p1)

# Rolling 104-week correlation between MYGN and SPY weekly returns.
roll_corr <- rollapply(data = merge(MYGN_weekly_full, SPY_weekly_full), width = 104, 
                       FUN = function(z) cor(z[,1], z[,2]), align = "right", fill = NA)

roll_corr_df <- data.frame(Date = index(roll_corr),
                           Rolling_Correlation = coredata(roll_corr))

# Plot the rolling correlation
p2 <- ggplot(roll_corr_df, aes(x = Date, y = Rolling_Correlation)) +
  geom_line(color = "blue") +
  labs(title = "Rolling 2-Year (104 Weeks) Return Correlation: MYGN vs Market (SPY)",
       y = "Correlation", x = "Date")
print(p2)



