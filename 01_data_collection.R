# ECOM6003 - Data Collection and Preparation
# Section 2.1: Data Collection and Stock Selection

# Install packages once if needed:
# install.packages(c("quantmod", "dplyr", "tibble", "xts"))

library(quantmod)
library(dplyr)
library(tibble)
library(xts)

# 15 candidate stocks
tickers <- c(
  "AAPL", "MSFT", "NVDA", "JPM", "V",
  "AMZN", "HD", "GOOGL", "JNJ", "CAT",
  "PG", "XOM", "LIN", "PLD", "NEE"
)

# Download Yahoo Finance data for the 2020-2024 in-sample period.
# Using to = 2025-01-01 ensures the sample ends at 2024-12-31.
getSymbols(
  Symbols = tickers,
  src = "yahoo",
  from = "2020-01-01",
  to = "2025-01-01",
  auto.assign = TRUE
)

# Extract adjusted closing prices
adjusted_list <- lapply(tickers, function(ticker) {
  Ad(get(ticker))
})

adjusted_prices <- do.call(merge, adjusted_list)
colnames(adjusted_prices) <- tickers

# Align common trading dates
aligned_prices <- na.omit(adjusted_prices)

cat("\n--- PRICE DATA CHECK ---\n")
cat("First retained date:", as.character(first(index(aligned_prices))), "\n")
cat("Last retained date :", as.character(last(index(aligned_prices))), "\n")
cat("Aligned price observations:", nrow(aligned_prices), "\n\n")

# Missing values after alignment
missing_by_stock <- colSums(is.na(aligned_prices))
print(missing_by_stock)

# Data completeness across retained common trading dates
common_dates <- nrow(aligned_prices)

completeness <- tibble(
  Ticker = tickers,
  Available_Observations = sapply(
    seq_along(tickers),
    function(i) sum(!is.na(aligned_prices[, i]))
  ),
  Common_Trading_Dates = common_dates
) %>%
  mutate(
    Data_Completeness_Percent =
      100 * Available_Observations / Common_Trading_Dates
  )

cat("\n--- DATA COMPLETENESS ---\n")
print(completeness)

# Calculate discrete daily returns
daily_returns <- na.omit(
  ROC(aligned_prices, type = "discrete")
)

cat("\n--- RETURN DATA CHECK ---\n")
cat("Aligned daily return observations:", nrow(daily_returns), "\n")
cat("First return date:", as.character(first(index(daily_returns))), "\n")
cat("Last return date :", as.character(last(index(daily_returns))), "\n\n")

# Save outputs
if (!dir.exists("output")) {
  dir.create("output")
}

write.csv(
  completeness,
  "output/data_completeness.csv",
  row.names = FALSE
)

write.csv(
  data.frame(
    Date = index(aligned_prices),
    coredata(aligned_prices)
  ),
  "output/aligned_adjusted_prices_2020_2024.csv",
  row.names = FALSE
)

write.csv(
  data.frame(
    Date = index(daily_returns),
    coredata(daily_returns)
  ),
  "output/aligned_daily_returns_2020_2024.csv",
  row.names = FALSE
)

cat("Files saved in the output/ folder.\n")
cat("\nIMPORTANT:\n")
cat("Report the observation counts produced by YOUR run.\n")
cat("Do not manually change the output to force 1,258 or 1,257 observations.\n")
