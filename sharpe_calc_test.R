# Sharpe ratio calculation
# from ChatGPT

# ---- Setup ----
packages <- c("tidyquant", "PerformanceAnalytics", "xts", "dplyr")
invisible(lapply(packages, function(p) if (!require(p, character.only = TRUE)) install.packages(p)))
lapply(packages, library, character.only = TRUE)

# ---- Parameters ----
tickers <- c("DFIV", "DFAI", "DFAX","DFAE")
start_date <- "2020-01-01"
end_date <- Sys.Date()

# Risk-free rate (annualized)
Rf_annual <- 0.045   # e.g. 4.5% annualized — you can replace with current 3M T-bill yield
Rf_monthly <- Rf_annual / 12

# ---- Get price data and monthly returns ----
returns_xts <- tq_get(tickers,
                      from = start_date, to = end_date,
                      get = "stock.prices") %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "monthly",
               col_rename = "returns") %>%
  spread(symbol, returns) %>%
  as.xts()

# ---- Compute Sharpe Ratio ----
# Several variations available:
# "StdDev" = classic Sharpe (mean excess / sd)
# "VaR" or "ES" = alternative downside-based versions

# Monthly Sharpe ratio
sharpe_monthly <- SharpeRatio(returns_xts,
                              Rf = Rf_monthly,
                              FUN = "StdDev",
                              annualize = FALSE)

# Annualized Sharpe ratio
sharpe_annual <- SharpeRatio.annualized(returns_xts,
                                        Rf = Rf_monthly,
                                        scale = 12)  # monthly data → annualize by sqrt(12)

# ---- Print results ----
cat("\n--- Monthly Sharpe Ratios ---\n")
print(round(sharpe_monthly, 3))

cat("\n--- Annualized Sharpe Ratios ---\n")
print(round(sharpe_annual, 3))

# ---- Optional: combine with other performance metrics ----
charts.PerformanceSummary(returns_xts,
                          Rf = Rf_monthly,
                          main = "DFA ETFs: Returns and Risk Metrics")

# ---- Alternative: full table of performance stats ----
table.AnnualizedReturns(returns_xts, Rf = Rf_monthly)

# ---- Rolling Sharpe (Annualized) with PerformanceAnalytics ----

# Rolling window in months (common choices: 24, 36, 60)
roll_n <- 36

# 1) Quick plot helper (one-liner)
chart.RollingPerformance(
  R    = returns_xts,
  width = roll_n,
  FUN   = "SharpeRatio.annualized",
  Rf    = Rf_monthly,   # monthly risk-free, same periodicity as returns
  scale = 12,           # annualize monthly data
  main  = paste0("Rolling ", roll_n, "-Month Sharpe (Annualized)"),
  legend.loc = "topleft",
  na.pad = TRUE
)
abline(h = 1, lty = 2)  # visual reference line at Sharpe = 1

# 2) If you want the rolling values as a time series object:
roll_sharpe_xts <- rollapply(
  data       = returns_xts,
  width      = roll_n,
  by.column  = TRUE,
  align      = "right",
  FUN        = function(R) SharpeRatio.annualized(R, Rf = Rf_monthly, scale = 12),
  na.pad     = TRUE,
  fill       = NA
)

# Peek at most recent rolling Sharpe for each ETF
round(tail(roll_sharpe_xts, 1), 3)
