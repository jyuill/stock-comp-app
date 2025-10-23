# Sharpe ratio calculations - revised
# - provided by ChatGPT as different version from previous

# ---- Packages ----
pkgs <- c("tidyverse","tidyquant","PerformanceAnalytics","xts","zoo","scales")
invisible(lapply(pkgs, function(p) if (!require(p, character.only = TRUE)) install.packages(p)))
lapply(pkgs, library, character.only = TRUE)

# ---- Parameters ----
tickers    <- c("DFIV","DFAI","DFAX","DFAE", "DEHP")   # change/add as needed
start_date <- "2020-01-01"
end_date   <- Sys.Date()

Rf_annual  <- 0.04     # 4% annual risk-free (set to your preference)
Rf_monthly <- Rf_annual / 12

# ---- Prices → monthly returns (xts, one column per ticker) ----
returns_xts <- tq_get(tickers, from = start_date, to = end_date, get = "stock.prices") %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "monthly",
               type = "log",        # use "arithmetic" if preferred
               col_rename = "ret") %>%
  ungroup() %>%
  tidyr::pivot_wider(names_from = symbol, values_from = ret) %>%
  arrange(date) %>%
  { xts::as.xts(dplyr::select(., -date), order.by = .$date) }

# ---- Sharpe ratios (annualized) & summary stats ----
# Annualized Sharpe using monthly data (scale = 12)
sharpe_ann_xts <- SharpeRatio.annualized(returns_xts, Rf = Rf_monthly, scale = 12)

# Annualized return/vol via PerformanceAnalytics
ann_tbl <- PerformanceAnalytics::table.AnnualizedReturns(returns_xts, Rf = Rf_monthly, scale = 12)
# Convert to a tidy tibble
metrics <- ann_tbl %>%
  data.frame(check.names = FALSE) %>%
  rownames_to_column(var = "Metric") %>%
  pivot_longer(-Metric, names_to = "symbol", values_to = "value") %>%
  pivot_wider(names_from = Metric, values_from = value) %>%
  # table.AnnualizedReturns names: "Annualized Return", "Annualized Std Dev", "Annualized Sharpe Ratio"
   rename(ann_return = `Annualized Return`,
          ann_vol    = `Annualized Std Dev`,
          sharpe_ann = contains("Sharpe")) %>%
  arrange(desc(sharpe_ann))

print(metrics)

# ---- Plot 1: Bar chart of annualized Sharpe ----
p_sharpe <- metrics %>%
  mutate(symbol = fct_reorder(symbol, sharpe_ann)) %>%
  ggplot(aes(symbol, sharpe_ann)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = round(sharpe_ann, 2)), vjust = -0.4, size = 3.8) +
  labs(title = "Annualized Sharpe Ratio (monthly data, scale = 12)",
       subtitle = paste0("Risk-free (annual): ", scales::percent(Rf_annual),
                         " • Period: ", format(start(returns_xts), "%Y-%m"), " → ",
                         format(end(returns_xts), "%Y-%m")),
       x = NULL, y = "Sharpe (annualized)") +
  theme_minimal(base_size = 12)
print(p_sharpe)

# ---- Plot 2: Risk–Return with each asset’s Sharpe line (facet per symbol) ----
# Build Sharpe lines: for each symbol, line is Return = Rf + Sharpe * Vol
max_vol <- max(metrics$ann_vol, na.rm = TRUE)
vol_grid <- seq(0, max(0.25, max_vol * 1.2), by = 0.002)

line_df <- metrics %>%
  select(symbol, sharpe_ann) %>%
  mutate(Rf = Rf_annual) %>%
  tidyr::crossing(vol = vol_grid) %>%
  mutate(ret = Rf + sharpe_ann * vol)

points_df <- metrics %>% select(symbol, ann_return, ann_vol, sharpe_ann)

p_rr <- ggplot() +
  geom_line(data = line_df, aes(x = vol, y = ret), size = 1) +
  geom_point(data = points_df, aes(x = ann_vol, y = ann_return), size = 2) +
  geom_text(data = points_df,
            aes(x = ann_vol, y = ann_return,
                label = paste0(symbol, "\nVol=", percent(ann_vol, 0.1),
                               ", Ret=", percent(ann_return, 0.1),
                               "\nSharpe=", round(sharpe_ann, 2))),
            hjust = -0.05, vjust = -0.6, size = 3.2) +
  geom_hline(yintercept = Rf_annual, linetype = "dashed") +
  facet_wrap(~ symbol, ncol = 2, scales = "free_x") +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = "Risk–Return Efficiency: Sharpe Lines by Asset",
       subtitle = paste0("Each line passes through the risk-free rate (", percent(Rf_annual),
                         ") with slope = asset’s annualized Sharpe"),
       x = "Volatility (annualized σ)", y = "Return (annualized)") +
  theme_minimal(base_size = 12)
print(p_rr)

# ---- Optional: Rolling Sharpe (annualized) ----
roll_n <- 36
chart.RollingPerformance(
  R    = returns_xts,
  width = roll_n,
  FUN   = "SharpeRatio.annualized",
  Rf    = Rf_monthly,
  scale = 12,
  main  = paste0("Rolling ", roll_n, "-Month Sharpe (Annualized)"),
  legend.loc = "topleft",
  na.pad = TRUE
)
abline(h = 1, lty = 2)

# ---- Optional: combine with other performance metrics ----
charts.PerformanceSummary(returns_xts,
                          Rf = Rf_monthly,
                          main = "DFA ETFs: Returns and Risk Metrics")
