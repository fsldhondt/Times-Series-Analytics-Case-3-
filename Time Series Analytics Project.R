# Case 3: Modeling Stock Prices, Returns and Volatility

# DATA EXPLORATION ----
# ---- Importing data ----
install.packages("quantmod")
library(quantmod)
# NVIDIA - data from 1999-01-22 (NVIDIA's IPO date) so we capture the 
# full history, including the 2008 financial crisis and the 2020 COVID crash.
getSymbols("NVDA", src = "yahoo", from = "1999-01-22", to = as.Date("2026-09-18"))

head(NVDA)
tail(NVDA)
str(NVDA)

# We will work mainly with the Adjusted Close price: the closing price corrected for stock splits and dividends,
# so it reflects the "true" return an investor would have earned.
# (NVIDIA has had several stock splits, e.g. 4-for-1 in 2021 and
# 10-for-1 in 2024 - without adjusting, the price series would show
# fake "crashes" on those split dates that have nothing to do with
# actual value changes)
price <- Ad(NVDA)          
colnames(price) <- "Adjusted_Close"

# ---- Plotting ----

# chartSeries gives a financial plot with volume
chartSeries(price, theme = "white", name = "NVIDIA - Adjusted Close Price")

# a simple base R plot of the same thing
plot(price, main = "NVIDIA Adjusted Close Price", col = "steelblue",
     ylab = "Price (USD)", xlab = "Date")

# NVIDIA: mostly flat/low for years, then a massive upward trend from ~2016 onward, and an
# especially steep rise from 2023 onward (possibly due to the AI boom)
# => a classic visual sign of NON-STATIONARITY

# ---- Crisis periods ----

# Dot-com crash
dotcom_window <- price["1999-01-22/2004-01-01"]
plot(dotcom_window, main = "NVIDIA - Dot-com Crash",
     col = "darkorange", ylab = "Price (USD)")
# NVIDIA's own price peak does not match the
# broader NASDAQ crash (which peaked in March 2000). Instead,
# NVIDIA keeps rising to an even higher peak around Jan/Feb 2002,
# about two years later, before dropping hard (~85%) into 2003.
# A possible reason is that NVIDIA was still growing and selling real
# products, so investors kept buying it even while purely
# speculative dot-com companies were already crashing in 2000-2001.
# NVIDIA's own crash came later, more linked to the 2001-2002
# slowdown in the tech/chip industry than to the initial dot-com crash itself.


# Global Financial Crisis
gfc_window <- price["2006-09-01/2010-12-31"]
plot(gfc_window, main = "NVIDIA - Global Financial Crisis",
     col = "red", ylab = "Price (USD)")
# NVIDIA show a clear drop starting middle 2008,
# bottoming out in early 2009, then recovering slopwer than it dropped


# COVID-19 crash 
covid_window <- price["2019-01-01/2022-03-31"]
plot(covid_window, main = "NVIDIA - COVID-19 Crash",
     col = "darkred", ylab = "Price (USD)")
# The plot shows a sharp drop, followed quickly by a strong 
# recovery and then a much bigger run-up afterward 


# ---- Taking log prices ----
# Because NVIDIA's price grew by orders of magnitude over the sample,
# a normal-scale plot makes early years look like a flat line near zero.
# zero. Plotting on a log scale lets us compare percentage moves
# across different price levels/time periods more fairly.

plot(log(price), main = "NVIDIA Log (Adjusted Close Price)",
     col = "darkgreen", ylab = "log(Price)")

# ---- Returns ----

# Briefly compute log returns
# Log return = log(P_t) - log(P_t-1), an approximation of the
# percentage change in price from one day to the next.
returns <- diff(log(price))
returns <- na.omit(returns)   # removes the first NA (no return on day 1)
colnames(returns) <- "log_return"

plot(returns, main = "NVIDIA Daily Log Returns", col = "lightgreen",
     ylab = "Log Return")

# Returns look like they fluctuate around zero, without any obvious trend.
# => visual signature of a (weakly) stationary series.
# Volatility clustering around 2001-2002, 2008, 2020

# ---- Rolling mean and volatility ----
# To make stationarity and "volatility clustering" more than just
# a visual impression, we compute rolling (moving-window) statistics.

# A rolling window of 21 trading days is roughly 1 month.

window <- 21

# Rolling mean of the price: if this rolling mean drifts substantially
# over time (rather than hovering around one fixed level), this
# supports the earlier observation that price is non-stationary.
roll_mean_price <- rollapply(price, width = window, FUN = mean, align = "right")

## Rolling standard deviation of returns: intuitive for "how volatile was the stock in the last month". 
# Peaks in this series pinpoint high-volatility episodes precisely.
roll_sd_returns <- rollapply(returns, width = window, FUN = sd, align = "right")

par(mfrow = c(2, 1)) 
plot(roll_mean_price, main = paste0(window, "-Day Rolling Mean of Price"),
     col = "steelblue", ylab = "Price (USD)")
plot(roll_sd_returns, main = paste0(window, "-Day Rolling Std. Dev. of Returns"),
     col = "firebrick", ylab = "Volatility")


# Rolling mean of price trending upward over long stretches =>
# further visual evidence of non-stationarity in levels.

# Rolling volatility is not flat: it spikes up during
# crises and calms down in between. This behavior, 
# volatility itself moving in persistent waves, is what motivates GARCH models.


# ---- Summary Statistics ----
install.packages("moments")
library(moments)
install.packages("knitr")
library(knitr)

summary_table <- function(x, name) {
  x <- as.numeric(x)
  data.frame(
    Series   = name,
    Min      = min(x),
    Max      = max(x),
    Mean     = mean(x),
    Median   = median(x),
    SD       = sd(x),
    Skewness = skewness(x),
    Kurtosis = kurtosis(x)
  )
}

price_summary   <- summary_table(price,   "NVDA Adjusted Close Price")
returns_summary <- summary_table(returns, "NVDA Daily Log Returns")

knitr::kable(rbind(price_summary, returns_summary), digits = 4)




# UNIT ROOT TEST (PRICE SERIES) ----
install.packages("bootUR")
library(bootUR)

set.seed(123)

# Pantula Principle: d_max = 2, standard for asset prices:
# Step 1: d = 2 -> test Delta^2(Price) for a unit root
# Step 2: d = 1 -> test Delta(Price) for a unit root (if Step 1 rejects)
# Step 3: d = 0 -> test Price (level) for a unit root (if Step 2 rejects)
# Stop at the first fail to reject.

d2price <- diff(price, differences = 2)
adf_d2price <- boot_adf(d2price, deterministics = "none")
print(adf_d2price)

# Step 2: d = 1 (if Step 1 rejects)
dprice <- na.omit(diff(price))
adf_dprice <- boot_adf(dprice, deterministics = "intercept")
print(adf_dprice)

# Step 3: d = 0 (if Step 2 rejects)
adf_price <- boot_adf(price, deterministics = "trend")
print(adf_price)

# => order of integration is I(1)

# ---- Order of Integration Function (double check the order)----
auto_order <- order_integration(data = price, data_name = "NVDA Price",
                                max_order = 2, method = "boot_adf",
                                bootstrap = "AWB", level = 0.05)

print(auto_order$order_int)

# using the automatic function  => order of integration is I(1)



# ARIMA MODELS FOR PRICE SERIES ----

# Pantula Principle showed that price is I(1), so d = 1.

plot(dprice, main = "First Difference of NVIDIA Adjusted Close Price",
     col = "darkred", ylab = "Change in Price")

# After first differencing, the strong upward trend in the price level is removed
# and the series fluctuates around zero, which is consistent with our finding that
# the price is I(1). However, the size of the fluctuations increases substantially
# in later years, indicating time-varying volatility.



# ---- (i) Manual ARIMA selection ----

# The first-differenced price is stationary, so we use its ACF and PACF
# to identify possible AR and MA components for the ARIMA model.

acf(dprice,
    main = "ACF of First-Differenced NVIDIA Price",
    lag.max = 30)

pacf(dprice,
     main = "PACF of First-Differenced NVIDIA Price",
     lag.max = 30)

library(forecast)

# Pantula Principle showed that price is I(1), so d = 1.
# The ACF and PACF of the first-differenced price are used to
# identify possible AR and MA components. We therefore compare
# several simple low-order ARIMA models.

arima_010 <- Arima(price, order = c(0, 1, 0))
arima_110 <- Arima(price, order = c(1, 1, 0))
arima_011 <- Arima(price, order = c(0, 1, 1))
arima_111 <- Arima(price, order = c(1, 1, 1))

summary(arima_010)
summary(arima_110)
summary(arima_011)
summary(arima_111)


# ---- (ii) Automatic ARIMA selection ----

# based on Pantula Principle, we set d = 1, auto.arima() then 
# searches across combinations of AR (p) and MA (q) orders.
# This provides a data-driven comparison with our manual approach.

auto_price <- auto.arima(
  price,
  d = 1,
  seasonal = FALSE,
  stepwise = FALSE,
  approximation = FALSE,
  trace = TRUE
)

summary(auto_price)
graphics.off()
checkresiduals(auto_price)


















# FROM PRICES TO RETURNS ----

# We use log returns rather than price changes.
# A log return measures the proportional change in the stock price:
# r_t = log(P_t) - log(P_t-1)
#
# Log returns are useful because they make price changes comparable
# across different price levels and are additive over time.

returns <- diff(log(price))
returns <- na.omit(returns)
colnames(returns) <- "log_return"

plot(returns,
     main = "NVIDIA Daily Log Returns",
     col = "darkgreen",
     ylab = "Log Return")

# The returns fluctuate around a relatively stable mean close to zero,
# without the strong upward trend observed in the price series.
# However, periods of large and small movements appear to cluster,
# suggesting time-varying volatility.













# UNIT ROOT TEST (RETURN SERIES) ----
library(bootUR)
set.seed(123)

# Test whether the return series contains a unit root.
# Because returns fluctuate around a non-zero mean, we include an intercept.
adf_returns <- boot_adf(returns, deterministics = "intercept")
print(adf_returns)

# H0: returns contain a unit root (non-stationary)
# H1: returns are stationary
#
# If the p-value is below 0.05, we reject the unit-root null.
# We then conclude that returns are stationary, I(0).
#
# This differs from the price series, which was I(1):
# price needed one difference to become stationary, whereas returns
# are already stationary and therefore do not need differencing.














# ---- ARIMA MODEL FOR RETURN SERIES ----

# Because returns are stationary, d = 0.
# We now use the ACF and PACF to investigate whether past returns
# or past shocks help explain current returns.

acf(returns,
    main = "ACF of NVIDIA Log Returns",
    lag.max = 21)

pacf(returns,
     main = "PACF of NVIDIA Log Returns",
     lag.max = 21)




# ---- (i) Manual ARIMA selection ----

library(forecast)

# Since returns are stationary, all candidate models have d = 0.
# We compare simple low-order ARIMA models based on the correlogram.

arima_ret_000 <- Arima(returns, order = c(0, 0, 0))
arima_ret_100 <- Arima(returns, order = c(1, 0, 0))
arima_ret_001 <- Arima(returns, order = c(0, 0, 1))
arima_ret_101 <- Arima(returns, order = c(1, 0, 1))

summary(arima_ret_000)
summary(arima_ret_100)
summary(arima_ret_001)
summary(arima_ret_101)


# ---- (ii) Automatic ARIMA selection ----

# The unit-root test showed that returns are stationary, so we set d = 0.
# auto.arima() then searches across combinations of AR (p) and MA (q)
# orders and compares the candidate models using AICc.

auto_returns <- auto.arima(
  returns,
  d = 0,
  seasonal = FALSE,
  stepwise = FALSE,
  approximation = FALSE,
  trace = TRUE
)

summary(auto_returns)

# Check whether autocorrelation remains in the residuals
checkresiduals(auto_returns)



# GARCH MODELS ----
install.packages("rugarch")
library(rugarch)
# ---- GARCH(1,1) normal ----

spec_norm <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model     = list(armaOrder = c(0, 0), include.mean = TRUE),
  distribution.model = "norm"
)

fit_norm <- ugarchfit(spec = spec_norm, data = returns)
fit_norm
# alpha1 + beta1 = 0.067 + 0.927 = 0.994 -> very high persistence,
# both coefficients significant to 5% critical level (beta1 to 1% critical value)

# ---- GARCH(1,1) Student-t (fat tails, kurtosis = 15.24) ----

spec_t <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model     = list(armaOrder = c(0, 0), include.mean = TRUE),
  distribution.model = "std"
)

fit_t <- ugarchfit(spec = spec_t, data = returns)
fit_t
# shape (df) = 4.68, highly significant (1% cv) -> confirms fat tails;
# alpha1 + beta1 = 0.053 + 0.946 = 0.998, even higher persistence than normal

# ---- Compare normal vs. t ----

infocriteria(fit_norm)
infocriteria(fit_t)
# AIC/BIC/Shibata/HQ all lower for t (-4.19 vs -4.02) -> t distribution fits
# clearly better, consistent with the high kurtosis observed earlier

# ---- Conditional volatility plot ----

cond_sd <- sigma(fit_t)
colnames(cond_sd) <- "cond_sd"

plot(cond_sd, main = "GARCH(1,1)-t: Estimated Conditional Volatility",
     col = "firebrick", ylab = "Conditional SD")
# spikes align with the crisis windows identified earlier
# (dot-com 2001-02, GFC 2008-09, COVID/AI run-up 2018-2020)

# ---- Residual diagnostics ----

std_resid <- residuals(fit_t, standardize = TRUE)

par(mfrow = c(2, 1))
acf(std_resid^2, main = "ACF of Squared Standardized Residuals", lag.max = 30)
pacf(std_resid^2, main = "PACF of Squared Standardized Residuals", lag.max = 30)
par(mfrow = c(1, 1))
# no significant bars beyond lag 0 -> no remaining ARCH effects,
# confirmed by Weighted Ljung-Box test on squared residuals (all p > 0.05)




# HAR MODELS ----

library(zoo)

# ---- Realized Range (Parkinson estimator, from High/Low) ----

high <- Hi(NVDA)
low  <- Lo(NVDA)

realized_range <- (1 / (4 * log(2))) * (log(high / low))^2
colnames(realized_range) <- "realized_range"

# ---- HAR model for Realized Range ----

rr <- realized_range
rr_d <- lag(rr, 1)
rr_w <- rollapply(lag(rr, 1), width = 5, FUN = mean, align = "right", fill = NA)
rr_m <- rollapply(lag(rr, 1), width = 22, FUN = mean, align = "right", fill = NA)

har_data <- na.omit(merge(rr, rr_d, rr_w, rr_m))
colnames(har_data) <- c("rr", "rr_d", "rr_w", "rr_m")

har_model <- lm(rr ~ rr_d + rr_w + rr_m, data = as.data.frame(har_data))
summary(har_model)
# all three components highly significant; daily (0.38) and monthly (0.43)
# lags matter most, weekly least (0.08) - typical HAR pattern of long-memory
# volatility; R2 = 0.41, decent given we only use daily range data as proxy


# Comparison between HAR (realized range) vs GARCH(1,1)-t
# HAR fitted values come back without dates, so we attach the dates of har_data
har_fit <- xts(fitted(har_model), order.by = index(har_data))

# Put both on the same scale: daily standard deviation
# (HAR models variance, so we take the square root; pmax guards against any slightly negative fitted values)
har_vol   <- sqrt(pmax(har_fit, 0))
garch_vol <- sigma(fit_t)

compare_vol <- na.omit(merge(har_vol, garch_vol))
colnames(compare_vol) <- c("HAR_RealizedRange", "GARCH_t")

plot(compare_vol,
     col = adjustcolor(c("steelblue", "indianred"), alpha.f = 0.65),
     lwd = 1,
     main = "Daily Volatility: HAR (Realized Range) vs GARCH(1,1)-t",
     legend.loc = "topright")

# How closely do the two volatility estimates move together?
cor(compare_vol)

# Persistence comparison: sum of HAR slope coefficients vs alpha1 + beta1
sum(coef(har_model)[-1])
sum(coef(fit_t)[c("alpha1", "beta1")])



# ---- Monthly Realized Variance from daily returns ----

returns_sq <- returns^2
monthly_rv <- apply.monthly(returns_sq, FUN = sum)
colnames(monthly_rv) <- "monthly_rv"

# Drop the first (Jan 1999) and last (Sep 2026) months: both are incomplete
# (data start on Jan 22 and end on Sep 18), so summing fewer trading days
# would understate their realized variance
monthly_rv <- monthly_rv[-c(1, nrow(monthly_rv))]

plot(monthly_rv, main = "Monthly Realized Variance (NVDA)", col = "darkblue")
# Overall pattern shows the same long stretches of calm (2012-2016) and 
# turbulence found earlier. Clear spikes align with dot-com crash (2001), 
# GFC (2008-09), plus occasional bursts in 2016-2020 and 2024-25. 

# ---- HAR model for Monthly Realized Variance ----

mrv <- monthly_rv
mrv_1 <- lag(mrv, 1)
mrv_3 <- rollapply(lag(mrv, 1), width = 3, FUN = mean, align = "right", fill = NA)
mrv_12 <- rollapply(lag(mrv, 1), width = 12, FUN = mean, align = "right", fill = NA)

har_monthly_data <- na.omit(merge(mrv, mrv_1, mrv_3, mrv_12))
colnames(har_monthly_data) <- c("mrv", "mrv_1", "mrv_3", "mrv_12")

har_monthly_model <- lm(mrv ~ mrv_1 + mrv_3 + mrv_12, data = as.data.frame(har_monthly_data))
summary(har_monthly_model)

# mrv_1 (0.29) and mrv_12 (0.38) significant (at 1% level), mrv_3 not (p=0.34) - unlike the
# daily HAR model, the medium-term (3-month) component adds little once
# last month and the yearly average are already included; R2 = 0.30, lower
# than the daily HAR (0.41), as expected with far fewer observations (n=321
# vs. n=6935) and fewer, more aggregated data points.






# MULTIVARIATE EXTENSION: VOLATILITY SPILLOVERS ANALYSIS ----

# Estimate a VAR on the daily volatility of 5 series and measure how much of each
# series' volatility is caused by shocks to the others.

# ---- Importing additional data ----
# (for ^GSPC the object is called GSPC, without the ^)
getSymbols(c("AMD", "MSFT", "^GSPC", "NEM"), src = "yahoo",
           from = "1999-01-22", to = as.Date("2026-09-18"))

# ---- Realized range and log volatility ----
# Compute the realized range and take logs
# The same Parkinson estimator as in the HAR section:
# RR = (1 / (4 * log(2))) * (log(High / Low))^2

realized_range_function <- function(x) {
  (1 / (4 * log(2))) * (log(Hi(x) / Lo(x)))^2
}

rr_all <- merge(realized_range_function(NVDA),
                realized_range_function(AMD),
                realized_range_function(MSFT),
                realized_range_function(GSPC),
                realized_range_function(NEM))
colnames(rr_all) <- c("NVDA", "AMD", "MSFT", "GSPC", "NEM")

# Keep only days on which all of the 5 series have data
rr_all <- na.omit(rr_all)

# If High + Low on a day, the range is 0 and log(0) = - infinity => removing those days
colSums(rr_all == 0)     # how many zero days per series
rr_all[rr_all == 0] <- NA
rr_all <- na.omit(rr_all)

volatility <- log(rr_all)

# Checking to see why we take the logs: raw realized range is very skewed with fat tails,
# the log version should be close to normal (skewness ~ 0, kurtosis ~3)
skewness(as.numeric(rr_all$NVDA))
kurtosis(as.numeric(rr_all$NVDA))
skewness(as.numeric(volatility$NVDA))
kurtosis(as.numeric(volatility$NVDA))

par(mfrow = c(1, 2))
hist(rr_all$NVDA, breaks = 100, main = "NVDA Realized Range", xlab = "Realized range", ylab = "Frequency")
hist(volatility$NVDA, breaks = 100, main = "NVDA Log Realized Range", xlab = "Log realized range", ylab = "Frequency")
par(mfrow = c(1,1))

# ---- Exploring the volatility series ----
plot(volatility, multi.panel = TRUE, yaxis.same = FALSE,
     main = " Log Realized Range: NVDA, AMD, MSFT, GSPC, NEM")

# Correlations: which series move together
round(cor(volatility), 2)

# Correlation heatmap of the five volatility series
install.packages("ggplot2")
install.packages("reshape2")
library(ggplot2)
library(reshape2)

# Correlation matrix of the log volatility series
cor_matrix <- cor(volatility)

# Convert the matrix to long format (one row per pair) for ggplot
cor_data <- melt(cor_matrix)

# Heatmap
ggplot(cor_data, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white", linewidth = 1) +                     # one square per pair
  geom_text(aes(label = sprintf("%.2f", value)), size = 5) +      # value inside each square
  scale_fill_gradient(low = "#E8F1FA", high = "#205493",          # light = low, dark = high
                      limits = c(0, 1), name = "Correlation") +
  coord_equal() +                                                 # square tiles
  labs(title = "Correlation of Log Realized Range (Volatility)",
       subtitle = "NVIDIA, AMD, Microsoft, S&P 500 and Newmont",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 13) +
  theme(panel.grid = element_blank(),
        axis.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold", size = 16))

# ---- Unit root tests ----
# A VAR requires stationary series, so we test each of the series
# H0: unit root (non-stationarity) vs. H1: stationary
# Intercept only: volatility moves around a mean, without a trend

set.seed(123)
boot_adf(volatility$NVDA, deterministics = "intercept")
# p-value = 0.0005003 => stationary
boot_adf(volatility$AMD, deterministics = "intercept")
# p-value = 0 => stationary
boot_adf(volatility$MSFT, deterministics = "intercept")
# p-value = 0 => stationary
boot_adf(volatility$GSPC, deterministics = "intercept")
# p-value = 0 => stationary
boot_adf(volatility$NEM, deterministics = "intercept")
# p-value = 0 => stationary

# ---- Lag selection and VAR estimation ----
library(vars)
# Each series is regressed on its own lags and the lags of the other 4 series.
# VARselect shows which lag length each information criterion prefers
lag_select <- VARselect(volatility, lag.max = 22, type = "const")
lag_select$selection

# 5 lags, as selected by BIC; 
# Diebold & Yilmaz (2012) use 4,
# the robustness check shows results are not sensitive to this choice
p <- 5

var_model <- VAR(volatility, p = p, type = "const")

# Example: the NVIDIA equation (NVIDIA's volatility explained by all the 5 series' lags)
summary(var_model)$varresult$NVDA

# ---- Spillover table ----
# Diebold - Yilmaz spillover table
install.packages("frequencyConnectedness")
library(frequencyConnectedness)

# n.ahead = 10     => 10-day forecast horizon (as in Diebold & Yilmaz)
# no.corr = FALSE  => generalized decomposition: the results do not depend on the order of the variables
spill <- spilloverDY12(var_model, n.ahead = 10, no.corr = FALSE)

cat("\n Spillover Table")
cat("Entry (i, j) = % of i's 10-day forecast error variance caused by shocks to j\n\n")
print(spill)

cat("\n Total Spillover Index")
cat("% of all forecast error variance coming from cross-series shocks:\n")
print(round(unlist(overall(spill)), 2))

cat("\n Directional Spillovers: to others")
cat("How much each series transmits to the others (%):\n")
print(round(unlist(to(spill)), 2))

cat("\n Directional Spillovers: from others")
cat("How much each series receives from the others (%):\n")
print(round(unlist(from(spill)), 2))

cat("\n Net Spillovers (to - from)")
cat("Positive => net transmitter, negative => net receiver (%):\n")
print(round(unlist(net(spill)), 2))

cat("\n Net Pairwise Spillovers")
cat("Positive = second series transmits more to the first than it receives (%):\n")
print(round(unlist(pairwise(spill)), 2))

# ---- Robustness: lag length (full sample) ----
# Robustness: does the total spillover index change with the lag length?
# (Diebold & Yilmaz do the same for lags 2-6, Fig. A.1)
for (k in c(2, 4, 5, 6, 10, 21)) {
  var_k   <- VAR(volatility, p = k, type = "const")
  spill_k <- spilloverDY12(var_k, n.ahead = 10, no.corr = FALSE)
  cat("Lags:", k, "-> total spillover index:", overall(spill_k)[[1]], "\n")
}


# ---- Rolling spillover index ----
# Re-estimate the VAR on a moving window of 200 days (as in Diebold & Yilmaz)
# to see how spillovers change over time. 
rolling_spill <- spilloverRollingDY12(volatility, n.ahead = 10, no.corr = FALSE, 
                                      "VAR", params_est = list(p = p, type = "const"),
                                      window = 200)

# Total spillover index over time, with the crisis windows of the subsample analysis
roll_total <- overall(rolling_spill)[[1]]      # rolling total index (one value per day)
roll_df <- data.frame(date  = index(roll_total),
                      total = as.numeric(roll_total))

# Same crisis windows as in the crisis vs. calm subsample analysis
crisis_df <- data.frame(
  label = c("GFC", "COVID"),
  start = as.Date(c("2008-01-01", "2020-01-01")),
  end   = as.Date(c("2009-06-30", "2020-12-31"))
)

ggplot() +
  geom_rect(data = crisis_df,                                   # shaded crisis periods
            aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            fill = "#E08A2E", alpha = 0.2) +
  geom_line(data = roll_df, aes(x = date, y = total),           # rolling index
            color = "#24135F", linewidth = 0.5) +
  geom_text(data = crisis_df,                                   # crisis labels at the top
            aes(x = start + (end - start) / 2, y = Inf, label = label),
            vjust = 1.5, size = 5, fontface = "bold", color = "#B4621A") +
  labs(title = "Total Spillover Index (200-day rolling window, VAR(5))",
       subtitle = "Shaded: crisis windows used in the subsample analysis",
       x = NULL, y = "Total spillover index (%)") +
  theme_minimal(base_size = 13)

# ---- Robustness: lag length (rolling index) ----
# Robustness of the rolling index to the lag length
# Lags 2-6 as in Diebold & Yilmaz (Fig. A.1), plus 10 (HQ) and 21 (AIC)
lags_to_check <- c(2, 3, 4, 5, 6, 10, 21)

roll_list <- list()
for (k in lags_to_check) {
  roll_k <- spilloverRollingDY12(volatility, n.ahead = 10, no.corr = FALSE,
                                 "VAR", params_est = list(p = k, type = "const"),
                                 window = 200)
  roll_list[[paste0("lag", k)]] <- overall(roll_k)[[1]]   # total index over time
}

# Put all series side by side (one column per lag order)
roll_all <- do.call(merge, roll_list)
colnames(roll_all) <- names(roll_list)

# Band: min, median and max across lags 2-6 only (as in the paper)
roll_2to6 <- roll_all[, c("lag2", "lag3", "lag4", "lag5", "lag6")]

robust_df <- data.frame(date   = index(roll_all),
                        min    = apply(roll_2to6, 1, min),
                        median = apply(roll_2to6, 1, median),
                        max    = apply(roll_2to6, 1, max),
                        lag5   = as.numeric(roll_all$lag5),
                        lag10  = as.numeric(roll_all$lag10),
                        lag21  = as.numeric(roll_all$lag21))

# Plot: grey band = lags 2-6, dashed balck = median, blue = lag 5 (BIC), red = lag 10 (HQ), orange = lag 21 (AIC)
ggplot(robust_df, aes(x = date)) +
  geom_ribbon(aes(ymin = min, ymax = max), fill = "grey75") +
  geom_line(aes(y = lag10,  color = "Lag 10 (HQ)"),  linewidth = 0.4, alpha = 0.6) +
  geom_line(aes(y = lag21,  color = "Lag 21 (AIC)"), linewidth = 0.4, alpha = 0.6) +
  geom_line(aes(y = lag5,   color = "Lag 5 (BIC)"),  linewidth = 0.6) +
  geom_line(aes(y = median, color = "Median (lags 2-6)"), linewidth = 0.4, linetype = "dashed") +
  scale_color_manual(values = c("Lag 5 (BIC)" = "#205493",
                                "Median (lags 2-6)" = "black",
                                "Lag 10 (HQ)"       = "indianred",
                                "Lag 21 (AIC)"      = "darkorange"),
                     breaks = c("Lag 5 (BIC)", "Median (lags 2-6)", "Lag 10 (HQ)", "Lag 21(AIC)"),
                     name = NULL) +
  labs(title = "Total Spillover Index: Robustness to VAR Lag Order",
       subtitle = "Grey band = min-max across lags 2-6",
       x = NULL, y = "Total spillover index (%)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")










# CRISIS VS. CALM SUBSAMPLE ANALYSIS ----

# Matched-window comparison: crisis periods vs. calm periods
# of comparable length, to isolate the effect of market stress from
# sample-size effects on the VAR estimates (GFC and COVID only - dot-com
# excluded due to insufficient pre-crisis data and NVIDIA's own crash
# being delayed relative to the broader market crash).

gfc_crisis   <- volatility["2008-01-01/2009-06-30"]   # ~18 months
gfc_calm     <- volatility["2006-01-01/2007-06-30"]   # ~18 months

covid_crisis <- volatility["2020-01-01/2020-12-31"]   # ~12 months
covid_calm   <- volatility["2018-01-01/2018-12-31"]   # ~12 months

# GFC: 377 vs. 375 obs, COVID: 253 vs. 251 obs - matched window lengths


# ---- Helper function: fit VAR + spillover on a given window ----

run_spillover <- function(window_data, lags = p, label = "") {
  var_fit <- VAR(window_data, p = lags, type = "const")
  spill   <- spilloverDY12(var_fit, n.ahead = 10, no.corr = FALSE)
  cat("\n==========", label, "==========\n")
  cat("Total Spillover Index:", round(unlist(overall(spill)), 2), "\n")
  print(round(unlist(net(spill)), 2))
  return(spill)
}

spill_gfc_crisis   <- run_spillover(gfc_crisis,   label = "GFC Crisis (Jan 2008 - Jun 2009)")
spill_gfc_calm     <- run_spillover(gfc_calm,     label = "GFC Calm Benchmark (Jan 2006 - Jun 2007)")
spill_covid_crisis <- run_spillover(covid_crisis, label = "COVID Crisis (Jan 2020 - Dec 2020)")
spill_covid_calm   <- run_spillover(covid_calm,   label = "COVID Calm Benchmark (Jan 2018 - Dec 2018)")

# Total Spillover: 
# GFC 47.13 (crisis) vs. 23.22 (calm) - doubles
# COVID: 59.68 (crisis) vs. 45.94 (calm) - smaller relative jump, because
# the 2018 calm baseline is itself already much higher than 2006/07,
# consistent with the long-run upward trend in the rolling spillover index


# ---- Summary comparison table ----

comparison_table <- data.frame(
  Period = c("GFC Crisis", "GFC Calm", "COVID Crisis", "COVID Calm"),
  Total_Spillover = c(
    round(unlist(overall(spill_gfc_crisis)), 2),
    round(unlist(overall(spill_gfc_calm)), 2),
    round(unlist(overall(spill_covid_crisis)), 2),
    round(unlist(overall(spill_covid_calm)), 2)
  )
)
print(comparison_table)


# ---- Net spillovers comparison: who transmits vs. receives, crisis vs. calm ----

net_comparison <- data.frame(
  Series       = names(unlist(net(spill_gfc_crisis))),
  GFC_Crisis   = round(unlist(net(spill_gfc_crisis)), 2),
  GFC_Calm     = round(unlist(net(spill_gfc_calm)), 2),
  COVID_Crisis = round(unlist(net(spill_covid_crisis)), 2),
  COVID_Calm   = round(unlist(net(spill_covid_calm)), 2)
)
print(net_comparison)

# GSPC is by far the dominant net transmitter in the GFC (+7.73), consistent with
# a crisis that started in the financial sector; in COVID it is only a weak
# transmitter (+0.69), while MSFT (+1.46) and NEM (+1.13) transmit more.
# NVDA is a net receiver in both crises (-3.98 GFC, -0.95 COVID) but a slight
# net transmitter in the GFC calm period (+0.44) - driven by its own dynamics in calm periods, swept into systemic risk during crises.
# MSFT and AMD flip roles between the two crises (MSFT: -4.13 -> +1.46;
# AMD: +2.35 -> -2.32), reflecting their different market positions in
# 2008 vs. 2020.

# ---- Rolling spillover plot with the matched windows shaded ----
# Total spillover index fluctuates roughly between 15 and 60 over the full
# sample, with visible elevated stretches around 2008-09 and 2020 relative
# to the surrounding periods - consistent with the matched-window results above

roll_total <- overall(rolling_spill)[[1]]          # rolling total index from the spillover section
roll_df <- data.frame(date  = index(roll_total),
                      total = as.numeric(roll_total))

windows_df <- data.frame(
  label = c("GFC crisis", "GFC calm", "COVID crisis", "COVID calm"),
  type  = c("Crisis", "Calm", "Crisis", "Calm"),
  start = as.Date(c("2008-01-01", "2006-01-01", "2020-01-01", "2018-01-01")),
  end   = as.Date(c("2009-06-30", "2007-06-30", "2020-12-31", "2018-12-31"))
)

ggplot() +
  geom_rect(data = windows_df,                                  # shaded windows
            aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = type),
            alpha = 0.25) +
  geom_line(data = roll_df, aes(x = date, y = total),           # rolling index
            color = "#24135F", linewidth = 0.5) +
  scale_fill_manual(values = c("Crisis" = "#E08A2E", "Calm" = "#2F5DB8"),
                    name = NULL) +
  labs(title = "Total Spillover Index with Crisis and Calm Windows",
       subtitle = "200-day rolling window, VAR(5)",
       x = NULL, y = "Total spillover index (%)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

# The index is clearly elevated in both crisis windows relative to their calm
# benchmarks. The 2018 calm window already sits at a higher level than 2006-07,
# which explains the smaller relative jump for COVID.

