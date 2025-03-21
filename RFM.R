# _________________________________________________________________
# RFM Analysis
# _________________________________________________________________

# Load necessary libraries
library(kableExtra)
library(ggplot2)
library(tidyr)
library(dplyr)
library(rfm)

# Preview data
head(data) %>% 
  kable() %>% 
  kable_minimal()

# Summary statistics
summary(data) %>% 
  kable() %>% 
  kable_minimal()

# Top 10 customers by total spend
data %>%
  group_by(customer_id) %>%
  summarise(total_spend = sum(order_value_ex_vat_ex_freight, na.rm = TRUE)) %>%
  arrange(desc(total_spend)) %>%
  head(10) %>%
  kable() %>%
  kable_classic_2()

# Top 10 products by total revenue
data %>%
  group_by(product_title) %>%
  summarise(total_revenue = sum(order_value_ex_vat_ex_freight, na.rm = TRUE)) %>%
  arrange(desc(total_revenue)) %>%
  head(10) %>%
  kable() %>%
  kable_classic_2()

# Top 10 product categories by revenue
data %>%
  group_by(product_group_level_1) %>%
  summarise(total_revenue = sum(order_value_ex_vat_ex_freight, na.rm = TRUE)) %>%
  arrange(desc(total_revenue)) %>%
  head(10) %>%
  kable() %>%
  kable_classic_2()

# Prepare data for RFM analysis
rfm_data <- data %>%
  rename(
    order_date = date,      
    revenue = order_value_ex_vat_ex_freight  
  )

# Define analysis date (set to latest dataset date)
analysis_date <- as.Date("2024-12-31")

# Compute RFM scores
rfm_result <- rfm_table_order(
  data = rfm_data, customer_id,order_date, revenue, analysis_date)
print(rfm_result, n=10)

# Visualize RFM results
rfm_plot_heatmap(rfm_result)  # Heatmap of Recency, Frequency, and Monetary scores #consider removing this as recency does not make sense in this case
rfm_bar_chart(rfm_result)  # RFM Score Distribution

# We label the various segments
segment_names <- c("Champions", "Loyal Customers", "Potential Loyalist",
                   "New Customers", "Promising", "Need Attention", "About To Sleep",
                   "At Risk", "Can't Lose Them", "Lost")

# We set the upper and lower bounds for recency, frequency, and monetary for the above segments (segments intervals)
recency_lower <- c(4, 2, 3, 4, 3, 2, 2, 1, 1, 1)
recency_upper <- c(5, 5, 5, 5, 4, 3, 3, 2, 1, 2)
frequency_lower <- c(4, 3, 1, 1, 1, 2, 1, 2, 4, 1)
frequency_upper <- c(5, 5, 3, 1, 1, 3, 2, 5, 5, 2)
monetary_lower <- c(4, 3, 1, 1, 1, 2, 1, 2, 4, 1)
monetary_upper <- c(5, 5, 3, 1, 1, 3, 2, 5, 5, 2)

# Define segmentation criteria (ensure these variables are defined)
segment <- rfm_segment(
  rfm_result,
  segment_names,
  recency_lower,
  recency_upper,
  frequency_lower, 
  frequency_upper, 
  monetary_lower,
  monetary_upper
)

# segment summary
segment_overview <- rfm_segment_summary(segment)
segment_overview

# plot segment summary
rfm_plot_segment_summary(segment_overview, flip = TRUE, sort=TRUE, axis_label_size = 12) # horizontal bars #customers is deafult
rfm_plot_segment_summary(segment_overview, metric = "orders", flip = TRUE, sort=TRUE, axis_label_size = 12) # horizontal bars
rfm_plot_segment_summary(segment_overview, metric = "revenue", flip = TRUE, sort=TRUE, axis_label_size = 12) # select metric to be visualized


# another view
head(segment) %>% 
  kable() %>% 
  kable_classic_2()


# Segment wise median recency, frequency & monetary value plot.
rfm_plot_median_recency(segment, sort=TRUE, axis_label_size = 12)
rfm_plot_median_frequency(segment, sort=TRUE, axis_label_size = 12)
rfm_plot_median_monetary(segment, sort=TRUE, axis_label_size = 12)

# _________________________________________________________________
# CLV Prediction Model
# _________________________________________________________________

# Required packages
library(BTYD)
library(BTYDplus)
library(Metrics)
library(tibble)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggpubr)

# ______________________________________________________________________________

#CHAT OUTPUT - NEED TO BE CORRECTED!!!!!!
# _________________________________________________________________
# CLV Prediction Model - Adapted for S360 x JYSK Case Competition
# _________________________________________________________________

# Required packages
library(BTYD)
library(BTYDplus)
library(Metrics)
library(tibble)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggpubr)

# Rename for modeling consistency
data_clv <- data %>%
  rename(
    cust = customer_id,
    date = date,
    sales = order_value_ex_vat_ex_freight
  ) %>%
  mutate(date = as.Date(date))

data_clv <- data_clv %>%
  select(cust, date, sales)

str(data_clv)

# Merge transactions on same date for each customer (BTYD requirement)
data_clv <- dc.MergeTransactionsOnSameDate(data_clv)

# Preview transaction data
head(data_clv)

# ----------------------------
# Calibration matrix creation
# ----------------------------

# Define end of calibration period
end.of.cal.period <- as.Date("2024-09-30")  # Set based on your context

# Convert transaction log into customer-by-sufficient-statistics (CBS) matrix
data_cbs <- dc.ElogToCbsCbt(
  elog = data_clv,
  per = "week",
  T.cal = end.of.cal.period,
  statistic = "freq"
)

cal.cbs <- as.matrix(data_cbs$cal$cbs)

# ----------------------------
# Pareto/NBD Model
# ----------------------------

# Estimate model parameters
params.pareto <- pnbd.EstimateParameters(cal.cbs)
params.pareto

# Log-likelihood for model fit
LL.pareto <- pnbd.cbs.LL(params.pareto, cal.cbs)

# Plot fit on calibration data
pnbd.PlotFrequencyInCalibration(params.pareto, cal.cbs, censor = 7)

# ----------------------------
# BG/NBD Model
# ----------------------------

# Estimate model parameters
params.bg <- bgnbd.EstimateParameters(cal.cbs)
params.bg

# Log-likelihood
LL.bg <- bgnbd.cbs.LL(params.bg, cal.cbs)

# Plot fit
bgnbd.PlotFrequencyInCalibration(params.bg, cal.cbs, censor = 7)

# ----------------------------
# Gamma-Gamma Model (monetary value)
# ----------------------------

# Convert to data frame and include customer IDs
cal.cbs1 <- as.data.frame(cal.cbs)
cal.cbs1 <- rownames_to_column(cal.cbs1, "cust")
cal.cbs1$cust <- as.character(cal.cbs1$cust)

# Get average spend per transaction during calibration
data_gamma_train <- data_clv %>% filter(date <= end.of.cal.period)

df_gamma_train <- data_gamma_train %>%
  group_by(cust) %>%
  summarise(m.x = mean(sales), .groups = "drop")

cal.cbs.gamma <- merge(cal.cbs1, df_gamma_train, by = "cust")

# Filter to customers with at least 1 repeat purchase
valid_idx <- cal.cbs.gamma$x > 0
ave.spend <- cal.cbs.gamma$m.x[valid_idx]
tot.trans <- cal.cbs.gamma$x[valid_idx]

# Estimate Gamma-Gamma model
params_gamma <- spend.EstimateParameters(m.x.vector = ave.spend, x.vector = tot.trans)
params_gamma

# Plot actual vs expected average transaction value
spend.plot.average.transaction.value(
  params = params_gamma,
  m.x.vector = ave.spend,
  x.vector = tot.trans,
  xlab = "Average Transaction Value",
  ylab = "Marginal Distribution of Avg Transaction Value",
  title = "Actual vs. Expected Average Transaction Value"
)

# ----------------------------
# Predict CLV (12-month horizon)
# ----------------------------

# Define prediction period (T.star = 52 weeks = 1 year)
T.star <- 52

# Predict # of transactions using both models
cal.cbs1$pred_pareto <- pnbd.ConditionalExpectedTransactions(
  params.pareto,
  T.star = T.star,
  x = cal.cbs1$x,
  t.x = cal.cbs1$t.x,
  T.cal = cal.cbs1$T.cal
)

cal.cbs1$pred_bg <- bgnbd.ConditionalExpectedTransactions(
  params.bg,
  T.star = T.star,
  x = cal.cbs1$x,
  t.x = cal.cbs1$t.x,
  T.cal = cal.cbs1$T.cal
)

# Add predicted spend per customer (Gamma-Gamma)
cal.cbs.gamma$spend <- spend.expected.value(
  params = params_gamma,
  m.x = cal.cbs.gamma$m.x,
  x = cal.cbs.gamma$x
)

# Merge predictions
clv_df <- merge(cal.cbs.gamma, cal.cbs1[, c("cust", "pred_pareto", "pred_bg")], by = "cust")

# Compute CLV
clv_df$CLV_pareto <- clv_df$spend * clv_df$pred_pareto
clv_df$CLV_bg <- clv_df$spend * clv_df$pred_bg

# ----------------------------
# Evaluate predictions vs actuals (optional, if you have holdout data)
# ----------------------------

# Create holdout dataset
data_gamma_test <- data_clv %>% filter(date > end.of.cal.period)

# Compute actual sales per customer in holdout
df_gamma_test <- data_gamma_test %>%
  group_by(cust) %>%
  summarise(
    real_sales = mean(sales),
    real_freq = n(),
    .groups = "drop"
  )

# Merge with CLV prediction
clv_eval <- merge(clv_df, df_gamma_test, by = "cust", all.x = TRUE)
clv_eval[is.na(clv_eval)] <- 0  # Replace NAs with 0 (customers with no future purchases)

# Calculate actual CLV
clv_eval$CLV_real <- clv_eval$real_sales * clv_eval$real_freq

# ----------------------------
# Performance evaluation
# ----------------------------

rmse_pareto <- rmse(clv_eval$CLV_real, clv_eval$CLV_pareto)
mae_pareto <- mae(clv_eval$CLV_real, clv_eval$CLV_pareto)

rmse_bg <- rmse(clv_eval$CLV_real, clv_eval$CLV_bg)
mae_bg <- mae(clv_eval$CLV_real, clv_eval$CLV_bg)

# Create summary table
model_perf <- rbind(
  c(RMSE = rmse_pareto, MAE = mae_pareto),
  c(RMSE = rmse_bg, MAE = mae_bg)
)
rownames(model_perf) <- c("Pareto/NBD", "BG/NBD")
round(model_perf, 2)

# ----------------------------
# Histogram of Real vs Predicted CLV
# ----------------------------

hist(clv_eval$CLV_real,
     main = "Histogram of CLV.real vs CLV.pred.pareto",
     xlab = "CLV Value",
     col = rgb(0, 0, 1, 0.5),
     border = "black",
     breaks = 50,
     xlim = c(0, 7000),
     ylim = c(0, 100))

hist(clv_eval$CLV_pareto,
     col = rgb(1, 0, 0, 0.5),
     border = "black",
     breaks = 50,
     add = TRUE)

legend("topright",
       legend = c("CLV.real", "CLV.pred.pareto"),
       fill = c(rgb(0, 0, 1, 0.5), rgb(1, 0, 0, 0.5)))
