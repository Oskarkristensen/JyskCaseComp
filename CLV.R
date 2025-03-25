#_______________________________________________________________________________
#CASE COMPETITION: S360 X JYSK 
#_______________________________________________________________________________

library(readxl)
library(DataExplorer)
library(dplyr)
library(lubridate)

data <- read_excel("jysk_case_competition_final.xlsx")
colnames(data)

sum(is.na(data))

data$product_group_level_1 <- as.character(data$product_group_level_1)
data$product_group_level_1[data$product_group_level_1 == "Bed linen"] <- "Bed Linen"
data$product_group_level_1 <- as.factor(data$product_group_level_1)

# > sum(is.na(data))
# [1] 54766

plot_missing(data)
#Due to low percentage of missing values, rows with missing values will be removed 

data <- na.omit(data)
sum(is.na(data))

# > sum(is.na(data))
# [1] 0

str(data)

data <- data %>%
  mutate(date = dmy(date)) 

data <- data %>% filter(order_value_ex_vat_ex_freight > 0)

t(names(data))

for (i in colnames(data)[c(4, 6:9)]) {
  data[[i]] <- as.factor(data[[i]])  
}

data$customer_id <- as.character(data$customer_id)

str(data)

dim(data)


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

#Segment labels
segment_names <- c("Champions", "Loyal", "Potential", "At Risk", "Lost", "New")

#Segment thresholds
recency_lower    <- c(4, 3, 1, 1, 1, 5)
recency_upper    <- c(5, 5, 3, 3, 3, 5)
frequency_lower  <- c(4, 3, 1, 1, 1, 1)
frequency_upper  <- c(5, 5, 3, 3, 4, 1)
monetary_lower   <- c(4, 3, 2, 2, 1, 1)
monetary_upper   <- c(5, 5, 5, 4, 4, 5)

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
# Customer Segmentation - Cluster
# _________________________________________________________________

# Create dataset for clustering
rfm_cluster_data <- rfm_result$rfm %>%
  group_by(customer_id) %>%
  summarise(
    recency = min(recency_days),               # lower is better
    frequency = n_distinct(order_id),          # number of orders
    monetary = sum(amount)                     # total spend
  )

# Sample 50,000 observations for computational efficiency
set.seed(123)
rfm_cluster_data_small <- rfm_cluster_data %>%
  sample_n(50000)

# Select RFM columns
rfm_cluster <- c("recency", "frequency", "monetary")

# Check correlations
cor(rfm_cluster_data_small[, rfm_cluster])

# Identify outliers using Euclidean distance
dev <- sweep(rfm_cluster_data_small[, rfm_cluster], 2, colMeans(rfm_cluster_data_small[, rfm_cluster]))
distances <- sqrt(rowSums(dev^2))

# Add distances back to data
rfm_cluster_data_small$distance <- distances

# Show top 10 outliers
outliers <- rfm_cluster_data_small %>%
  arrange(desc(distance)) %>%
  select(customer_id, distance, recency, frequency, monetary)

head(outliers, 10) %>% 
  kable() %>%
  kable_classic_2()

# Remove top 5 outliers
outlier_ids <- head(outliers$customer_id, 5)
rfm_cluster_data_small <- rfm_cluster_data_small %>%
  filter(!customer_id %in% outlier_ids)

# Reassign numeric customer IDs for plotting
rfm_cluster_data_small$customer_id <- seq_len(nrow(rfm_cluster_data_small))

# Hierarchical Clustering
dist_mat <- dist(rfm_cluster_data_small[, rfm_cluster], method = "euclidean")

# Ward's Method
H.fit_ward <- hclust(dist_mat, method = "ward.D")
plot(H.fit_ward)

# Evaluate percentage increase in within-cluster SS
merge_heights <- H.fit_ward$height
denominator <- c(1, cumsum(merge_heights[-length(merge_heights)]))
pct_increase <- merge_heights / denominator
tail(pct_increase, 10)

# Determine clusters (e.g., 4 clusters based on visual inspection or jump in pct increase)
grp <- as.factor(cutree(H.fit_ward, k = 4))
table(grp)

# Add group to data
rfm_cluster_data_small$grp <- grp

# Visualize clusters
rect.hclust(H.fit_ward, k = 4, border = "red")

# Assess cluster characteristics
aggregate(rfm_cluster_data_small[, rfm_cluster], list(Cluster = grp), mean)
summary(aov(recency ~ grp, data = rfm_cluster_data_small))
summary(aov(frequency ~ grp, data = rfm_cluster_data_small))
summary(aov(monetary ~ grp, data = rfm_cluster_data_small))

# (Optional) Try complete linkage
H.fit_complete <- hclust(dist_mat, method = "complete")
plot(H.fit_complete)
rect.hclust(H.fit_complete, k = 4, border = "red")

# NbClust – find optimal number of clusters
library(NbClust)
set.seed(123)
nb <- NbClust(
  rfm_cluster_data_small[, rfm_cluster],
  distance = "euclidean",
  min.nc = 2, max.nc = 8,
  method = "ward.D",
  index = "all"
)
nb$Best.nc

# _________________________________________________________________
# Customer Lifetime Value - Prediction 
# _________________________________________________________________

library(BTYD)
library(BTYDplus)
library(dplyr)
library(lubridate)
library(ggpubr)
library(Metrics)

# Define training period end
cutoff_date <- as.Date("2024-09-15")

# Identify newcomers: first purchase after cutoff
first_purchase <- data %>%
  group_by(customer_id) %>%
  summarise(first_date = min(date))

newcomers <- first_purchase %>%
  filter(first_date >= cutoff_date)

# Remove newcomers from dataset
data <- data %>%
  filter(!customer_id %in% newcomers$customer_id)

# Select relevant columns and rename
data_clv <- data %>%
  select(customer_id, date, order_value_ex_vat_ex_freight) %>%
  rename(cust = customer_id, sales = order_value_ex_vat_ex_freight) %>%
  mutate(date = as.Date(date))

# Merge multiple transactions on the same date
data_clv <- BTYD::dc.MergeTransactionsOnSameDate(data_clv)

# Filter out outliers (e.g., customers with > 100 transactions)
freq <- data_clv %>%
  count(cust) %>%
  filter(n < 101)

data_clv <- data_clv %>%
  filter(cust %in% freq$cust)

# Build CBS and CBT matrix
calibration_end <- as.Date("2024-09-14")
cbs_data <- dc.ElogToCbsCbt(data_clv, per = "week", T.cal = calibration_end, statistic = "freq")
cal.cbs <- as.matrix(cbs_data$cal$cbs)

# _________________________________________________________________
# PARETO/NBD MODEL
# _________________________________________________________________

params.pareto <- BTYD::pnbd.EstimateParameters(cal.cbs = cal.cbs)
params.pareto

LL.pareto <- pnbd.cbs.LL(params.pareto, cal.cbs)
LL.pareto

# Fit plot
pnbd.PlotFrequencyInCalibration(params.pareto, cal.cbs, censor = 7)

# _________________________________________________________________
# BG/NBD MODEL
# _________________________________________________________________

params.bg <- bgnbd.EstimateParameters(cal.cbs)
LL.bg <- bgnbd.cbs.LL(params.bg, cal.cbs)

# Fit plot
bgnbd.PlotFrequencyInCalibration(params.bg, cal.cbs, censor = 7)

# _________________________________________________________________
# GAMMA-GAMMA MONETARY MODEL
# _________________________________________________________________

cal.cbs1 <- as.data.frame(cal.cbs) %>%
  tibble::rownames_to_column("cust") %>%
  mutate(cust = as.character(cust))

# Transaction data during calibration period
data_gamma_train <- data_clv %>% filter(date < cutoff_date)

df_gamma_train <- data_gamma_train %>%
  group_by(cust) %>%
  summarise(m.x = mean(sales))

cal.cbs.gamma <- merge(cal.cbs1, df_gamma_train, by = "cust")

# Estimate Gamma-Gamma parameters
ave.spend <- cal.cbs.gamma$m.x
tot.trans <- cal.cbs.gamma$x

# Filter for customers with at least 1 repeat purchase
ave.spend <- ave.spend[tot.trans > 0]
tot.trans <- tot.trans[tot.trans > 0]

params_gamma <- spend.EstimateParameters(m.x.vector = ave.spend, x.vector = tot.trans)

# Plot model fit
spend.plot.average.transaction.value(
  params = params_gamma,
  m.x.vector = ave.spend,
  x.vector = tot.trans,
  xlab = "Average Transaction Value",
  ylab = "Marginal Distribution of Average Transaction Value",
  title = "Actual vs Expected Average Transaction Value"
)

# Independence assumption check
tot.trans.trunc <- ifelse(tot.trans > 7, 7, tot.trans)
data.ass <- data.frame(tot.trans.trunc, ave.spend)

ggboxplot(data.ass, x = "tot.trans.trunc", y = "ave.spend",
          fill = "grey88", ylab = "Average purchase value", xlab = "Repeat purchases") +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 15))

cor(data.ass$ave.spend, data.ass$tot.trans.trunc)

# _________________________________________________________________
# CLV PREDICTIONS (1 YEAR)
# _________________________________________________________________

T.star <- 52  # weeks

# Predict expected transactions
cal.cbs1$yearpred.pareto <- round(pnbd.ConditionalExpectedTransactions(
  params.pareto, T.star = T.star,
  x = cal.cbs1$x, t.x = cal.cbs1$t.x, T.cal = cal.cbs1$T.cal,
  hardie = TRUE), 2)

cal.cbs1$yearpred.bg <- round(bgnbd.ConditionalExpectedTransactions(
  params.bg, T.star = T.star,
  x = cal.cbs1$x, t.x = cal.cbs1$t.x, T.cal = cal.cbs1$T.cal,
  hardie = TRUE), 2)

# Predict average spend
cal.cbs.gamma$spend <- spend.expected.value(
  params = params_gamma,
  m.x = cal.cbs.gamma$m.x,
  x = cal.cbs.gamma$x
)

# _________________________________________________________________
# REAL VALUES FROM VALIDATION
# _________________________________________________________________


data_gamma_test <- data_clv %>% filter(date >= cutoff_date)

df_gamma_test <- data_gamma_test %>%
  group_by(cust) %>%
  summarise(m.x.avg.real.sale.test = mean(sales))

Freq <- as.data.frame(table(data_gamma_test$cust))
colnames(Freq) <- c("cust", "Freq")

# Merge with predicted
cal.cbs.gamma.test <- merge(cal.cbs.gamma, df_gamma_test, by = "cust", all.x = TRUE)
cal.cbs.gamma.test <- merge(cal.cbs.gamma.test, Freq, by = "cust", all.x = TRUE)
cal.cbs.gamma.test <- merge(cal.cbs.gamma.test, cal.cbs1, by = "cust", all.x = TRUE)
cal.cbs.gamma.test[is.na(cal.cbs.gamma.test)] <- 0

# CLV calculations
cal.cbs.gamma.test$CLV.pred.pareto <- cal.cbs.gamma.test$spend * cal.cbs.gamma.test$yearpred.pareto
cal.cbs.gamma.test$CLV.pred.bg     <- cal.cbs.gamma.test$spend * cal.cbs.gamma.test$yearpred.bg
cal.cbs.gamma.test$CLV.real        <- cal.cbs.gamma.test$m.x.avg.real.sale.test * cal.cbs.gamma.test$Freq

# Evaluation metrics
rmse_pareto <- rmse(cal.cbs.gamma.test$CLV.real, cal.cbs.gamma.test$CLV.pred.pareto)
mae_pareto <- mae(cal.cbs.gamma.test$CLV.real, cal.cbs.gamma.test$CLV.pred.pareto)

rmse_bg <- rmse(cal.cbs.gamma.test$CLV.real, cal.cbs.gamma.test$CLV.pred.bg)
mae_bg <- mae(cal.cbs.gamma.test$CLV.real, cal.cbs.gamma.test$CLV.pred.bg)

models_stats <- rbind(
  c(RMSE = rmse_pareto, MAE = mae_pareto),
  c(RMSE = rmse_bg, MAE = mae_bg)
)
rownames(models_stats) <- c("Pareto/NBD", "BG/NBD")
round(models_stats, 2)

# _________________________________________________________________
# CLV Histogram
# _________________________________________________________________

hist(cal.cbs.gamma.test$CLV.real,
     main = "CLV.real vs CLV.pred.pareto",
     xlab = "CLV Value",
     col = rgb(0, 0, 1, 0.5),
     border = "black",
     breaks = 50,
     xlim = c(0, 7000),
     ylim = c(0, 100))

hist(cal.cbs.gamma.test$CLV.pred.pareto,
     col = rgb(1, 0, 0, 0.5),
     border = "black",
     breaks = 50,
     add = TRUE)

legend("topright",
       legend = c("CLV.real", "CLV.pred.pareto"),
       fill = c(rgb(0, 0, 1, 0.5), rgb(1, 0, 0, 0.5)))

