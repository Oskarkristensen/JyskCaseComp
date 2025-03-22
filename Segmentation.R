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

for (i in colnames(data)[c(4, 7:9)]) {
  data[[i]] <- as.factor(data[[i]])  
}

data$customer_id <- as.character(data$customer_id)

str(data)

dim(data)

# _________________________________________________________________
# Customer Segmentation
# _________________________________________________________________

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
# Cluster Analysis - Hierarchical
# _________________________________________________________________

#To do the cluster analysis, we are using the columns recency, frequency and monetary
#from the RFM analysis 

colnames(rfm_result)
colnames(rfm_result$rfm)

# Create dataset for clustering
rfm_cluster_data <- rfm_result$rfm %>%
  group_by(customer_id) %>%
  summarise(
    recency = min(recency_days),  # lower is better
    frequency = n_distinct(order_id),  # how many orders
    monetary = sum(amount)  # total spend
  )

str(rfm_cluster_data)

#Data preparation
#_______________________________________________________________________________
prep <- nrow(rfm_cluster_data) #stores number of of rows in dataset

#Choosing variables for the cluster analysis and summarize them
rfm_cluster <- c("recency","frequency","monetary")
summary(rfm_cluster_data[,rfm_cluster])

#Computes standard deviation for each variable
apply(rfm_cluster_data[,rfm_cluster],2,sd)

#Checking for multicollinarity
cor(rfm_cluster_data[,rfm_cluster])
#Highest multicollinaruty is between frequency and monetary 
#Is 41% which is okay

#Looking for outliers 
# Compute Euclidean distance from the mean for each customer
dev <- t(t(rfm_cluster_data[, rfm_cluster]) - apply(rfm_cluster_data[, rfm_cluster], 2, mean))
dev2 <- dev^2
sumdev2 <- rowSums(dev2)
distances <- sqrt(sumdev2)

# Add distances to the original data
rfm_cluster_data$distance <- distances

# Sort by descending distance and show top customers
outliers <- rfm_cluster_data %>%
  arrange(desc(distance)) %>%
  select(customer_id, distance, recency, frequency, monetary)

# Show top 10 potential outliers
head(outliers, 10) %>%
  kable() %>%
  kable_classic_2()

#10 outliers are deleted
rfm_cluster_data <- subset(rfm_cluster_data,customer_id!="1018127357"&customer_id!="10185455"
                           &customer_id!="101829177"&customer_id!="101879163"&customer_id!="101830099"&
                           customer_id!="101825776"&customer_id!="101818399"&customer_id!="101824856"&
                           customer_id!="1018125720"&customer_id!="101818984")
nobs <- nrow(rfm_cluster_data)
rfm_cluster_data$customer_id <- seq(1,nobs)

## Hierarchical clustering
#Create distance matrix
dmat <- dist(rfm_cluster_data[, rfm_cluster], method = "euclidean")
dist2 <- dmat^2

#Using wards method
H.fit <- hclust(dist2,method="ward.D")

#Drawing a Dendogram
plot(H.fit)

#Assess pct. increase
denominator <- cumsum(H.fit[[2]])
length(denominator) <- nobs-2
denominator <- c(1,denominator)
pct <- H.fit[[2]]/denominator
tail(pct,n=10)

#Apart from going from 2-1 the largest jump is from 4-3, we stop just before
grp <- as.factor(cutree(H.fit,k=4))
table(grp)

#Illustrate a 4 cluster solution
rect.hclust(H.fit,k=4,border="red")

#Assess outcome
aggregate(rfm_cluster_data[,rfm_cluster],list(grp),mean)
summary(aov(recency~grp,data=rfm_cluster_data))
summary(aov(frequency~grp,data=rfm_cluster_data))
summary(aov(monetary~grp,data=rfm_cluster_data))

#Complete linkage
H.fit <- hclust(dmat,method="complete")

#Drawing a dendogram
plot(H.fit)

#Assess pct. increase
denominator <- cumsum(H.fit[[2]])
length(denominator) <- nobs-2
denominator <- c(1,denominator)
pct <- H.fit[[2]]/denominator
tail(pct,n=10)

#Results from Ward's method are supported -
grp <- as.factor(cutree(H.fit,k=4))
table(grp)

#But size of the clusters differ
rect.hclust(H.fit,k=4,border="red")

#Assess outcome
aggregate(rfm_cluster_data[,rfm_cluster],list(grp),mean)
summary(aov(recency~grp,data=rfm_cluster_data))
summary(aov(frequency~grp,data=rfm_cluster_data))
summary(aov(monetary~grp,data=rfm_cluster_data))

#Using NbClust
res <- NbClust(rfm_cluster_data[,rfm_cluster], distance = "euclidean", min.nc=2, max.nc=8, 
             method = "ward.D", index = "all")
res$All.index
res$Best.nc

