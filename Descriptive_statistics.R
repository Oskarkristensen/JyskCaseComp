# Descriptive statitics

# Required libraries 
library(ggplot2)
library(DataExplorer)
library(GGally)
library(BTYD)
library(plyr)
library(ggplot2)
library(lubridate)
library(reshape2)
library(BTYDplus)
library(ggpubr)
library(Hmisc)
library(tibble)
library(Metrics)
library(magrittr)
library(dplyr)
library(car)
library(rfm)
library(kableExtra)
library(reshape2)
library(tidyverse)
library(knitr)
library(flexclust)
library(clue)
library(openxlsx)
library(readxl)  
library(openxlsx)
library(scales)

# Load and check data
data <- read_excel("jysk_case_competition_final.xlsx")
View(data)
variable.names(data) # data is devided in customer information, order information and product information. 
colSums(is.na(data))
# date: 0                      
# order_id: 0                   
# customer_id: 19111
# product_id: 365
# order_value_ex_vat_ex_freight: 0
# product_title: 365             
# customer_zip_code: 19111 
# product_group_level_1 
# product_category_level_2: 7907

# check data types and convert 
str(data)
#data$date <- as.date (Date-format)

# Delete NA 
data <- na.omit(data)

# Descriptive statistics --------------------------------------------------

# Number of products sold per month:
data$date_month <- substr(data$date, 4, 5)  # Create a Month column 
# plot
ggplot(data) + 
  geom_bar(aes(x = date_month), colour = "steelblue", fill = "steelblue") + 
  ggtitle("Products sold per month") + 
  xlab("Month") +
  ylab("Number of products") +
  theme(panel.background =  element_rect( fill = "grey 94")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text=element_text(size=14, face = "plain"),
        axis.title=element_text(size=14, face = "plain"),
        title = element_text(size=14, face = "plain"))  
# Concl: There was a notable rise in product sales between September and November, with November reaching 
# the highest point.


# Revenue per month 
monthly_revenue <- aggregate(  # Agregate sales data for every month 
  order_value_ex_vat_ex_freight ~ date_month,
  data = data,
  FUN = sum,
  na.rm = TRUE
)
# Print the results of aggregated sale per month
print(monthly_revenue)  
# Plot
barplot(
  height = monthly_revenue$order_value_ex_vat_ex_freight / 1e6,
  names.arg = monthly_revenue$date_month,
  main = "Total Monthly Revenue (for 2024) in millions (in DKK)",
  xlab = "Month",
  ylab = "Revenue (ex. VAT & freight)",
  col = "steelblue",
  border = "black"
)
# Most revenue is generated in March and from Sept-Nov with peak in November. 


# Number of sales for every product category
unique(data$product_group_level_1) # 10 major product category / levels
# Count the frequency of each product category and convert to a data frame
category_counts <- as.data.frame(sort(table(data$product_group_level_1), decreasing = TRUE))
colnames(category_counts) <- c("category", "count")  # Rename columns for clarity
# Plot
ggplot(category_counts, aes(x = reorder(category, -count), y = count)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "black") +  # Barplot
  labs(
    title = "Most Popular Product Categories",
    x = "Product Categories",
    y = "Number of Sales"
  ) +
  scale_y_continuous(
    labels = comma,  # Format with commas
    breaks = seq(0, 150000, by = 25000)  # Manually set breaks at clean intervals
  ) +
  theme_minimal(base_size = 14) +  # Clean theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    plot.title = element_text(hjust = 0.5, face = "bold")  # Center-align and bold title
  )
# concl: The most popular categories are by far Furniture, followed by Bathroom, Textiles and Homeware.
# The least popular categories are Bed Linen which barely exists and Windows. 


# Revenue per product category
# Aggregate total revenue per product category
category_revenue <- aggregate(
  order_value_ex_vat_ex_freight ~ product_group_level_1,
  data = data,
  FUN = sum
)
# Rename columns for clarity
colnames(category_revenue) <- c("category", "total_revenue")
# Sort categories by total revenue in descending order
category_revenue <- category_revenue[order(category_revenue$total_revenue, decreasing = TRUE), ]

# Determine rounded y-axis limits and breaks
y_max <- ceiling(max(category_revenue$total_revenue) / 10000000) * 10000000  # Round up to nearest 10 million
y_breaks <- seq(0, y_max, length.out = 7)  # Create 7 evenly spaced breaks

# Plot
ggplot(category_revenue, aes(x = reorder(category, -total_revenue), y = total_revenue)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "black") +  # Barplot
  labs(
    title = "Revenue Per Product Category",
    x = "Product Categories",
    y = "Total Revenue (in millions)"
  ) +
  scale_y_continuous(
    labels = comma_format(scale = 1e-6, suffix = "M"),  # Format y-axis with rounded values in millions
    breaks = y_breaks  # Use rounded custom breaks
  ) +
  theme_minimal(base_size = 14) +  # Clean theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    plot.title = element_text(hjust = 0.5, face = "bold")  # Center-align and bold title
  )
# Concl: Furniture is by far the top revenue-generating category, followed by Mattresses and Garden. 
# On the other hand, Bed Linen and Windows contribute the least to the revenue.

