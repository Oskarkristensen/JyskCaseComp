###=============================================================================
### 1. DATA CLEANING & PREPROCESSING (JYSK)
###=============================================================================

library(readxl)
library(dplyr)
library(lubridate)
library(DataExplorer)

# Load dataset
data <- read_excel("jysk_case_competition_final.xlsx")

# Standardize category levels
data$product_group_level_1 <- as.character(data$product_group_level_1)
data$product_group_level_1[data$product_group_level_1 == "Bed linen"] <- "Bed Linen"
data$product_group_level_1 <- as.factor(data$product_group_level_1)

# Handle missing values
plot_missing(data)
data <- na.omit(data)

# Date conversion and positive order filtering
data <- data %>%
  mutate(date = dmy(date)) %>%
  filter(order_value_ex_vat_ex_freight > 0)

# Convert relevant columns
for (i in colnames(data)[c(4, 6:9)]) {
  data[[i]] <- as.factor(data[[i]])  
}
data$customer_id <- as.character(data$customer_id)

###=============================================================================
### 2. BINARY MATRIX CREATION (JYSK)
###=============================================================================

library(recommenderlab)
library(reshape2)

# Sample data. Is set to 20000 for computional reasons. 
set.seed(123)
data_small <- sample_n(data, 20000)

# Binary matrix is created
# value.var indicates if the product was bought or not, based on customer_id and product_value 
# Changes row names for customers_id to create a matrix and deletes it afterwards. 
# Converting to binaryratingMatrix
data_mat <- dcast(data_small, customer_id ~ product_title, value.var = "order_value_ex_vat_ex_freight", fill = 0)
rownames(data_mat) <- data_mat$customer_id
data_mat$customer_id <- NULL
binary_data <- as(as.matrix(data_mat), "binaryRatingMatrix")

# Top 10 products sold in sample

# Creating dataframe
items_bought <- data.frame(
  prodcut_name = names(colCounts(binary_data)),
  bought = colCounts(binary_data)
)
top_ten_items <- items_bought[order(items_bought$bought, decreasing = TRUE), ][1:10, ] 

# Plotting to 10 customers 
ggplot(top_ten_items) + 
  aes(x = prodcut_name, y = bought) + 
  geom_bar(stat = "identity", fill = "black", color = "dodgerblue2") + 
  xlab("Item Name") + 
  ylab("Count") +
  ggtitle("Top 10 Products Sold") +
  theme(axis.text = element_text(angle = 40, hjust = 1))

# Filter sparse users/items
# With the rates, noise is removed to create stability
# Choosing products that has been bought more than 10 users and choosing customers who have bought items more than 5 times 
# Filte reapplied 
rates <- binary_data[rowCounts(binary_data) > 5, colCounts(binary_data) > 10]
rates <- rates[rowCounts(rates) > 5,]

###=============================================================================
### 3. Choosing Models to test (JYSK) - BINARY DATA
###=============================================================================

### Recommendations
## Get an overview of different recommend-er models
# Overview of methods that can be used for binaryRatingMatrix 
recommenderRegistry$get_entries(dataType="binaryRatingMatrix")
recommender_models <- recommenderRegistry$get_entries(dataType="binaryRatingMatrix")
names(recommender_models)

# > names(recommender_models)
# [1] "HYBRID_binaryRatingMatrix"       "ALS_implicit_binaryRatingMatrix" "AR_binaryRatingMatrix"          
# [4] "IBCF_binaryRatingMatrix"         "POPULAR_binaryRatingMatrix"      "RANDOM_binaryRatingMatrix"      
# [7] "RERECOMMEND_binaryRatingMatrix"  "UBCF_binaryRatingMatrix"

lapply(recommender_models,"[[","description")

# With the following code, the method is explained - it is possible to insert different methods for check 
recommender_models$ALS_implicit_binaryRatingMatrix

# We use models suited for binary (implicit) purchase data:
#   
# IBCF: Recommends items often bought together (Jaccard similarity).
# UBCF: Recommends based on similar users’ purchases (Jaccard).
# ALS: Factorization model tailored for implicit data.
# RANDOM: Baseline for performance comparison.

###=============================================================================
### 4. MODEL PREDICTION & EVALUATION (JYSK) - BINARY DATA
###=============================================================================

# Create evaluation scheme for binary data using 4-fold cross-validation
scheme <- evaluationScheme(data = rates, method = "cross-validation", k = 4, given = 5, goodRating = 1)

# Define recommender models to evaluate
models_bin <- list(
  IBCF   = list(name = "IBCF", param = list(method = "Jaccard")),
  UBCF   = list(name = "UBCF", param = list(method = "Jaccard")),
  ALS    = list(name = "ALS", param = NULL),
  RANDOM = list(name = "RANDOM", param = NULL)
)

# Define number of recommendations to evaluate
n_rec_bin <- c(1, 5, seq(10, 100, 10))

# Evaluate models on the binary JYSK data
# Automatically split into a train and test set 
results_bin <- evaluate(x = scheme, method = models_bin, n = n_rec_bin)

# Plot ROC and Precision-Recall curves
plot(results_bin, annotate = 1, legend = "topleft", main = "Binary ROC Curve")
plot(results_bin, "prec/rec", annotate = 1, legend = "bottomright", main = "Binary Precision-Recall")

# Based on the plot it looks like IBCF is the best model as it perform most accurately. 
# That being said, it also recommends fewer items. 
# The worst performing model based on the plot is the random/base-line model. This means
# that all of the models are performing better than random guessing. 

###=============================================================================
### 5. MODEL PREDICTIONS - TOP-N RECOMMENDATIONS (JYSK)
###=============================================================================

# Build individual models on the training set for prediction
recommender_ibcf <- Recommender(getData(scheme, "train"), method = "IBCF", param = list(method = "Jaccard"))
recommender_ubcf <- Recommender(getData(scheme, "train"), method = "UBCF", param = list(method = "Jaccard"))
recommender_als  <- Recommender(getData(scheme, "train"), method = "ALS")
recommender_rand <- Recommender(getData(scheme, "train"), method = "RANDOM")

# Predict top 10 recommendations for each model
n_recommend <- 10
pred_ibcf <- predict(recommender_ibcf, getData(scheme, "known"), n = n_recommend)
pred_ubcf <- predict(recommender_ubcf, getData(scheme, "known"), n = n_recommend)
pred_als  <- predict(recommender_als,  getData(scheme, "known"), n = n_recommend)
pred_rand <- predict(recommender_rand, getData(scheme, "known"), n = n_recommend)

###=============================================================================
### 6. EVALUATE RECOMMENDATIONS AGAINST ACTUAL PURCHASES
###=============================================================================

# Evaluate Top-N list accuracy (TP, FP, Precision, Recall)
acc_ibcf <- calcPredictionAccuracy(pred_ibcf, getData(scheme, "unknown"), given = 5, goodRating = 1)
acc_ubcf <- calcPredictionAccuracy(pred_ubcf, getData(scheme, "unknown"), given = 5, goodRating = 1)
acc_als  <- calcPredictionAccuracy(pred_als,  getData(scheme, "unknown"), given = 5, goodRating = 1)
acc_rand <- calcPredictionAccuracy(pred_rand, getData(scheme, "unknown"), given = 5, goodRating = 1)

###=============================================================================
### 7. MODEL ACCURACY SUMMARY TABLE - BINARY (JYSK)
###=============================================================================

# Combine evaluation results into a summary table
accuracy_summary <- data.frame(
  Model = c("IBCF", "UBCF", "ALS", "RANDOM"),
  Precision = c(acc_ibcf["precision"], acc_ubcf["precision"], acc_als["precision"], acc_rand["precision"]),
  Recall    = c(acc_ibcf["TPR"],       acc_ubcf["TPR"],       acc_als["TPR"],       acc_rand["TPR"])
)

print(accuracy_summary)

# > print(accuracy_summary)
# Model   Precision     Recall
# 1   IBCF 0.050000000 0.08611111
# 2   UBCF 0.033333333 0.09444444
# 3    ALS 0.008333333 0.08333333
# 4 RANDOM 0.016666667 0.05000000

# Based on the accuracy summary, ICBF is the best performing model, as it has the highest
# precision and is performing well on recall. Also here, it can bee concluded that
# the random model is the worst performing model. 
# This makes the IBCF the chosen model. 

###=============================================================================
### 8. FINAL IBCF MODEL: USER RECOMMENDATIONS (JYSK)
###=============================================================================

# Build final IBCF model on the full filtered binary dataset
final_ibcf_model <- Recommender(data = rates, method = "IBCF", param = list(method = "Jaccard"))

# Generate recommendations for all users
n_final_recommend <- 5
final_ibcf_predictions <- predict(final_ibcf_model, newdata = rates, n = n_final_recommend)

# Convert predictions to list of product names per user
final_recommendation_list <- lapply(final_ibcf_predictions@items, function(x) {
  colnames(rates)[x]
})

# Preview recommendations for first 5 users
names(final_recommendation_list) <- rownames(rates)
head(final_recommendation_list, 5)

# > head(final_recommendation_list, 5)
# $`1018118361`
# [1] "Airer LEIFHEIT Pegasus 16m clothes line" 
# [2] "Bar stool JONSTRUP white/oak"            
# [3] "Bedside table HEMDRUP 1 drawer oak/black"
# [4] "Bookcase GISLINGE 5 shelves white"       
# [5] "Coffee table JEGIND Ø80 white/natural"   
# 
# $`101812725`
# [1] "Bar stool JONSTRUP white/oak"      
# [2] "Bench BADSTED w/storage light grey"
# [3] "BTH ENGBLOMME 220x240 rose"        
# [4] "Coffee table LEJRE 48x85 white/oak"
# [5] "Dining chair ADSLEV beige/oak"     
# 
# $`1018127357`
# [1] "Artificial flower LAUST H38cm grey"  
# [2] "Bench BADSTED w/storage beige"       
# [3] "Desk VANDBORG 60x120 light oak/black"
# [4] "DUV 950g VIKANES warm 135x200cm"     
# [5] "Picture frame OSCAR 40x50cm black"   
# 
# $`1018127365`
# [1] "Airer LEIFHEIT Pegasus 16m clothes line"
# [2] "Bench BADSTED w/storage beige"          
# [3] "Dining table JEGIND 80x130 oak/black"   
# [4] "Dining table AABENRAA 90x160 oak/black" 
# [5] "Mirror MARSTAL Ø70 black"               
# 
# $`1018130861`
# [1] "Armchair w/footstool HVILSTED off-white"
# [2] "BT MALUNG 70x140cm grey KR"             
# [3] "DUV 1260g VIKANES WARM 180x200cm"       
# [4] "DUV 950g ULVIK warm 135x200cm"          
# [5] "THR BONDEROSE 130x170 fleece blue"

# Get recommendations for a specific user ID (example: the 3rd user in the matrix)
user_id <- rownames(rates)[3]
final_recommendation_list[[user_id]]

# > final_recommendation_list[[user_id]]
# [1] "Artificial flower LAUST H38cm grey"  
# [2] "Bench BADSTED w/storage beige"       
# [3] "Desk VANDBORG 60x120 light oak/black"
# [4] "DUV 950g VIKANES warm 135x200cm"     
# [5] "Picture frame OSCAR 40x50cm black" 

# Unlist all recommendations and count frequencies
all_recs_flat <- unlist(final_recommendation_list)
rec_freq_table <- as.data.frame(table(all_recs_flat))
top_recs <- rec_freq_table[order(rec_freq_table$Freq, decreasing = TRUE), ][1:10, ]

# Plot top recommended products
library(ggplot2)
ggplot(top_recs, aes(x = reorder(all_recs_flat, -Freq), y = Freq)) +
  geom_bar(stat = "identity", fill = "blue") +
  xlab("Product") + ylab("Frequency") +
  ggtitle("Top 10 Most Frequently Recommended Products") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

