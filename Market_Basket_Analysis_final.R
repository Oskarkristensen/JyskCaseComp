# Market Basket Analysis (Association Rules)


# Load packages
library(readxl)
library(plyr)
library(tidyverse)
library(arules)
library(arulesViz)
library(RColorBrewer)
rm(list=ls())

# Load data 
data <- read_excel("jysk_case_competition_final.xlsx")
View(data)

###### Prepare the data for analysis 

# Delete all Missing Values
data <- data[complete.cases(data), ] # Filters out any rows that contain missing values (NA). This ensures data quality before grouping or rule mining.

# Convert Columns (Feature Engineering)
data <- data %>% 
  mutate(date = as.Date(date, format = "%d.%m.%Y")) %>%
  mutate(order_id = as.character(order_id)) %>%
  mutate(customer_id = as.character(customer_id)) %>%
  mutate(product_id = as.character(product_id)) %>%
  mutate(price = as.numeric(order_value_ex_vat_ex_freight)) %>%
  mutate(product = as.character(product_title)) %>%
  mutate(zip_code = as.character(customer_zip_code)) %>%
  mutate(product_group = as.factor(product_group_level_1)) %>%
  mutate(product_category = as.factor(product_category_level_2)) 

# Clean 'Bed Linen'
data$product_group_level_1[data$product_group_level_1 == "Bed linen"] <- "Bed Linen"


# Get a glimpse of your data.
glimpse(data) 
#Rows: 398,783
#Columns: 14
#$ date             <date> 2024-01-01, 2024-01-01, 2024-01-01, 2024-01-01, 2024-01-01, 2024-01-01, 202…
#$ order_id         <chr> "4045956663", "4045956669", "4045956669", "4045956755", "4045956913", "40459…
#$ customer_id      <chr> "1018108478", "10188560", "10188560", "101837935", "101853520", "101853520",…
#$ product_id       <chr> "3617186", "4240100", "4241700", "4050685", "2512600", "2512600", "2771241",…
#etc. 


# Market Basket Analysis (MBA) is about finding product combinations within the same transaction, which 
# is typically represented by an order_id. So i will now group by order_id, so we collect all items brought
# together to find combinations. 

# Prepare Transaction Data for Basket Analysis
transactionData <- plyr::ddply(data,c("order_id"),
                               function(df1)paste(df1$product,collapse = ","))
# Groups the data by order_id (i.e., one basket = one order).
# For each group (order_id), it concatenates all items purchased into a single string separated by commas.
# This data-variabel is based on only a string of products in each basket and order_id (two variables)


# Check the new data frame out which have order_id and a string of products brought
head(transactionData)

# Remove unnecessary columns (order_id) from transactionData.
transactionData$order_id <- NULL 
# This leaves us with only 1 column: A string of item names seperated by commas.

# Rename column to "items".
colnames(transactionData) <- c("items") 

# Check we only have one column consisting of a string of different item names. 
head(transactionData)

# Save transactions to CSV file
write.csv(transactionData,"market_basket_transactions_jysk.csv", quote = FALSE, 
          row.names = FALSE)
# Saves the prepared data to a CSV file. quote = FALSE: avoids putting quotes around text values. 
# row.names = FALSE: doesn't include row numbers in the file.


######  Load the Data from the CSV file holding Transactions into arules and start finding association rules

# Load the CSV file holding transaction data we just created above
tr <- read.transactions('market_basket_transactions_jysk.csv', format = 'basket', 
                        sep=',')  # tr is the result of reading the data into Arules
# Now tr is a special transactions object, ready for rule mining.

summary(tr)
  #158321 rows (elements/itemsets/transactions) - There are 158.321 transactions (orders)
  # 4094 columns (items) - Across those transactions, there are 4,094 unique products (items).
  
  #most frequent items 
  #DUV 1320g BAGN warm 200x220cm  -> 1661                                    
  #Clothes rail GUDME double black/chrome  -> 1230
  #Folding table KULESKOG W75xL180 white  -> 1196   
  # PIL 750g TRONFJELLET 50x70cm  ->  1178 
  #BTH ENGBLOMME 220x240 rose ->  1048    
  # These are the top 5 most purchased products, with their frequency (number of times they appear across
  # all transactions).
  
  #    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  #   1.000   1.000   1.000   1.627   2.000  37.000 
  # On average, transactions contain 1.63 items.
  # Half of the transactions contain only 1 item (median = 1).
  # The most items in one transaction is 37.


###### Association Rule Mining with Apriori Algorithm - look for association rules 

# Min Support as 0.001, confidence as 0.8.
association.rules <- apriori(tr, 
                             parameter = list(supp=0.0005, conf=0.4,maxlen=10)) 
# Minimum support = 0.05% of all transactions → itemsets must appear in at least 79 transactions.
# Minimum confidence = 40% → the rule must be true at least 40% of the time. We value high confidence for business strategy/insigths even though it only gives 5 rules.
# Maxlen = 10 -> The total number of items in a rule (LHS + RHS) must be ≤ 10.


# Check the selected rules that passes the parameteres above
summary(association.rules)
#set of 52 rules
#rule length distribution (lhs + rhs):sizes
#2 3 -> This is the number of items in each rule
#49 3 -> This is the rules

# We got 52 rules despite using very large dataset consisting of 158.321 transactions. 49 rules are a length of 2 items, 
# meaning each of these rules consists of one item on the left-hand side (LHS) and one on the right-hand side (RHS).
# 3 rules are a length of 3 items. 


# Lets have a look at two of the rules, we have found (for inspection purposes)
inspect(association.rules[1:2]) 
#     lhs                                 rhs                           support      confidence coverage    lift     count
#[1] {Pouffe VILDSUND 52x41 off-white} => {Armchair VILDSUND off-white} 0.0006379444 0.404      0.001579070 248.8781 101  
#[2] {MA 60x186x12}                    => {5cm PLUS F15 chair}          0.0012948377 1.000      0.001294838 772.2976 205 


# Lets find the most interesting and unique rules by removing redundant rules 

# Removing redundant rules - get subset rules in vector
subset.rules <- which(colSums(is.subset(association.rules,association.rules))>1) # Identifies redundant rules (rules that are subsets of others with same or better performance).
length(subset.rules)  # 31 rules should be removed. 

# Remove these 31 subset rules - Removes redundant rules from the rule set.
subset.association.rules. <- association.rules[-subset.rules] 

# Lets sort the rules based on Lift, so we have the strongest associations rules  
sortedRules <- sort(subset.association.rules.,by="lift",decreasing=TRUE)  
# This sorts the rules that are back by lift - a measure of rule interestingness (higher = stronger relationship).

# Lets now display the 21 rules that are back - These should be interesting, unique and strong
inspect(sortedRules[1:21]) 
# This will be the final rules, we will move on with as they are all strong. The "weakest" rule has a lift of 77.12, 
# which is ok. 



# Now Lets find the most profitable rules out of the 21 rules. 
# This will help identity which item combinations are 
# not just common, but also financially valuable for Jysk. 

# First get item/product names from the rules 
item_names <- unique(unlist(LIST(items(sortedRules), decode = TRUE))) # This extracts all products involved in rules

# Check that the product names appear in item_names by looking at 10 products
head(item_names, 10) 
#[1] "BT MALUNG 70x140cm cream KR"       "TO MALUNG 50x100cm cream KR"       "BT NORA 70x140cm dusty rose KR"   
#[4] "TO NORA 50x100cm dusty rose KR"    "End table TAPS 40x40 white/bamboo" "End table TAPS 55x55 white/bamboo"
#[7] "BT YSBY 65x130cm beige"            "TO YSBY 50x90cm beige"             "BT YSBY 65x130cm dark grey"       
#[10] "TO YSBY 50x90cm dark grey" 



# Simulate per-item margin data - Simulate item profit margins. Since we dont have cost data, we will simulate margins
set.seed(03870)
item_margins <- data.frame(margin=rnorm(length(item_names),
                                        mean=0.30, sd=0.30))
# Simulates random profit margins (normally distributed) for each item.
# Mean margin = 0.30 (30%), SD = 0.30.
# Each item is given a hypothetical profitability value by doing this simulation. 

# Add items names to each item margin
rownames(item_margins) <- item_names

# Quick look at some of the margins 
head(item_margins, 3)
#                               margin
#BT MALUNG 70x140cm cream KR    0.8834036
#TO MALUNG 50x100cm cream KR    0.5296409
#BT NORA 70x140cm dusty rose KR 0.2211481
# Here we see 3 out of the 21 items. All these 3 have positive margins. 


# Inspect margin data
quantile(item_margins$margin)
#     0%        25%        50%        75%       100% 
#-0.3041718  0.1415396  0.3213632  0.4146024  0.8834036 
# This shows the range of margins (some can be negative, representing losses or promotions).
# This provides a summary of the distribution of the simulated item profit margins 
# The lowest margin in this simulated dataset is -30.4%, which indidate min. one product is sold with loss 
# On the other hand, the highest margin is 88.3%, which indicate some products are sold with very high profit. 
# 25% of the products have margins less than or equal to 14.15%, so conversely, 75% of items have margins greater than 14.15%
# This indicates that some items have negative margins - Lets find out if the rules (consisting of multiply items) 
# have positive or negative margins. 

####### Margins for each association rule

# This next part of the script defines and tests a custom function called retail.margsum() that calculates 
# the total profit margin for one or more items, transactions, or rules.

# Make a more generic function to do the calculations
margsum <- function(items, itemMargins) {
  # Input: 
  #     "items" == item names, rules or transactions in arules format
  #     "itemMargins" == a data frame of profit margin indexed by name
  # Output: 
  #     look up the item margins, and return the sum
  library(arules)
  # check the class of "items" and coerce appropriately to an item list
  if (class(items) == "rules") {
    tmp.items <- as(items(items), "list") # rules ==> item list
  } else if (class(items) == "transactions") {
    tmp.items <- as(items, "list") # transactions ==> item list
  } else if (class(items) == "list") {
    tmp.items <- items # it’s already an item list!
  } else if (class(items) == "character") {
    tmp.items <- list(items) # characters ==> item list
  } else {
    stop("Don’t know how to handle margin for class ", class(items))
  }
  # make sure the items we found are all present in itemMargins
  good.items <- unlist(lapply(tmp.items, function (x)    # This checks if all item names in each list element exist in retail.margin.
    all(unlist(x) %in% rownames(itemMargins))))
  if (!all(good.items)) {
    warning("Some items not found in rownames of itemMargins. ",
            "Lookup failed for element(s):\n",
            which(!good.items), "\nReturning only good values.")
    tmp.items <- tmp.items[good.items]
  }
  # and add them up
  return(unlist(lapply(tmp.items, function(x) sum(itemMargins[x, ]))))   # For each item set (transaction or rule), sum up their margins and return the result.
}

# Sum margins for rules (e.g. lhs + rhs of each rule).
margsum(sortedRules, item_margins)
#  [1] 1.4130445 0.1832100 0.7316484 0.1723309 0.4915326 1.1445311 0.3818787 0.7200134 0.9262289 0.1788984 0.3792600 1.1475999
# [13] 0.7412630 0.4768984 0.3310779 0.7232487 1.0476860 0.5352036 0.6872760 0.1444324 0.4001684 
# This is all the margins for each rule (sorted by lift). 

# Lets now connect each margin to the rule (add the products involved in the margins)

# Convert rules to readable labels
rule_labels <- labels(sortedRules)
# Get total margins per rule
rule_margins <- margsum(sortedRules, item_margins)
# Get support, confidence, and lift
rule_quality <- quality(sortedRules)
# Combine everything into a clean summary data frame (without the Items column)
rule_summary <- data.frame(
  Rule = rule_labels,
  Support = round(rule_quality$support, 6),
  Confidence = round(rule_quality$confidence, 3),
  Lift = round(rule_quality$lift, 2),
  Total_Margin = round(rule_margins, 4),
  stringsAsFactors = FALSE
)
# Sort by total margin (descending)
rule_summary_sorted <- rule_summary[order(-rule_summary$Total_Margin), ]
# Show top 10 most profitable rules
head(rule_summary_sorted, 21)
#                                                                         Rule  Support Confidence   Lift Total_Margin
#1              {BT MALUNG 70x140cm cream KR} => {TO MALUNG 50x100cm cream KR} 0.000556      0.571 380.12       1.4130
#12               {BT MALUNG 70x140cm grey KR} => {TO MALUNG 50x100cm grey KR} 0.000771      0.526 247.78       1.1476
#6                  {BT NORA 70x140cm white KR} => {TO NORA 50x100cm white KR} 0.000663      0.515 304.06       1.1445
#17          {TO MALUNG 50x100cm dark grey KR} => {TO MALUNG 50x100cm grey KR} 0.000834      0.406 191.38       1.0477
#9      {BT MALUNG 70x140cm dark grey KR} => {TO MALUNG 50x100cm dark grey KR} 0.000802      0.529 257.78       0.9262
#13             {TO MALUNG 50x100cm cream KR} => {TO MALUNG 50x100cm beige KR} 0.000758      0.504 236.17       0.7413
#3  {End table TAPS 40x40 white/bamboo} => {End table TAPS 55x55 white/bamboo} 0.000505      0.503 326.47       0.7316
#16         {TO MALUNG 50x100cm dark grey KR} => {TO MALUNG 50x100cm beige KR} 0.000840      0.409 191.69       0.7232
#8        {BT NORA 70x140cm dusty blue KR} => {TO NORA 50x100cm dusty blue KR} 0.000524      0.459 281.40       0.7200
#19      {Back cushion DUNHAMMER 35x75 beige} => {BTH DUNHAMMER 220x240 beige} 0.001503      0.810 166.23       0.6873
#18               {RB Duo FEMRIS 45x180 taupe} => {RB Duo FEMRIS 60x180 taupe} 0.000524      0.437 166.65       0.5352
#5                 {BT YSBY 65x130cm dark grey} => {TO YSBY 50x90cm dark grey} 0.000613      0.548 309.87       0.4915
#14                     {TO YSBY 50x90cm pink} => {TO YSBY 50x90cm dusty blue} 0.000764      0.498 234.63       0.4769
#21        {Back cushion ENGBLOMME 60x90 grey} => {BTH ENGBLOMME 220x240 grey} 0.000733      0.498  77.12       0.4002
#7          {BT MALUNG 70x140cm beige KR} => {BT MALUNG 70x140cm dark grey KR} 0.000575      0.429 283.16       0.3819
#11         {Pouffe VILDSUND 52x41 off-white} => {Armchair VILDSUND off-white} 0.000638      0.404 248.88       0.3793
#15              {BT YSBY 65x130cm dusty blue} => {TO YSBY 50x90cm dusty blue} 0.000657      0.491 231.15       0.3311
#2        {BT NORA 70x140cm dusty rose KR} => {TO NORA 50x100cm dusty rose KR} 0.000531      0.519 340.63       0.1832
#10             {BT MALUNG 70x140cm beige KR} => {TO MALUNG 50x100cm beige KR} 0.000733      0.547 256.30       0.1789
#4                         {BT YSBY 65x130cm beige} => {TO YSBY 50x90cm beige} 0.000518      0.516 317.70       0.1723
#20        {Back cushion ENGBLOMME 60x90 rose} => {BTH ENGBLOMME 220x240 rose} 0.000973      0.650  98.16       0.1444

# Concl: Out of our 21 strongest association rules, we have now ranked them based on simulated profit margins. 
# These rules are highly valuable to focus on — not only are they non-redundant and backed by high lift scores,
# but they also demonstrate solid profitability. Promoting the product combinations identified in these
# rules has the potential to boost overall sales performance.

# For example, customers who purchase BT MALUNG 70x140 in cream are 380.12 times more likely to also purchase 
# TO MALUNG 50x100 in cream. This is a clear behavioral pattern that could inform in-store product placement,
# suggesting that these items should be displayed near each other.

# Overall, these rules provide important insights for store layout design, online recommendation systems, 
# and customer behavior analysis. Additionally, all rules have positive total margins, meaning that every 
# combination contributes to increased revenue when the items are sold together rather than individually.

# Product pairs with the highest margins are especially suitable for bundling, targeted promotions, and 
# cross-selling strategies. Interestingly, while some individual products may have negative margins on their 
# own, when sold as part of these combinations, they result in positive overall profit, highlighting effective
# sales and marketing opportunities.

# Notably, every rule has a lift greater than 89, indicating very strong associations. These are precisely
# the types of rules that are "golden" for campaigns and business optimization.

# It is important to note that these margins are based on simulated data. In a real-world implementation, 
# actual cost and pricing data should be used to ensure accurate financial insights and actionable impact.
# However they provide a "score" that gives a relative idea of the profitability, and we use them here
# to rank the rules quickly. 

# Lets interpret the next two ones:
# In 52% (confidence=0.526) of cases where someone bought the MALUNG 70x140 in grey, they also buy the matching 
# set in a smaller size. This rule occurs in 0.0771% of all the transactions. The smaller matching towel is 
# 247.78 times more likely to be purchased when the BT MALUNG towel is bought, compared to chance. The 
# combined margin of the two items is about 114.8%. This means Jysk are earning more profit than the selling 
# price (because it is over 100%) - This can happen in simulated data or when the cost is much lower than 
# the price in real world scenarios. 
# In 51% of cases where someone bought the NORA 70x100, they also bought the matching color in smaller size. 
# This rule occurs in 0.0663% of all transactions. The smaller NORA size is 304 times more likely to be 
# purchased when the bigger size is bought compared to chance. The combined margin of the two items is about 
# 114%. 
