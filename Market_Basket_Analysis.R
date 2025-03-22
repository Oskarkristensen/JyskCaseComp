# Market Basket Analysis

# Load packages
library(readxl)
library(plyr)
library(tidyverse)
library(arules)
library(arulesViz)
library(RColorBrewer)
rm(list=ls())

data <- read_excel("jysk_case_competition_final.xlsx")
View(data)

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


# Check the new data frame out
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


#  Load the Data from the CSV file holding Transactions into arules

# sep tell how items are separated. In this case you have separated using ',' (comma)
# tr is the result of reading the data into Arules)
tr <- read.transactions('market_basket_transactions_jysk.csv', format = 'basket', 
                        sep=',')
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


# Item Frequency Analysis
# Create an item frequency plot for the top 20 items
if (!require("RColorBrewer")) {
  # install color package of R
  #install.packages("RColorBrewer")
  #include library RColorBrewer
  #  library(RColorBrewer)
}

# Plot the 20 most purchased items based on count 
itemFrequencyPlot(tr,topN=20,type="absolute",col=brewer.pal(8,'Pastel2'), 
                  main="Absolute Item Frequency Plot")  
# DUV 1320g BA GN warm 200x220cm is the most purchased item. 

# Plot the 20 most purchased items based on procentage
itemFrequencyPlot(tr,topN=20,type="relative",col=brewer.pal(8,'Pastel2'),
                  main="Relative Item Frequency Plot") 
# DUV 1320g BA GN warm 200x220cm appears in just over 1% of all transactions, making it the most purchased product.


# Association Rule Mining with Apriori Algorithm - look for association rules 
# Min Support as 0.001, confidence as 0.8.
association.rules <- apriori(tr, 
                             parameter = list(supp=0.0005, conf=0.7,maxlen=10)) # We will maximum look at 10 items, so maximum length of a set is 10.
# Minimum support = 0.05% of all transactions → itemsets must appear in at least 79 transactions.
# Minimum confidence = 80% → the rule must be true at least 70% of the time. We value high confidence for business strategy/insigths even though it only gives 5 rules.
# The total number of items in a rule (LHS + RHS) must be ≤ 10.

# Check the selected rules that passes the parameteres above
summary(association.rules)
  #set of 8 rules
  #rule length distribution (lhs + rhs):sizes
  #2 
  #8 
# We got only 8 rules despite using very large dataset consisting of 158.321 transactions. All rules are of length 2, 
# meaning each rule consists of one item on the left-hand side (LHS) and one on the right-hand side (RHS).
# The rules show relatively high confidence, indicating strong associations when the rule is triggered. However, each rule 
# appears in only about 79 transactions — a very small portion of the total — so the support is low, which may limit their 
# practical usefulness. When we allowed less strictions regarding minimum support and confidence, we only discovered 5 more 
# rules, going from 3 to now 8 rules. This suggests that the dataset contains few strong, frequent patterns under the
# current parameter settings.


#inspect 5 of the 8 association rules
inspect(association.rules[1:5]) 
#lhs                                     rhs                               support      confidence coverage     lift      count
#[1] {MA 60x186x12}                       => {5cm PLUS F15 chair}          0.0012948377 1.0000000  0.0012948377  772.2976 205  
#[2] {5cm PLUS F15 chair}                 => {MA 60x186x12}                0.0012948377 1.0000000  0.0012948377  772.2976 205  
#[3] {5L plastic white}                   => {Basket INFINITY 4}           0.0005242514 1.0000000  0.0005242514 1028.0584  83  
#[4] {5L plastic grey}                    => {Basket INFINITY 4}           0.0005305676 1.0000000  0.0005305676 1028.0584  84  
#[5] {Back cushion DUNHAMMER 35x75 beige} => {BTH DUNHAMMER 220x240 beige} 0.0015032750 0.8095238  0.0018569868  166.2317 238  


# Lets find the most interesting and unique rules 

# Removing redundant rules - get subset rules in vector
subset.rules <- which(colSums(is.subset(association.rules,association.rules))>1) # Identifies redundant rules (rules that are subsets of others with same or better performance).
length(subset.rules)  # 4 rules should be removed. 

# Remove subset rules - Removes redundant rules from the rule set.
subset.association.rules. <- association.rules[-subset.rules] 

# We can sort the rules that are left as the most unique and interesting ones by lift
sortedRules <- sort(subset.association.rules.,by="lift",decreasing=TRUE)  # Sorts rules by lift, a measure of rule interestingness (higher = stronger relationship).

# Display the 4 rules we have identified that are unique, interesting and strong rules for the business strategy 
inspect(sortedRules[1:4]) 
#    lhs                                     rhs                                 support      confidence coverage     lift      count
#[1] {5L plastic white}                   => {Basket INFINITY 4}                 0.0005242514 1.0000000  0.0005242514 1028.0584  83  
#[2] {5L plastic grey}                    => {Basket INFINITY 4}                 0.0005305676 1.0000000  0.0005305676 1028.0584  84  
#[3] {Plate FERDUS Ø19cm stoneware beige} => {Bowl FERDUS Ø15cm stoneware beige} 0.0005937305 0.7175573  0.0008274329  757.3625  94  
#[4] {Back cushion DUNHAMMER 35x75 beige} => {BTH DUNHAMMER 220x240 beige}       0.0015032750 0.8095238  0.0018569868  166.2317 238 

# Every customer (confidence=1) who bougth the 5L white plastic item also bouth the INFINITY 4 basket, same goes for the 5L 
# plastic in grey ->These products are almost always bougth together - Bundle them or place them together in store/online, 
# or suggets it doing checkout. 
# The lift for these two rules are extremely high (1028) - This is a perfect cross-sell. This indicates, that the Basket are not 
# bought on it own, so this specific product pairing is incredible meaningful. 

# About 72% (confidence=0.717) of those who buy the FERDUS plate also buy the matching FERDUS bowl -> Promote as a dinnerware 
# set - customers clearly pair these. 

# About 81% (confidence=0.801) who buy the DUNHAMMER cushion, also buy the matching DUNHAMMER BTH -> Highlith this combination set.

# All the above rules are have strong confidence and strong lifts indicating the products are clearly linked to each other, and 
# not just by chance. They are all strong associations, and should all be either bundled together or recommend one when the 
# other is viewed or added to the chart. 



# This next part of the script dives deeper into targeted rule mining and visualization using the Apriori algorithm. 
# Instead of generating all possible rules, it focuses on specific items (like “METAL”) and how they relate 
# to others. Lets dig deeper into what customers also buy, when they buy the FERDUS plate. 

# For example, to find what customers buy before buying 'FERDUS Bowl' run the 
# following line of code
# lhs = antecedents of metal the left hand side is the unknown
ferdusbowl.association.rules <- apriori(tr, parameter = list(supp=0.0004, conf=0.6),  # We lower the confidence and support level, to get more results. 
                                   appearance = list(default="lhs",rhs="Bowl FERDUS Ø15cm stoneware beige"))

# We get three association rules telling us what customers buy, that might lead to buying the FERDUS Bowl as well.
inspect(ferdusbowl.association.rules)
#lhs                                      rhs                                      support confidence     coverage     lift count
#[1] {Plate FERDUS Ø27cm stoneware beige}  => {Bowl FERDUS Ø15cm stoneware beige} 0.0005684653  0.6617647 0.0008590143 698.4750    90
#[2] {Plate FERDUS Ø19cm stoneware beige}  => {Bowl FERDUS Ø15cm stoneware beige} 0.0005937305  0.7175573 0.0008274329 757.3625    94
#[3] {Plate FERDUS Ø19cm stoneware beige,
#     Plate FERDUS Ø27cm stoneware beige}  => {Bowl FERDUS Ø15cm stoneware beige} 0.0004989862  0.7745098 0.0006442607 817.4744    79 

# All these above have strong associations. They indicate that if a customer buys the FERDUS plate in whatever size or even buy two different 
# sizes of the plate, they are very likely to buy the FERDUS Bowl. This suggets that these FERDUS products should be bundled as a full 
# dining set. 


# What else do customers buy with FERDUS Bowl?
# Similarly, to find the answer to the question Customers who bought METAL 
# also bought.... you will keep FERDUS Bowl on lhs:
# rhs = consequence (decendent)
ferdusbowl.association.rules <- apriori(tr, parameter = list(supp=0.0005, conf=0.6),
                                   appearance = list(lhs="Bowl FERDUS Ø15cm stoneware beige",default="rhs"))

inspect(head(ferdusbowl.association.rules))
#    lhs                                    rhs                                  support      confidence coverage     lift     count
#[1] {Bowl FERDUS Ø15cm stoneware beige} => {Plate FERDUS Ø27cm stoneware beige} 0.0005684653 0.6000000  0.0009474422 698.4750 90   
#[2] {Bowl FERDUS Ø15cm stoneware beige} => {Plate FERDUS Ø19cm stoneware beige} 0.0005937305 0.6266667  0.0009474422 757.3625 94  

# If a customer buys Bowl FERDUS Ø15cm, they are 60% likely to also buy Plate FERDUS Ø27cm.
# If a customer buys Bowl FERDUS Ø15cm, they are 62.7% likely to also buy Plate FERDUS Ø19cm.
# Interpretation: The bowl is strongly tied to the 27cm plate and 19cm plate — customers tend to pair these products. 





# VISUALIZING ASSOCIATION RULES

# Filter Rules with High Confidence (just keep all 4 as we only have 4)
subRules<-subset.association.rules[quality(subset.association.rules)$confidence>0.4]

#Plot SubRules the more red color, the higher is the lift.
plot(subRules)
# Keeps only rules with confidence > 0.4 (meaning them all). These are stronger, more reliable rules. We can see that for many
# rules, confidence is above 0.6 and support is mainly mid, but a few strong support. 

# Interactive plots. The order is the number of items in the rule
plot(subRules,method="two-key plot")
# red = 2 items in the rule 

# Plot the top 4 (n=4)
top4subRules <- head(subRules, n = 4, by = "confidence")
plot(top4subRules, method = "graph",  engine = "htmlwidget")
# Shows the top high-confidence rules as a network graph.
# Each node = item
# Each arrow = rule
# Helps you see item interconnections visually
