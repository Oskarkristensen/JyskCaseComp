#CASE COMPETETION: S360 X JYSK 
#_______________________________________________________________________________

library(readxl)
library(DataExplorer)
library(dplyr)
library(lubridate)

data <- read_excel("jysk_case_competition_final.xlsx")
colnames(data)

# > colnames(data)
# [1] "date"                         
# [2] "order_id"                     
# [3] "customer_id"                  
# [4] "product_id"                   
# [5] "order_value_ex_vat_ex_freight"
# [6] "product_title"                
# [7] "customer_zip_code"            
# [8] "product_group_level_1"        
# [9] "product_category_level_2" 

sum(is.na(data))

# > sum(is.na(data))
# [1] 54766

plot_missing(data)
#Do to low percentage of missing values, rows with missing values will be removed 

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

str(data)

dim(data)
