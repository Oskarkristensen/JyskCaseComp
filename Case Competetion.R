#CASE COMPETETION: S360 X JYSK 
#_______________________________________________________________________________

library(readxl)
library(DataExplorer)

data <- read_excel("jysk_case_competition_final.xlsx")
View(data)

sum(is.na(data))
plot_missing(data)
#Do to low percentage of missing values, rows with missing values will be removed 

