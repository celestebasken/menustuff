# Testing things out

# yum
library(readr)
library(dplyr)
library(tidyr)
library(stringr)

# Beef time
Beef_Price_Report <- read_csv("Beef_Price_Report.csv")
View(Beef_Price_Report)

#Let's clean this shit up
  # Because the data is weirdly formatted to not have product names!? Rofl
Beef_Price_Clean <- Beef_Price_Report %>%
  # Fill down the Product column
  fill(Product, .direction = "down") %>%
  # Remove rows where Product is filled but all other columns are NA
  filter(!if_all(-Product, is.na))

# Delete the extra columns I made, I don't need them 
  # NOT NEEDED ANYMORE
#Beef_Price_Clean$Total_spend_based_on_sales_batch_unit  <- NULL
#Beef_Price_Clean$Total_spend_based_on_base_unit <- NULL

# Turning the Quantity Sales Batch Unit into two columns for ~math~
Beef_Price_Clean <- Beef_Price_Clean %>%
  separate(Quantity_sales_batch_unit,
           into = c("QS_batch_value", "QS_batch_unit"),
           sep = " ",
           convert = TRUE)  # Converts QS_bu_value to numeric

## Calculating the total spend based on the Sales Batch Unit:
# Turning the Quantity Sales Batch Unit into two columns for ~math~
Beef_Price_Clean <- Beef_Price_Clean %>%
  separate(Quantity_sales_batch_unit,
           into = c("QS_batch_value", "QS_batch_unit"),
           sep = " ",
           convert = TRUE)  # Converts QS_bu_value to numeric

# Turning Price_excl_vat_/_sales_unit into two columns for ~math~, p2
Beef_Price_Clean <- Beef_Price_Clean %>%
  mutate(
    price_sales_per_unit = as.numeric(str_extract(Price_excl_vat_per_sales_unit, "\\d+\\.\\d+")),
    price_sales_unit = str_extract(Price_excl_vat_per_sales_unit, "(?<=/ )\\w+")
  )

# Making a new column 



