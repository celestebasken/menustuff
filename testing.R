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

## Calculating the total spend based on the Sales Batch Unit (lbs)
# Turning the Quantity Sales Batch Unit into two columns for ~math~
Beef_Price_Clean <- Beef_Price_Clean %>%
  separate(Quantity_sales_batch_unit,
           into = c("QS_batch_value", "QS_batch_unit"),
           sep = " ",
           convert = TRUE)  # Converts to numeric

# Turning Price_excl_vat_/_sales_unit into two columns for ~math~, p2
Beef_Price_Clean <- Beef_Price_Clean %>%
  mutate(
    price_sales_per_unit = as.numeric(str_extract(Price_excl_vat_per_sales_unit, "\\d+\\.\\d+")),
    price_sales_unit = str_extract(Price_excl_vat_per_sales_unit, "(?<=/ )\\w+")
  )

# Making a new column 
# First gotta make things numeric
Beef_Price_Clean <- Beef_Price_Clean %>%
  mutate(
    QS_batch_value = as.numeric(QS_batch_value),
    price_sales_per_unit = as.numeric(price_sales_per_unit),
    )
  )
# then multiply, adding NAs if needed
Beef_Price_Clean <- Beef_Price_Clean %>%
  mutate(
    Spend_based_on_sales_batch_unit = if_else(
      QS_batch_unit == price_sales_unit,
      QS_batch_value * price_sales_per_unit,
      NA_real_
    )
  )

## Calculating the total spend based on the Sales Batch Unit:
# Turning the Quantity Base Unit into two columns for ~math~
Beef_Price_Clean <- Beef_Price_Clean %>%
  separate(Quantity_base_unit,
           into = c("QS_base_value", "QS_base_unit_unit"),
           sep = " ",
           convert = TRUE)  # Converts to numeric
# aaaand price
Beef_Price_Clean <- Beef_Price_Clean %>%
  mutate(
    price_base_per_unit = as.numeric(str_extract(Price_excl_vat_per_base_unit, "\\d+\\.\\d+")),
    price_base_unit = str_extract(Price_excl_vat_per_base_unit, "(?<=/ )\\w+")
  )
# Making a new column 
# First gotta make things numeric
  # check this with str(Beef_Price_Clean)
Beef_Price_Clean <- Beef_Price_Clean %>%
  mutate(
    QS_base_value = as.numeric(QS_base_value),
  )
)
# then multiply, adding NAs if needed
Beef_Price_Clean <- Beef_Price_Clean %>%
  mutate(
    Spend_based_on_base_unit = if_else(
      QS_base_unit_unit == price_base_unit,
      QS_base_value * price_base_per_unit,
      NA_real_
    )
  )

# They seem to be the same!! Yippee

# PART II: adding it all together in a new dataframe, slimming down to just one row per item

# New dataframe
Beef_Product_Price <- Beef_Price_Clean

# Let's clean this up plsss
Beef_Product_Price$QS_batch_value  <- NULL
Beef_Product_Price$QS_batch_unit <- NULL
Beef_Product_Price$Price_excl_vat_per_sales_unit <- NULL
Beef_Product_Price$QS_base_value  <- NULL
Beef_Product_Price$QS_base_unit_unit <- NULL
Beef_Product_Price$Price_excl_vat_per_base_unit <- NULL
Beef_Product_Price$price_sales_per_unit <- NULL
Beef_Product_Price$price_sales_unit <- NULL


# Total spend per product, based on sales batch (arbitrarily chosen since they seem the same)
# Adding this to the Product Price df
Beef_Product_Price <- Beef_Product_Price %>%
  group_by(Product) %>%
  mutate(Total_spend = sum(Spend_based_on_sales_batch_unit, na.rm = TRUE)) %>%
  ungroup()

# Now a new df with just one row per item
Beef_Product_Summary <- Beef_Price_Clean %>%
  group_by(Product) %>%
  summarise(
    Total_spend_base = sum(Spend_based_on_sales_batch_unit, na.rm = TRUE),
    Total_quantity_lbs = sum(QS_base_value, na.rm = TRUE),
    Average_price_per_lb = mean(price_base_per_unit, na.rm = TRUE),
    Producer = first(Producer),
    Meat_certs = first(Meat_certs),
    Sustainable = first(Sustainable),
    Product_Code = first(Product_code),
    Supplier = first(Supplier),
    .groups = "drop"
  )

# Let's save this new df
write.csv(Beef_Product_Summary,"~/Downloads/Beef_Product_Summary.csv", row.names = FALSE)
