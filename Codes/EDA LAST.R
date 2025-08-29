# Load necessary library
library(tidyverse)

# Load dataset
main.eda <- read_csv('/Users/jawadzaarif7/Desktop/38159/SmartFresh Retail.csv')

# Create new calculated columns
main.eda$Age <- 2025 - main.eda$Year_Birth
main.eda$Spend_Total <- rowSums(main.eda[, c("Spend_Wine", "Spend_OrganicFood", "Spend_Meat", 
                                             "Spend_WellnessProducts", "Spend_Treats", "Spend_LuxuryGoods")], 
                                na.rm = TRUE)
main.eda$Purchases_Total <- rowSums(main.eda[, c("Purchases_Online", "Purchases_Catalog", "Purchases_Store")], 
                                    na.rm = TRUE)

# Select key variables for central tendency and variance
variables <- c("Age", "Annual_Income", "Spend_Wine", "Spend_OrganicFood", "Spend_Meat", 
               "Spend_WellnessProducts", "Spend_Treats", "Spend_LuxuryGoods", "Spend_Total",
               "Purchases_Online", "Purchases_Catalog", "Purchases_Store", "Purchases_Total",
               "Promo_Purchases", "Response_Latest")

# Function to compute mean, variance, and standard deviation while handling NA values
calculate_summary <- function(x) {
  data.frame(
    Mean = mean(x, na.rm = TRUE),
    Variance = var(x, na.rm = TRUE),
    Std_Dev = sd(x, na.rm = TRUE)
  )
}

# Apply function to selected variables
summary_table <- main.eda %>%
  summarise(across(all_of(variables), calculate_summary))

# Transpose the table for better readability
summary_table <- summary_table %>%
  pivot_longer(everything(), names_to = c("Variable", ".value"), names_sep = "_")

# Print the table
print(summary_table)