# Load Packages
library(tidyverse)
library(corrplot)
library(ggplot2)
library(dplyr)
library(tidyr)

# Create a Dataset for Visualization
main.eda <- read_csv('/Users/jawadzaarif7/Desktop/38159/SmartFresh Retail.csv')
main.eda$Spend_Total <- rowSums(main.eda[, c("Spend_Wine", "Spend_OrganicFood", "Spend_Meat", "Spend_WellnessProducts", "Spend_Treats", "Spend_LuxuryGoods")], na.rm = TRUE)
main.eda$Purchases_Total <- rowSums(main.eda[, c("Purchases_Online", "Purchases_Catalog", "Purchases_Store" )], na.rm = TRUE)
main.eda$Education_Level <- as.factor(main.eda$Education_Level)
main.eda$Marital_Status <- as.factor(main.eda$Marital_Status)

# Plot the Year_Birth Box Plot
boxplot(main.eda$Year_Birth, xlab="Year of Birth", main="Boxplot: Year of Birth", horizontal=TRUE)

# Plot the Annual_Income Box Plot
boxplot(main.eda$Annual_Income, xlab="Annual Income", main="Boxplot: Annual Income", horizontal=TRUE)

# Plot the Spend_Wine Box Plot
boxplot(main.eda$Spend_Wine, xlab="Amount Spent on Wine", main="Boxplot: Amount Spent on Wine", horizontal=TRUE)

# Plot the Spend_OrganicFood Box Plot
boxplot(main.eda$Spend_OrganicFood, xlab="Amount Spent on Organic Food", main="Boxplot: Amount Spent on Organic Food", horizontal=TRUE)

# Plot the Spend_Meat Box Plot
boxplot(main.eda$Spend_Meat, xlab="Amount Spent on Meat", main="Boxplot: Amount Spent on Meat", horizontal=TRUE)

# Plot the Spend_WellnessProducts Box Plot
boxplot(main.eda$Spend_WellnessProducts, xlab="Amount Spent of Wellness Product", main="Boxplot: Amount Spent of Wellness Product", horizontal=TRUE)

# Plot the Spend_Treats Box Plot
boxplot(main.eda$Spend_Treats, xlab="Amount Spent of Treats", main="Boxplot: Amount Spent on Treats", horizontal=TRUE)

# Plot the Spend_LuxuryGoods Box Plot
boxplot(main.eda$Spend_LuxuryGoods, xlab="Amount Spent on Luxury Goods", main="Boxplot: Amount Spent on Luxury Goods", horizontal=TRUE)

# Plot the Year of Births Histogram
hist(main.eda$Year_Birth, 
     main="Histogram: Year of Birth",
     xlab="Years",
     ylab="Frequency",
     breaks=30,
     col="lightgreen") 

# Plot the Total Annual Income Histogram
hist(main.eda$Annual_Income, 
     main="Histogram: Annual Income",
     xlab="Annual Income",
     ylab="Frequency",
     breaks=30,
     col="lightgreen") 

# Plot the Total Spending Histogram
hist(main.eda$Spend_Total, 
     main="Histogram: Total Amount Spent on Purchasing",
     xlab="Total Amount Purchased",
     ylab="Frequency",
     breaks=30,
     col="lightgreen") 

# Plot the Total Purchases Histogram
hist(main.eda$Purchases_Total, 
     main="Histogram: Total Number of Purchases",
     xlab="Number of Purchases",
     ylab="Frequency",
     breaks=30,
     col="lightgreen") 

# Pair Plot
filtered_data <- main.eda %>%
  filter(!Customer_ID %in% c('7829','1150','11004','9432',
                             '8475','1503','5555','1501',
                             '5336','4931','11181'))
pairs(formula = ~Year_Birth + Annual_Income + Spend_Total + Purchases_Total + Promo_Purchases, data=filtered_data, cex=0.5)

# Plot the Correlation Heatmap
cor_matrix <- cor(main.eda[, c('Year_Birth', 'Annual_Income', 'Spend_Total', 'Purchases_Total','Promo_Purchases')],
  use = "complete.obs")
corrplot(cor_matrix, 
         method = 'color',
         addCoef.col = "blue",
         tl.col = "black"
         )

# Plot the Education Level Bar Plot
ggplot(main.eda, aes(x = Education_Level)) + 
  geom_bar(fill = "yellow") +  
  labs(title = "Bar Plot: Level of Education",
       x = "Level of Education",
       y = "Frequency") +
  theme_minimal()

# Plot the Marital Status Bar Plot
ggplot(main.eda, aes(x = Marital_Status)) + 
  geom_bar(fill = "yellow") +  
  labs(title = "Bar Plot: Marital Status",
       x = "Marital Status",
       y = "Frequency") +
  theme_minimal()

# Plot the Kid Home Bar Plot
ggplot(main.eda, aes(x = Kidhome)) + 
  geom_bar(fill = "yellow") +  
  labs(title = "Bar Plot: Number of Kids at home",
       x = "Number of Kids",
       y = "Frequency") +
  theme_minimal()

# Plot the Teen Home Bar Plot
ggplot(main.eda, aes(x = Teenhome)) + 
  geom_bar(fill = "yellow") +  
  labs(title = "Bar Plot: Number of Teens at home",
       x = "Number of Teens",
       y = "Frequency") +
  theme_minimal()

# Plot the Offers Bar Plot
offer_summary <- main.eda %>%
  summarise(across(c('Accepted_Offer1', 'Accepted_Offer2', 'Accepted_Offer3', 'Accepted_Offer4', 'Accepted_Offer5', 'Response_Latest'), 
                   sum, na.rm = TRUE)) %>%
  pivot_longer(cols = everything(), 
               names_to = "Offers", 
               values_to = "Total")
ggplot(offer_summary, aes(x = Offers, y = Total)) +
  geom_bar(stat = "identity", fill = 'yellow') +  
  labs(title = "Number of Offers Accepted",
       x = "Offers",
       y = "Frequency",
       ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot the Purchases Bar Plot
purchase_summary <- main.eda %>%
  summarise(across(c('Purchases_Online', 'Purchases_Store', 'Purchases_Catalog'), 
                   sum, na.rm = TRUE)) %>%
  pivot_longer(cols = everything(), 
               names_to = "Offers", 
               values_to = "Total")
ggplot(purchase_summary, aes(x = Offers, y = Total)) +
  geom_bar(stat = "identity", fill = 'yellow') +  
  labs(title = "Number of Purchases by Channels",
       x = "Channels",
       y = "Number of Purchases",
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))