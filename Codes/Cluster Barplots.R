# I will first load tidyverse package to load the pipe operator
library(tidyverse)

# I will first factor Annual Income and Year Birth as they are currently in strings
smartfresh_cluster <- smartfresh_cluster %>%
  mutate(Year_Birth = recode(Year_Birth, `1` = "Adults", `2` = "Matured-Adults", `3` = "Seniors", `4` = "Youngsters"))
smartfresh_cluster <- smartfresh_cluster %>%
  mutate(Annual_Income = recode(Annual_Income, `1` = "High Income", `2` = "Low Income",`3` = "Lower Middle Income", `4` = "Middle Income", `5` = "Upper Middle Income "))
smartfresh_cluster <- smartfresh_cluster %>%
  mutate(Cluster = recode(Cluster, `1` = "Elite-Consumers", `2` = "Economical-Consumers", `3` = "Budget-Consumers"))

# I will now bring total spending based on products and clusters for interpretations
product.clus <- smartfresh_cluster %>%
  pivot_longer(cols = c(Spend_Wine, Spend_OrganicFood, Spend_Meat, Spend_WellnessProducts, Spend_Treats, Spend_LuxuryGoods), 
               names_to = "Product", 
               values_to = "Spending") %>%
  group_by(Cluster, Product) %>%
  summarise(Total_Spending = sum(Spending), .groups = "drop")

# Let's view the bar chart now
ggplot(product.clus, 
       aes(x = Product, y = Total_Spending, fill = as.factor(Cluster))) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Total Amount Spent by Cluster and Product",
       x = "Product",
       y = "Total Amount Spent",
       fill = "Cluster") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# I will now bring total offer accepted based on offers and clusters for interpretations
offer.clus <- smartfresh_cluster %>%
  pivot_longer(cols = c(Accepted_Offer1, Accepted_Offer2, Accepted_Offer3, Accepted_Offer4, Accepted_Offer5, Response_Latest), 
               names_to = "Offer", 
               values_to = "Acceptance") %>%
  group_by(Cluster, Offer) %>%
  summarise(Total_Acceptances = sum(Acceptance), .groups = "drop")

# Let's view the bar chart now
ggplot(offer.clus, 
       aes(x = Offer, y = Total_Acceptances, fill = as.factor(Cluster))) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Total Offer Accepted by Cluster and Offer",
       x = "Offer",
       y = "Total Offer Accepted",
       fill = "Cluster") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# I will now bring total purchases based on channels and clusters for interpretations
channel.clus <- smartfresh_cluster %>%
  pivot_longer(cols = c(Purchases_Online, Purchases_Catalog, Purchases_Store), 
               names_to = "Channel", 
               values_to = "Purchase") %>%
  group_by(Cluster, Channel) %>%
  summarise(Total_Purchases = sum(Purchase), .groups = "drop")

# Let's view the bar chart now
ggplot(channel.clus, 
       aes(x = as.factor(Cluster), y = Total_Purchases, fill = Channel)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Total Number of Purchases by Cluster and Channel",
       x = "Cluster",
       y = "Total Number of Purchases",
       fill = "Channel") +  
  theme_minimal()

# I will now bring customer counts based on age group and clusters for interpretations
age.clus <- smartfresh_cluster %>%
  group_by(Cluster, Year_Birth) %>%
  summarise(Customer_Count = n(), .groups = "drop")

# Let's view the bar chart now
ggplot(age.clus, 
       aes(x = as.factor(Cluster), y = Customer_Count, 
           fill = as.factor(Year_Birth))) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Number of Customers by Age Group and Cluster",
       x = "Cluster",
       y = "Number of Customers",
       fill = "Age Group") +  
  theme_minimal()

# I will now bring customer counts based on income level and clusters for interpretations
income.clus <- smartfresh_cluster %>%
  group_by(Cluster, Annual_Income) %>%
  summarise(Customer_Count = n(), .groups = "drop")

# Let's view the bar chart now
ggplot(income.clus, 
       aes(x = as.factor(Cluster), y = Customer_Count, 
           fill = as.factor(Annual_Income))) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Number of Customers by Income Level and Clusters",
       x = "Cluster",
       y = "Number of Customers",
       fill = "Income Level") +  
  theme_minimal()