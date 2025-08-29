# Load Packages
library(tidyverse)
library(lubridate)
options(width=100)

# Labels
# 1-"Elite-Consumers", 2-"Economical-Consumers", 3-"Budget-Consumers"

# Mean of Promo Purchases by Clusters
plot_test <- smartfresh_cluster
plot_test_test <- plot_test %>% 
  group_by(Cluster) %>% 
  summarise(mean=mean(Promo_Purchases), n=n())
plot_test_test

# Plot the Promo Purchases Distribution
ggplot(plot_test) + 
  geom_histogram(aes(x=Promo_Purchases), binwidth=1) + 
  facet_grid(Cluster~.) + xlim(-1,16) + 
  labs(x="Number of Purchases during Promotion", 
       y="Frequency") + 
  geom_vline(data=plot_test_test, 
             mapping=aes(xintercept=mean), 
             col="green")

# Filter the Dataset (Cluster 1 and 2)
main.data.cluster1n2 <- smartfresh_cluster %>%
  filter(Cluster %in% c("Elite-Consumers", "Economical-Consumers"))

# Mean of Promo Purchases by Clusters
promo.by_cluster1n2 <- main.data.cluster1n2 %>% 
  group_by(Cluster) %>% 
  summarise(mean=mean(Promo_Purchases), n=n())
promo.by_cluster1n2

# Summary of Mean Differences
promo.by_cluster1n2 %>% summarise(difference=diff(mean))

# Plot the Cluster-wise Promo Purchases Distribution
ggplot(main.data.cluster1n2) + 
  geom_histogram(aes(x=Promo_Purchases), binwidth=1) + 
  facet_grid(Cluster~.) + xlim(-1,16) + 
  labs(x="Number of Purchases during Promotion", 
       y="Frequency") + 
  geom_vline(data=promo.by_cluster1n2, 
             mapping=aes(xintercept=mean), 
             col="green")

# Performing T-test
t.test(Promo_Purchases~Cluster, data=main.data.cluster1n2)

# Filter the Dataset (Cluster 3 and 2)
main.data.cluster3n2 <- smartfresh_cluster %>%
  filter(Cluster %in% c("Budget-Consumers", "Economical-Consumers"))

# Mean of Promo Purchases by Clusters
promo.by_cluster3n2 <- main.data.cluster3n2 %>% 
  group_by(Cluster) %>% 
  summarise(mean=mean(Promo_Purchases), n=n())
promo.by_cluster3n2

# Summary of Mean Differences
promo.by_cluster3n2 %>% summarise(difference=diff(mean))

# Plot the Cluster-wise Promo Purchases Distribution
ggplot(main.data.cluster3n2) + 
  geom_histogram(aes(x=Promo_Purchases), binwidth=1) + 
  facet_grid(Cluster~.) + xlim(-1,16) + 
  labs(x="Number of Purchases during Promotion", 
       y="Frequency") + 
  geom_vline(data=promo.by_cluster3n2, 
             mapping=aes(xintercept=mean), 
             col="green")

# Performing T-test
t.test(Promo_Purchases~Cluster, data=main.data.cluster3n2)

# Filter the Dataset (Cluster 3 and 1)
main.data.cluster3n1 <- smartfresh_cluster %>%
  filter(Cluster %in% c("Budget-Consumers", "Elite-Consumers"))

# Mean of Promo Purchases by Clusters
promo.by_cluster3n1 <- main.data.cluster3n1 %>% 
  group_by(Cluster) %>% 
  summarise(mean=mean(Promo_Purchases), n=n())
promo.by_cluster3n1

# Summary of Mean Differences
promo.by_cluster3n1 %>% summarise(difference=diff(mean))

# Plot the Cluster-wise Promo Purchases Distribution
ggplot(main.data.cluster3n1) + 
  geom_histogram(aes(x=Promo_Purchases), binwidth=1) + 
  facet_grid(Cluster~.) + xlim(-1,16) + 
  labs(x="Number of Purchases during Promotions", 
       y="Frequency") + 
  geom_vline(data=promo.by_cluster3n1, 
             mapping=aes(xintercept=mean), 
             col="green")

# Performing T-test
t.test(Promo_Purchases~Cluster, data=main.data.cluster3n1)