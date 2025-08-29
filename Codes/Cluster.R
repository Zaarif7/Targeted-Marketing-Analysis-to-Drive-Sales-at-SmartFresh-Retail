# First I will introduce a new variable where I will take the total amount spent on all the products for interesting analysis
smartfresh
smartfresh$total_purchase_spent <- rowSums(smartfresh[, c("Spend_Wine", "Spend_OrganicFood", "Spend_Meat", "Spend_WellnessProducts", "Spend_Treats", "Spend_LuxuryGoods")], na.rm = TRUE)

# Now I will be turning the features that are in string into integers.
smartfresh$Year_Birth <- as.integer(smartfresh$Year_Birth)
smartfresh$Education_Level <- as.integer(smartfresh$Education_Level)
smartfresh$Marital_Status <- as.integer(smartfresh$Marital_Status)
smartfresh$Annual_Income <- as.integer(smartfresh$Annual_Income)

# These will be my final variables for clustering 
variables_clus <- c('Year_Birth', 'Education_Level', 'Marital_Status', 'Annual_Income', 'Kidhome', 'Teenhome', 'total_purchase_spent')
smartfresh.clus <- smartfresh[variables_clus]

# I will now standardise the dataset
clus.scaled <- as.data.frame(lapply(smartfresh.clus,scale))
summary(clus.scaled)

# Now I will use a randomiser to train my dataset
set.seed(2500)

# I have decided to go with 10 possible iterations for clustering
k.max <- 10

# Since there will be different values of k now, I will have to run multiple iterations
wss <- sapply(1:k.max, 
              function(k){kmeans(clus.scaled, 
                                 k, nstart=50, 
                                 iter.max = 15 )$tot.withinss})

# To determine the optimal k value, I will be generating an elbow chart now
wss
plot(1:k.max, wss, type="b", pch = 19, frame = FALSE, xlab="K-values", ylab="Sum of squares within clusters", main = "Elbow Chart")

# As my elbow chart showe 3 as the optimal value of k, I will run it for k=3
clusters <- kmeans(clus.scaled, 3)
clusters

# Time to look at all the measures in clustering
clusters
clusters$tot.withinss
clusters$betweenss
clusters$size
clusters$withinss
clusters$centers

# The measures looked good to me, I will proceed with generating the cluster chart
library(cluster)
clusplot(clus.scaled, clusters$cluster, color = TRUE, shade = TRUE, labels = 4, lines = 0, main = "Cluster Diagram on K-Means")

# I will now create cluster columns and give them cluster values so that I can connect these cluster values at the very end of my discussions
smartfresh_cluster <- smartfresh
smartfresh_cluster$Cluster <- clusters$cluster