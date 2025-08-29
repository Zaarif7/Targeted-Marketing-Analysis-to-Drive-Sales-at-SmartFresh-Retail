# I will first load corrplot package to generate correlation heatmap between the selected variables.
library(corrplot)
# I will also install psych for performing varimax rotation on the PCA
install.packages("psych")
library(psych)

# I will first scale the dataset so that the variables are in a comparable scale
smartfresh.scaled <- smartfresh
factors.pca <- c('Spend_Wine', 'Spend_OrganicFood', 'Spend_Meat', 'Spend_WellnessProducts', 'Spend_Treats', 'Spend_LuxuryGoods')

# I will now standardise the dataset
smartfresh.scaled[,factors.pca] <- data.frame(scale(smartfresh[,factors.pca]))
summary(smartfresh.scaled)

# Now I will be creating a correlation heatmap to see how strongly are each variables related before performing PCA
corrplot(cor(smartfresh.scaled[, factors.pca]), method = 'color', order="hclust") 
title(main = "Correlation Heatmap on Spendings", line = 2)

# Since the correlation was determined, let's now perform PCA
spending.pca <- prcomp(smartfresh.scaled[,factors.pca], scale=TRUE)

# Let's see the summary of the PCA and see which variables has more variance that's getting explained
summary(spending.pca)

# Time to create the scree plot
screeplot(spending.pca, type = "lines", main = "Scree Plot on Spendings")

# Time to create the biplot
biplot(spending.pca, 
       xlabs = rep(".", nrow(spending.pca$x)), 
       main='Biplot on Spendings')

# Since the plots have been created, to get better insights, we will now run the cummulative variance
cumsum(summary(spending.pca)$importance[2,])
summary(spending.pca)$importance[3,]

# From the insights, I saw that 3 variables were only important for my further explanations, so I will create a new dataset with only those 3 components
spendingfinal.pca <- spending.pca$x[,1:3]
head(spendingfinal.pca)

# As the new dataset has been created with the 3 components, I will now view the PCA values
spending.pca$rotation

# Done! Final step would be to run varimax rotation and get the final insights
spendingrotated.pca <- principal(smartfresh.scaled[, factors.pca], 
                          nfactors = 3, rotate = "varimax")
spendingrotated.pca$loadings