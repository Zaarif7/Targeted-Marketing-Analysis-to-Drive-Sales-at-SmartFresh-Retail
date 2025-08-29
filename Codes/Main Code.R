# I will first load tidyverse package to load the pipe operator
install.packages('tidyverse')
library(tidyverse)

# I will now load the dataset given by our professor on Canvas
smartfresh <- read.csv('/Users/jawadzaarif7/Desktop/38159/SmartFresh Retail.csv')

# I will now view at the dataset
summary(smartfresh)
str(smartfresh)

# Since there were some missing values in Annual Income, I will fill them up by grouping the Education Level 
# And taking a mean of it so that my mean values don't vary too much for same Education Level groups
smartfresh <- smartfresh %>% 
  group_by(Education_Level) %>% 
  mutate(Annual_Income=
           ifelse(is.na(Annual_Income),
                  mean(Annual_Income, na.rm=TRUE),
                  Annual_Income
  ))

# Since there were some nonsense Marital Status like "Absurd" and "Yolo", I will turn them all into Single and finally have 5 different Marital Status
smartfresh$Marital_Status <-
  ifelse(smartfresh$Marital_Status %in%
           c('Divorced',
             'Married',
             'Single',
             'Together',
             'Widow'),
         smartfresh$Marital_Status, 'Single')

# I will now turn the Year_Birth into age and create bins to group them into 4 for better understanding when we go deeper into the analysis
smartfresh <- smartfresh %>% 
  mutate(Year_Birth = case_when(
    Year_Birth >= 1990 ~ 'Youngsters',
    Year_Birth >= 1975 ~ 'Adults',
    Year_Birth >= 1955 ~ 'Matured-Adults',
    TRUE ~ 'Seniors'
  ))

# I will now create bins for Annual Income for better understanding further into the analysis
smartfresh <- smartfresh %>% 
  mutate(Annual_Income = case_when(
    Annual_Income <= 20000 ~ 'Low Income',
    Annual_Income <= 40000 ~ 'Lower Middle Income',
    Annual_Income <= 80000 ~ 'Middle Income',
    Annual_Income <= 120000 ~ 'Upper Middle Income',
    TRUE ~ 'High Income'
  ))

# Since the values are in strings, I will turn them into factors or else they would be treated as individuals instead of groupings further in the analysis
smartfresh$Education_Level <- as.factor(smartfresh$Education_Level)
smartfresh$Marital_Status <- as.factor(smartfresh$Marital_Status)
smartfresh$Year_Birth <- as.factor(smartfresh$Year_Birth)
smartfresh$Annual_Income <- as.factor(smartfresh$Annual_Income)
