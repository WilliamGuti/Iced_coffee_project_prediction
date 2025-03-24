
file.choose()
paht_coffee <- 'add your file .csv path'
df_coffee <- read.csv(path_coffee)
df_coffee
str(df_coffee)

# $ record_id        : [candidate key to determine duplicates] int  1 2 3 4 4 5 6 7 8 8 ...
# $ coffee_type      : [SI] [NOMINAL CATEGORICAL] chr  "Arabica" "Robusta" "Arabica" "Blend" ...
# $ milk_type        : [SI] [NOMINAL CATEGORICAL] chr  "Plant-based" "Whole" "Skim" "Skim" ...
# $ flavor_additions : [I/D] [NOMINAL CATEGORICAL] chr  "None" "Mocha" "None" "Vanilla" ...
# $ packaging_type   : [SI] [NOMINAL CATEGORICAL] chr  "Plastic" "Paper" "Paper" "Glass" ...
# $ serving_size     : [I/D] [ORDINAL CATEGORICAL] chr  "Medium" "Medium" "Medium" "Small" ...
# $ texture_level    : [I/D] [NOMINAL CATEGORICAL] chr  "Smooth" "Smooth" "Thin" "Creamy" ...
# $ bitterness_level : [I/D] [NUMERIC] num  0.349 0.926 0.409 0.735 0.735 ...
# $ sugar_pct        : [I/D] [NUMERIC] num  0.738 0.455 0.579 0.392 0.392 ...
# $ foam_thickness   : [I/D] [NUMERIC] num  0.383 0.533 0.257 0.293 0.293 ...
# $ caffeine_strength: [I/D] [NUMERIC] num  0.534 0.505 0.833 0.66 0.66 ...
# $ price            : [I/D] [NUMERIC] num  3.5 4.99 5.99 6.99 6.99 4.99 3.99 2 3 3 ...
# $ discount_pct     : [I/D] [NUMERIC] num  6.64 6.3 7.57 8.52 8.52 ...
# $ aftertaste       : [I/D] [NUMERIC] num  0.65 0.29 0.486 0.729 0.729 ...
# $ aroma_strength   : [I/D] [NUMERIC] num  0.705 0.442 0.313 0.291 0.291 ...
# $ satisfaction     : [SD] [NUMERIC] num  5.12 3.59 4.76 4.98 4.98 ...
# $ churn_rate       : [SD] [NUMERIC] num  0.526 0.602 0.652 0.605 0.605 ...
# $ record_timestamp : [I/D] [DATE/INTEGER] chr  "2024-03-05" "2024-07-15" "2024-09-07" "2024-11-29" ...
# $ buy_again        : [SD] [NOMINAL CATEGORICAL] chr  "Yes" "No" "No" "No" ...

#Assumptions:
#Milk type choice is purely based on personal preference and not affected by any of the coffee characteristics or other variables
#Packaging type was changed randomly by the company implementing new ideas to see which one would be preferred by customers
#Serving size depends on the price/discount (customers may get larger sizes if they are priced lower)
#Syrup and sugar depend on the coffee bean that is used and its taste, bitterness, etc. People may change their preferences based on the taste that the coffee beans have
#Price is dependent on the type of beans and other ingredients used (such as the milk type and flavour additions)
#Foam level is dependent on the type of milk that is used since some milk types tend to foam up more than others
#record_timestamp is classified as I/D because it is generally independent of the other variables but depends on the record_id (without record_id, there can be no record_timestamp)


#changing date variable into date type
df_coffee$record_timestamp <- as.Date(df_coffee$record_timestamp, format = '%Y-%m-%d')
str(df_coffee)

#to be able to better statistically analyze this variable, we are changing "buy_again" to numeric
#if buy_again == 'Yes', it will be represented by 1. If it is 'No', it will be represented by 0
df_coffee$buy_again <- ifelse(df_coffee$buy_again == 'Yes', 1,
                              ifelse(df_coffee$buy_again == 'No', 0, NA))
str(df_coffee)

##CHECKING FOR/HANDLING DUPLICATES

summary(df_coffee)

#there are duplicates. first one is the 5th observation in the dataframe
anyDuplicated(df_coffee$record_id)


#seems like a lot of the the second duplicated entries in the output have NAs
#so it makes sense to keep the bottom to top values since the first one that
#appears in the list in this ouput is the bottom to top entry. Also, the dates in the bottom to top order are also more recent
#so this may mean that corrections were made to the observations at a later date and we want to keep these updated observations.
df_coffee[duplicated(df_coffee$record_id) | duplicated(df_coffee$record_id, fromLast = T), ]


#removing duplicates in the top to bottom order
df_coffee <- df_coffee[!duplicated(df_coffee$record_id), ]
df_coffee

#checking to see if removing duplicates worked, and it did because this returns 0 (duplicates as index 0 so there are none left)
anyDuplicated(df_coffee$record_id)


#running summary again to check NAs (significantly decreased). flavor_additions, bitterness, satisfaction, churn rate, and buy_again are the only variables left with NAs
summary(df_coffee)

#Before handling NAs, we will be analyzing outliers. The reason we handle these before is because we will be filling NAs using statistical methods
#and if we have outliers that might be entry errors or need to be handled, they can be corrected before any statistical work is done. This way, we prevent
#incorrectly skewing the data 


#creating dataset with only the numeric variables:
coffee_numeric_vars <- df_coffee[, 8:17] 
str(coffee_numeric_vars)


#seeing if there are outliers in any of the variables
for(i in 1:ncol(coffee_numeric_vars)){
  boxplot(coffee_numeric_vars[[i]], col = terrain.colors(ncol(coffee_numeric_vars))[i],
          main = paste("Boxplot of", names(coffee_numeric_vars)[i]),
          xlab = names(coffee_numeric_vars)[i])
}

#churn_rate has outliers. We will not be treating these outliers because we do not believe that they represent inaccurate data. 
#Inidividuals may never buy the brand again or become a regular customer and only buy this brand from now on. Changing these values
#would actually lead to inaccuracies about customer activity

#satisfaction has outliers but we have decided to not alter these as the satisfaction can range from 0-10 this depends on the individual's opinions. There may not be many 
#people who rate is on the lower or higher extremes but we cannot remove and invalidate those who have. This would lead to inaccurate data.

#aftertaste has some outliers and we will treat these because they are in the negative values. This does not make sense with the data as there
#cannot be negative aftertase values. We are assuming that these are data entry errors and a low rating value was intended to be entered.
#therefore, we will be pulling these values up to the minimum value (0):

df_coffee$aftertaste[df_coffee$aftertaste < 0] <- 0

summary(df_coffee)

#now the outliers of aftertaste are handled. however, the max value for aftertaste can be 1 but the summary shows 1.4791
#we are going to make the same assumptions about data entry error here and apply the same method to bring these values down the the max (1):

df_coffee$aftertaste[df_coffee$aftertaste > 1] <- 1

summary(df_coffee) #now, the min is 0 and max is 1

#caffeine_strength also has outliers that are below the lower IQR range. However, because the strength of the coffee can range from 0 to 1
#and these values fall above 1, we cannot change them and will leave them as is. 

#sugar_pct also has outliers but similar to the reasons with previous variables, we will not be treating these values. Sugar percentage ranges from 0 - 1
#and all of the values fall within this range. There may not be many that are too high or low but these values are still valid regardless of how frequently 
#they appear in the dataset. Changing them would lead to inaccurate data.


#handling NAs and cleaning data:
#order of handling NAs: [SI] --> [I/D] [FACTORS] --> [I/D] [NUMERIC] --> [SD]

#assuming that the NA flavor_additions means there was no flavor added, we are replacing all the NAs with 'None'
df_coffee$flavor_additions <- factor(ifelse(is.na(df_coffee$flavor_additions), "None", df_coffee$flavor_additions))

summary(df_coffee)

#changing the categorical variables into factor
df_coffee[2:7] <- lapply(df_coffee[2:7], factor)
str(df_coffee)

#treating NAs in bitterness

#creating smaller sample of the numeric variables since shapiro test can only compute up to 5000 observations
sample_numeric_vars <- coffee_numeric_vars[sample(nrow(coffee_numeric_vars), 5000),]
sample_numeric_vars

lapply(sample_numeric_vars, shapiro.test)

#$bitterness_level: p-value < 2.2e-16
#$sugar_pct: p-value = 2.597e-06
#$foam_thickness: p-value < 2.2e-16
#$caffeine_strength: p-value = 3.726e-13
#$price: p-value < 2.2e-16
#$discount_pct: p-value < 2.2e-16
#$aftertaste: p-value = 7.275e-12
#$aroma_strength: p-value = 2.413e-08
#$satisfaction: p-value = 8.305e-06
#$churn_rate: p-value = 0.04448

#since most of the p-values for the variables are less than 0.05, we can conclude that the data is not normally distributed
#so we will proceed to do the shapiro test using the spearman method
library(dplyr)

df_coffee %>% select(where(is.numeric)) %>% chart.Correlation(method = 'spearman')

bitterness_train <- df_coffee[!is.na(df_coffee$bitterness_level), ]
bitterness_test <- df_coffee[is.na(df_coffee$bitterness_level), ]

#from the correlation chart, bitterness_level has the highest correlation with sugar_pct, aftertaste, satsifaction, and churn_rate.
#since satisfaction and churn_rate have NAs, we cannot use them in these regression models so we will need to use sugar_pct and aftertaste
lm_bitterness <- lm(bitterness_level ~ sugar_pct + aftertaste, data = bitterness_train)
summary(lm_bitterness)
#residual standard error is 0.1565
#r squared is 0.3839

step(lm_bitterness)

#from the step() results above:
#RSS when keeping both variables is 345.41
#RSS when removing aftertase is 387.55
#RSS when removing sugar_pct is 477.85
#Since RSS is lowest with both variables in the regression model, we will keep both as this is most accurate model

nlm_bitterness <- loess(bitterness_level ~ sugar_pct + aftertaste, data = bitterness_train, control = loess.control(surface = 'direct' ))
summary(nlm_bitterness)
#residual standard error is 0.1549

#The nlm_bitterness regression model seems to have to lowest residual standard error so this is the best model to handle the NAs for biterness_level

#applying nlm_bitterness to the NAs in bitterness_level:

predict(nlm_bitterness, newdata = bitterness_test)
bitterness_test$bitterness_level <- predict(nlm_bitterness, newdata = bitterness_test)
bitterness_test

df_coffee[is.na(df_coffee$bitterness_level), ] <- bitterness_test
summary(df_coffee)

#next, we will treat NAs in satisfaction using a similar method as before
#satisfaction has the strongest correlation with bitterness_level, sugar_pct, caffeine_strength, price, discount_pct, and aftertaste
#as satisfaction can only be tested with a maximum of 4 variables in regression models, we will be using the 4 with the strongest correlation
#and then try different models be rearranging the variables to see which one will give us the best model

satisfaction_train <- df_coffee[!is.na(df_coffee$satisfaction), ]
satisfaction_test <- df_coffee[is.na(df_coffee$satisfaction), ]

lm1_satisfaction <- lm(satisfaction ~ bitterness_level + sugar_pct + discount_pct + aftertaste, data = satisfaction_train)
summary(lm1_satisfaction)
#residual standard error is 0.4931
#r squared is 0.8556

step(lm1_satisfaction)

#RSS with all variables is 3463.6
#RSS when removing bitterness_level is 3652.0
#RSS when removing sugar_pct is 7444.0
#RSS when removing discount_pct is 7743.9
#RSS when removing aftertaste is 7861.3
#the lowest RSS is with all of the variables but removing bitterness_level does not seem to have too much of an impact as the other variables do
#so we will try replacing this with one of the other strongest correlation variables to see if anything changes:


lm2_satisfaction <- lm(satisfaction ~ price + sugar_pct + discount_pct + aftertaste, data = satisfaction_train)
summary(lm2_satisfaction)
#residual standard error is 0.5004
#r squared is 0.8513

lm3_satisfaction <- lm(satisfaction ~ caffeine_strength + sugar_pct + discount_pct + aftertaste, data = satisfaction_train)
summary(lm3_satisfaction)
#residual standard error is 0.4661
#r squared is 0.871

step(lm3_satisfaction)
#RSS with all variables is 3094.6 so this is actually the best model so far

#just trying another model with just 3 variables
lm4_satisfaction <- lm(satisfaction ~ sugar_pct + discount_pct + aftertaste, data = satisfaction_train)
summary(lm4_satisfaction)
#residual standard error is 0.5063 
#r squared is 0.8477

nlm_satisfaction <- loess(satisfaction ~ caffeine_strength + sugar_pct + discount_pct + aftertaste, data = satisfaction_train, control = loess.control(surface = 'direct' ))
summary(nlm_satisfaction)
#residual standard error is 0.4377

#nlm_satisfaction is the best model since it has the lowest residual standard error. we will use this to treat the NAs for satisfaction
predict(nlm_satisfaction, newdata = satisfaction_test)
satisfaction_test$satisfaction <- predict(nlm_satisfaction, newdata = satisfaction_test)
satisfaction_test

df_coffee[is.na(df_coffee$satisfaction), ] <- satisfaction_test
summary(df_coffee)

#next, we will treat NAs in churn_rate by using a similar approach as the previous 2 variables we have treated.
#churn_rate has the strongest correlations with bitterness_level, sugar_pct, caffeine_strength, price, discount_pct, aftertaste, and satisfaction
#we will start by using the strongest correlated variables and then adjust the models by trying different variables and seeing which one gives us the best model

churn_train <- df_coffee[!is.na(df_coffee$churn_rate), ]
churn_test <- df_coffee[is.na(df_coffee$churn_rate), ]


lm1_churn <- lm(churn_rate ~ bitterness_level + sugar_pct + aftertaste + satisfaction, data = churn_train)
summary(lm1_churn)
#residual standard error is 0.101
#r squared is 0.6201

step(lm1_churn)
#RSS with all variables is 142.23
#RSS when removing bitterness is 142.23
#RSS when removing aftertaste is 142.28
#RSS when removing sugar_pct is 142.49
#RSS when removing satisfaction is 214.00

#trying a model by replacing bitterness, aftertaste and sugar_pct since they do not seem to affect the model that much (from RSS above)
lm2_churn <- lm(churn_rate ~ caffeine_strength + price + discount_pct + satisfaction, data = churn_train)
summary(lm2_churn)
#residual standard error is 0.101
#r squared is 0.62

step(lm2_churn)
#from all the RSS values, it seems like the one variable that affects churn_rate the most is satisfaction so we will be trying a model with just satisfaction

lm3_churn <- lm(churn_rate ~ satisfaction, data = churn_train)
summary(lm3_churn)
#residual standard error is 0.101
#r squared is 0.6199

#trying with nlm to see if it will make a difference
nlm_churn <- loess(churn_rate ~ satisfaction, data = churn_train, control = loess.control(surface = 'direct' ))

summary(nlm_churn)
#residual standard error is 0.101

#trying this nlm because it had the highest r squared value
nlm2_churn <- loess(churn_rate ~ bitterness_level + sugar_pct + aftertaste + satisfaction, data = churn_train, control = loess.control(surface = 'direct' ))
summary(nlm2_churn)
#residual standard error is 0.101

#going to use nlm2_churn as the model to fill NAs because the corresponding lm had the highest r-squared value but since not all of the
#variables have normal distributions, we decided that a nlm model will be the best approach to most accurate results

predict(nlm2_churn, newdata = churn_test)
churn_test$churn_rate <- predict(nlm2_churn, newdata = churn_test)
churn_test

df_coffee[is.na(df_coffee$churn_rate), ] <- churn_test
summary(df_coffee)

#finally, to handle NA values in buy_again, we will be using satisfaction and churn_rate since these are the two variables
#that will have the most effect on whether or not a customer buys again from the brand
#we will be using generalized linear models for this variable since it is a logistic variable (categorical with 2 categories)


buyAgain_train <- df_coffee[!is.na(df_coffee$buy_again), ]
buyAgain_test <- df_coffee[is.na(df_coffee$buy_again), ]


glm1_buyAgain <- glm(buy_again ~ satisfaction + churn_rate, data = buyAgain_train, family = binomial)
summary(glm1_buyAgain)

step(glm1_buyAgain)

glm2_buyAgain <- glm(buy_again ~ satisfaction, data = buyAgain_train, family = binomial)
summary(glm2_buyAgain)

#churn_rate is not affecting the models so we will use glm2_buyAgain to handle the NAs in buy_again


predict(glm2_buyAgain, newdata = buyAgain_test, type = 'response')
buyAgain_test$buy_again <- predict(glm2_buyAgain, newdata = buyAgain_test, type = 'response')
buyAgain_test

#the above gives us probabilities that it will be 1 or 0 so we are rounding the ones that are equal to or above 0.5 to 1 ('yes') 
buyAgain_test$buy_again <- ifelse(buyAgain_test$buy_again >= 0.5, 1, 0)
buyAgain_test

df_coffee[is.na(df_coffee$buy_again), ] <- buyAgain_test
summary(df_coffee)

str(df_coffee)
head(df_coffee)

write.csv(df_coffee, file = "C:\\Users\\dendu\\Downloads\\iced_coffee.csv")


file.choose()
path_coffe_clean <- "C:\\Users\\wguti\\OneDrive\\Desktop\\Data Handling & Desition Making\\assingments\\final_assingment\\iced_coffee_clean.csv"
df_coffee_clean <- read.csv(path_coffe_clean)
df_coffee_clean
library(dplyr)

# here, we are going to select relevant columns to our first cluster, 
# (which there are only numerical values),

# we save them into a new variable, DF_PRODUCTS to performe our analysis
# its name will be FLAVOR_PROFILE_CLUSTER
df_products <- df_coffee_clean %>% 
  select(bitterness_level, sugar_pct, aroma_strength)
df_products
# our data is already normlized

# Apply K-Means Clustering
kmeans_flavor <- kmeans(df_products, centers = 3, nstart = 25)

# Add cluster labels
df_products$flavor_cluster <- as.factor(kmeans_flavor$cluster)

# 3D Plot
library(plotly)
plot_ly(df_products, x = ~ aroma_strength, y=~bitterness_level, 
        z=~sugar_pct, type="scatter3d", mode="markers", 
        color = ~flavor_cluster, 
        colors = c("#1f77b4", "#ff7f0e", "#2ca02c")) %>% 
  layout(scene = list(xaxis = list(title = "Aroma"),
                      yaxis = list(title = "Bitterness"),
                      zaxis = list(title = "Sugar")))

# Cluster Interpretation
# 
###############  Axes:
# X axis: Bitterness
# Y axis: Aroma strength
# Z axis: Sugar percentage
# ##############  Colors:
# Blue (Cluster 1) → Coffees with high bitterness, moderate aroma and little sugar.
# Orange (Cluster 2) → Coffees with medium-high sugar, less bitterness and less aroma.
# Green (Cluster 3) → Balanced coffees, with high aroma and greater sweetness.

viability_bussisness <- aggregate(df_coffee_clean[, c("bitterness_level", "sugar_pct", "aroma_strength")], 
                                  by = list(cluster = df_products$flavor_cluster), FUN = mean)
viability_bussisness

# Cluster Analysis
# According to the average data of each cluster group, we can interpret the results as follows:
#
# Cluster 1 (Blue) 
# 76.56% bitterness
# 36.50% sugar
# 50.98% aroma
# 
# This group has stronger and more bitter coffees, with less sugar, so this 
# could attract consumers who prefer black coffee without many added sugars, 
# and they may also be premium coffees and this would be oriented to people who
# prefer the purest coffee
# 
# Cluster 2 (Orange) 
# 44.70% bitterness
# 60.03% sugar
# 32.24% aroma
# 
# These are more balanced coffees, with sweetness and a stronger aroma.
# This profile is ideal and would correspond to more commercial coffees or 
# those with added flavors, and it also attracts consumers looking for a softer 
# and more accessible coffee.
# 
# # Cluster 3 (Green) 
# 43.96% bitterness
# 61.13% sugar
# 66.70% aroma
# 
# Finally, these are sweeter coffees but with less aroma intensity. 
# These could be lower quality coffees or those with production processes that
# affect the aroma. In addition, these could be aimed at less demanding consumers 
# or supermarket coffees.



df_coffee_clean
df_caffeine <- df_coffee_clean %>% 
  select(caffeine_strength, aftertaste, price )
str(df_caffeine)

# Apply K-Means Clustering1
kmeans_caffeine <- kmeans(df_caffeine_scaled, centers = 3, nstart = 25)

# Add cluster labels
df_caffeine$caffeinee_cluster <- as.factor(kmeans_caffeine$cluster)
df_caffeine
# 3D Plot
library(plotly)
plot_ly(df_caffeine, x = ~ caffeine_strength , y=~aftertaste , 
        z=~price  , type="scatter3d", mode="markers", 
        color= ~caffeinee_cluster,
        colors = c("#1f77b4", "#ff7f0e", "#2ca02c")) %>%
  layout(scene = list(xaxis = list(title = "caffeine_strength"),
                      yaxis = list(title = "aftertaste"),
                      zaxis = list(title = "price")))
# 
# #################  Axes:
# X axis: Caffeine Strength
# Y axis: Aftertaste
# Z axis: Price
# ################  Colors:
# Blue (Cluster 1) → Coffees with high caffeine, strong aftertaste and high price.
# Orange (Cluster 2) → Coffees with less caffeine, moderate aftertaste and low price.
# Green (Cluster 3) → Coffees with medium-high caffeine, less aftertaste and moderate price.


viability_bussisness_caffeine <- aggregate(df_coffee_clean[, c("caffeine_strength", "aftertaste", "price")], 
                                           by = list(cluster = df_caffeine$caffeinee_cluster), FUN = mean)
viability_bussisness_caffeine

# Cluster Analysis
# According to the average data of each cluster group, we can interpret the results as follows:
#
# Cluster 1 (Blue)
# 71.32% caffeine
# 84.50% aftertaste
# $6.02 price
# This group of intense and long-lasting coffees, rich in caffeine and with a long aftertaste.
# They are premium products, more expensive and positioned for intense coffee lovers.
#
# Cluster 2 (Orange)
# 46.39% caffeine
# 69.18% aftertaste
# $3.66 price
#
# We also find another group that represents soft and cheap coffees, less caffeinated
# and with a medium aftertaste. They are low-priced products, ideal for individuals who
# prefer a soft coffee.
#
# Cluster 3 (Green)
# 65.71% caffeine
# 34.96% aftertaste
# $5.78 
#
# Finally, the results of this group are coffees that are well-balanced with a 
# minimal aftertaste, medium caffeine content, and medium-high price. They are an
# option for individuals who want a better a more harmonious experience


# #############5 descriptive objectives
#
# First objective
# Explore the relationship between price (price) and customer satisfaction (satisfaction),
# also considering the impact of discounts (discount_pct) on value perception.

library(ggplot2)
df_obj_1 <- df_coffee_clean%>%
  select(c("price", "satisfaction", "discount_pct"))
str(df_obj_1)

library(PerformanceAnalytics)
sample_numeric_vars_df_obj_1<- df_obj_1[sample(nrow(df_obj_1), 5000),]
sample_numeric_vars_df_obj_1
lapply(sample_numeric_vars_df_obj_1, shapiro.test) 

# price <- p-value < 2.2e-16
# satisfaction <- p-value = 7.208e-06
# discount_pct <- p-value < 2.2e-16
# all data it is not notmally distributed.

chart.Correlation(sample_numeric_vars_df_obj_1, method = 'spearman')
# price and satisfaction = 0.33 
# price and discount = 0.76
# satisfaction  discount = 0.42
# we have a goods correlation into out analysis
# taking this in account we will start

# lineal regression
model_bjt_1 <- lm(satisfaction ~ price + discount_pct, data = df_obj_1)
summary(model_bjt_1)


# second objetive 
# Identify consumer preferences for coffee type (Arabica, Robusta, Blend)
# and their relationship with satisfaction and repurchase rate (buy_again)

df_obj_2 <- df_coffee_clean %>% 
  select(c("coffee_type", "satisfaction", "buy_again"))

table(df_obj_2$coffee_type)
prop.table(table(df_obj_2$buy_again)) * 100
library(ggplot2)

# satisfaction and coffee type Boxplot
ggplot(df_obj_2, aes(x = coffee_type, y = satisfaction, fill = coffee_type)) +
  geom_boxplot() +
  labs(title = "Satisfaction by coffee type", x = "Coffee type", y = "Satisfaction") +
  theme_minimal()

# 

ggplot(df_obj_2, aes(x = coffee_type, fill = factor(buy_again))) +
  geom_bar(position = "fill") +
  labs(title = "Repurchase Rate by Coffee Type", x = "Coffee type", y = "Satisfaction", fill = "Buy again") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

# desciptive statistics 
aggregate(satisfaction ~ coffee_type, data = df_obj_2, FUN = mean)
# Arabica: 5.54 (highest satisfaction)
# Blend: 4.79
# Robusta: 4.07 (lowest satisfaction)
# Interpretation: Consumers report the highest satisfaction with Arabica coffee, 
# followed by Blend and Robusta, which has the lowest average satisfaction.

aggregate(satisfaction ~ coffee_type, data = df_obj_2, FUN = median)
# Arabica: 5.49
# Blend: 4.76
# Robusta: 4.06
# Interpretation: The median reinforces the trend observed in the means, showing 
# that Arabica has a higher satisfaction compared to Blend and Robusta.

prop_buy_again <- aggregate(buy_again ~ coffee_type, data = df_obj_2, FUN = mean)
colnames(prop_buy_again) <- c("coffee_type", "buy_again")
print(prop_buy_again)

# Arabica: 65.08% repurchase
# Blend: 42.51% repurchase
# Robusta: 21.95% repurchase

# Interpretation: Consumers are most likely to repurchase Arabica coffee (65.08%), 
# followed by Blend (42.51%) and Robusta (21.95%). This suggests that Arabica not only 
# generates higher satisfaction, but also higher customer loyalty.
# 
# The analysis shows a clear preference for Arabica coffee, as it has the highest
# average (5.54) and median (5.49) satisfaction, as well as the highest repurchase 
# rate (65.08%). In contrast, Robusta has the lowest satisfaction and repurchase rate, 
# suggesting that it is not as popular among consumers.

# test
anova_test_obj_2 <- aov(satisfaction ~ coffee_type, data = df_obj_2)
summary(anova_test_obj_2)
#  Df Sum Sq Mean Sq F value Pr(>F)    
# coffee_type     2   6072  3036.0    2391 <2e-16 ***

# The analysis confirms that satisfaction differs significantly between coffee types.
# This justifies performing a post hoc analysis (such as Tukey HSD) to identify exactly
# which pairs of coffee types have significant differences in satisfaction.

tukey_test_obj_2 <- TukeyHSD(anova_test_obj_2)
print(tukey_test_obj_2)

# Analysis confirms that Arabica has the highest satisfaction, 
# followed by Blend and Robusta, with significant differences between all pairs.

buy_again_table <- table(df_obj_2$coffee_type, df_obj_2$buy_again)
chi_test_obj_2 <- chisq.test(buy_again_table)
print(chi_test_obj_2)

# Chi-square analysis confirms that there is a significant relationship between 
# coffee type and buy_again : (p-value < 2.2e-16)

# predictive model
logistic_model_obj_2 <- glm(buy_again ~ coffee_type + satisfaction, data = df_obj_2, family = binomial)
summary(logistic_model_obj_2)

# Satisfaction is the main predictor of the repurchase decision, while the 
# type of coffee has no significant effect.
