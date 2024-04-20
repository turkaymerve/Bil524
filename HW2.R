#Q1

# veri generate etmek için library
install.packages("MASS")
library(MASS) #MASS hazır paketinin okunabilmesi için 

# 1989 seedli veri seti oluşturma
set.seed(1989)
n <- 200 # observations
x1 <- rnorm(n, mean = 13, sd = 5) # independent variable 1
x2 <- rnorm(n, mean = 7, sd = 3) # independent variable 2
y <- 7 + 2*x1 + 0.5*x2 + rnorm(n, mean = 4, sd = 1) # d. variable

data <- data.frame(y, x1, x2) # üretilen değişkenlerde df oluşturma

head(data) #display df


# faktöriyel değişken üretimi
set.seed(1989)
n <- 200
x1_f <- rnorm(n, mean = 13, sd = 5)
x2_f <- rnorm(n, mean = 7, sd = 3)
y_f <- rnorm(n, mean = 7 + 2*x1 + 0.5*x2, sd = 1)

# x1 to numeric
x1_f_numeric <- as.numeric(x1_f)

# x2 to factor
x2_f_factor <- as.factor(x2_f)

# check
str(x1_f_numeric)
str(x2_f_factor)

#missing data  generation
set.seed(1989)
n <- 200
x1_md <- rnorm(n, mean = 13, sd = 5)
x2_md <- rnorm(n, mean = 7, sd = 3)
y_md <- rnorm(n, mean = 7 + 2*x1 +0.5*x2, sd = 1)

# na check
is_na_x1 <- is.na(x1_md)
is_na_x2 <- is.na(x2_md)

# replace missing with mean
mean_x1_md <- mean(x1, na.rm = TRUE)
x1_filled <- ifelse(is.na(x1_md), mean_x1, x1)

#outlier detection data generation
set.seed(1989)
data <- rnorm(200)

# box plot to check for outliers
boxplot(data)
sort(data) # sort 

# Z-score calculation
z_scores <- scale(data)

# detect outliers
outliers <- abs(z_scores) > 3.29

# upper and lower quantiles
Q1 <- quantile(data, 0.25)
Q3 <- quantile(data, 0.75)

# upper and lower bounds
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# outlier detection
outliers <- data < lower_bound | data > upper_bound

#distributions
set.seed(1989)
data <- rnorm(200)

# Histogram generation
hist(data)

# box plot generation
boxplot(data)

# Q-Q plot generation
qqnorm(data)
qqline(data)

# using quantiles
summary(data)

set.seed(1989)
x <- rnorm(200) # independent variable
y <- 5*x + rnorm(200, mean = 1, sd = 3) # d. variable

correlation <- cor(x, y) # finding correlations

print(correlation) # printing correlations

#multicollinearity

set.seed(1989)
x1 <- rnorm(200) # independent variable 1
x2 <- rnorm(200) # independent variable 2
x3 <- rnorm(200) # independent variable 3
y <- 7*x1 -0.9*x2 + 1.1*x3 + rnorm(200, mean = 4, sd = 0.5) # d. variable

# multiple linear regression
model <- lm(y ~ x1 + x2 + x3)

# summary
summary(model)

# independent variable correlations
correlation_matrix <- cor(data.frame(x1, x2, x3))
print(correlation_matrix)

install.packages("corrplot")
library(corrplot) # corrplot library

# correlation matrix plotting
corrplot(correlation_matrix, method = "color")

# plotting variables vs. d.v
par(mfrow=c(1,2)) # Grafiklerin yan yana yerleştirilmesi için
plot(x1, y, main = "x1 vs. y", xlab = "x1", ylab = "y", col = "yellow", pch = 12)
plot(x2, y, main = "x2 vs. y", xlab = "x2", ylab = "y", col = "red", pch = 12)
plot(x3, y, main = "x3 vs. y", xlab = "x3", ylab = "y", col = "blue", pch = 12)

# standartization data set generation
set.seed(1989)
x1 <- rnorm(200) # independent variable 1
x2 <- rnorm(200) # independent variable 2
y <- 2*x1 + 3*x2 + rnorm(200, mean = 5, sd = 1) # d. variable

# scaling to standardize
x1_standardized <- scale(x1)
x2_standardized <- scale(x2)
y_standardized <- scale(y)

# checking the data
summary(x1_standardized)
summary(x2_standardized)
summary(y_standardized)

#splitting data for test and training

install.packages("caTools")
library(caTools)

# generating data set
set.seed(1989)
x1 <- rnorm(200) # independent variable 1
x2 <- rnorm(200) # independent variable 2
y <- 2*x1 + 3*x2 + rnorm(200, mean = 5, sd = 1) # d. variable

# splitting the generated data
split <- sample.split(y, SplitRatio = 0.6) # 60% training, 40% test
train_data <- subset(data.frame(x1, x2, y), split == TRUE)
test_data <- subset(data.frame(x1, x2, y), split == FALSE)

# size check
dim(train_data)
dim(test_data)


#Q2 

#importing data set from csv to df
library(readr)
starbucks_df <- read_csv("starbucks-menu-nutrition-drinks.csv") 
print(starbucks_df) 
#data includes missing values "-"
#col1 is data label (drink name)
#col2 is calories -> dependent variable because it depends on fat, carb etc.
#col3 is Fat - ind. var.
#col4 is Carb - ind. var.
#col5 is Fiber - ind. var.
#col6 is Protein - ind. var.
#col7 is Sodium - ind. var.
starbucks_df<- na.omit(starbucks_df) #na values ommited from df
#rows matching "-" filtered
starbucks_df<-starbucks_df[!starbucks_df$Calories == "-", ] 
print(starbucks_df) #check
#sufficent data points so not replaced with mean etc.
starbucks_df$Calories <- as.numeric(as.character(starbucks_df$Calories))  # Convert one variable to numeric
starbucks_df$`Carb. (g)` <- as.numeric(as.character(starbucks_df$`Carb. (g)`))  # Convert one variable to numeric
starbucks_df$`Fat (g)` <- as.numeric(as.character(starbucks_df$`Fat (g)`))  # Convert one variable to numeric
starbucks_df$`Fiber (g)` <- as.numeric(as.character(starbucks_df$`Fiber (g)`))  # Convert one variable to numeric
starbucks_df$Protein <- as.numeric(as.character(starbucks_df$Protein))  # Convert one variable to numeric
starbucks_df$Sodium <- as.numeric(as.character(starbucks_df$Sodium))  # Convert one variable to numeric
print(starbucks_df) #check

#outlier checks
boxplot(starbucks_df$Calories)

z_scores_sb <- scale(starbucks_df$Calories)
outliers_sb <- abs(z_scores_sb) > 3
Q1_sb <- quantile(starbucks_df$Calories, 0.25)
Q3_sb <- quantile(starbucks_df$Calories, 0.75)
IQR_sb <- Q3_sb - Q1_sb
lower_bound_sb <- Q1_sb - 1.5 * IQR_sb
upper_bound_sb <- Q3_sb + 1.5 * IQR_sb
outliers_sb_2 <- starbucks_df$Calories < lower_bound_sb | starbucks_df$Calories > upper_bound_sb

#standardization
starbucks_df_st <- starbucks_df
starbucks_df_st$Calories <- scale(starbucks_df$Calories)
starbucks_df_st$`Carb. (g)` <- scale(starbucks_df$`Carb. (g)`)
starbucks_df_st$`Fat (g)` <- scale(starbucks_df$`Fat (g)`)
starbucks_df_st$Protein <- scale(starbucks_df$Protein)
starbucks_df_st$Sodium <- scale(starbucks_df$Sodium)

#correlations
correlation_carb <- cor(starbucks_df_st$Calories, starbucks_df_st$`Carb. (g)`)
correlation_fat <- cor(starbucks_df_st$Calories, starbucks_df_st$`Fat (g)`)
correlation_protein <- cor(starbucks_df_st$Calories, starbucks_df_st$Protein)
correlation_sodium <- cor(starbucks_df_st$Calories, starbucks_df_st$Sodium)


# i.v graphs
par(mfrow=c(1,4)) # Grafiklerin yan yana yerleştirilmesi için ayar
plot(starbucks_df$`Carb. (g)`, starbucks_df$Calories, main = "carb vs.calories ", xlab = "carb", ylab = "calories", col = "blue", pch = 16)
plot(starbucks_df$`Fat (g)`, starbucks_df$Calories, main = "fat vs. calories", xlab = "fat", ylab = "calories", col = "red", pch = 16)
plot(starbucks_df$Protein , starbucks_df$Calories, main = "protein vs. calories", xlab = "protein", ylab = "calories", col = "green", pch = 16)
plot(starbucks_df$Sodium , starbucks_df$Calories, main = "sodium vs. calories", xlab = "sodium", ylab = "calories", col = "yellow", pch = 16)
#most linear relationship seems to be between carbs and calories
#we know that 1g fat = 10 kcal vs 1g carb = 4 kcal so data set seems to be skewed towards carb-rich drinks







