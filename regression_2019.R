library(ggplot2)
library(caTools)
library(dplyr)
library(broom)
library(ggpubr)

# Importing the data set
data_2019 <- read.csv(file.choose())

# summarizing the data
summary(data_2019)
head(data_2019)
 
# plotting variable relations (2019)
plot(data_2019$health, data_2019$happiness_score, xlab='Health', 
     ylab='Happiness Score', main='Happiness Score by Health (2019)')

plot(data_2019$family, data_2019$happiness_score, xlab='Family', 
     ylab='Happiness Score', main='Happiness Score by Family (2019)')

plot(data_2019$gdp_per_capita, data_2019$happiness_score, xlab='GDP per capita',
     ylab='Happiness Score', main='Happiness Score by GDP per capita (2019)')

plot(factor(data_2019$continent), data_2019$happiness_score, xlab='Continent',
     ylab='Happiness Score', main='Happiness Score by Continent (2019)')


# splitting the data
split_2019 <- sample.split(data_2019$happiness_score, SplitRatio = 0.8)
training_set <- subset(data_2019, split_2019 == TRUE)
test_set <- subset(data_2019, split_2019 == FALSE)

# fitting the data into linear regression model
lm_2019 <- lm(happiness_score ~ gdp_per_capita, data=training_set)

coef(lm_2019)

summary(lm_2019)

# predicting test set results
ypred <- predict(lm_2019, newdata = test_set)

ypred

# linear regression graph (whole data set)
happiness_graph <- ggplot(data_2019, aes(x=gdp_per_capita, y=happiness_score)) + geom_point()

happiness_graph <- happiness_graph + geom_smooth(method="lm", col="black")

happiness_graph <- happiness_graph + stat_regline_equation()

happiness_graph <- happiness_graph + theme_bw() + labs(
  title="Happiness by GDP per capita (2019)",
  x = "GDP per capita",
  y = "Happiness Score")

happiness_graph

# linear regression graph (training data)
happiness_graph_train <- ggplot(training_set, aes(x=gdp_per_capita, y=happiness_score)) + geom_point()

happiness_graph_train <- happiness_graph_train + geom_smooth(method="lm", col="black")

happiness_graph_train <- happiness_graph_train + stat_regline_equation()

happiness_graph_train <- happiness_graph_train + theme_bw() + labs(
  title="Happiness by GDP per capita (2019) (Training data)",
  x = "GDP per capita",
  y = "Happiness Score") 

happiness_graph_train

# linear regression graph (test data)
happiness_graph_test <- ggplot(test_set, aes(x=gdp_per_capita, y=happiness_score)) + geom_point()

happiness_graph_test <- happiness_graph_test + geom_smooth(method="lm", col="black")

happiness_graph_test <- happiness_graph_test + stat_regline_equation()

happiness_graph_test <- happiness_graph_test + theme_bw() + labs(
  title="Happiness by GDP per capita (2019) (Test data)",
  x = "GDP per capita",
  y = "Happiness Score")

happiness_graph_test


# multiple linear regression
hist(data_2019$happiness_score)

multiple_lm_2019 <- lm(happiness_score ~ gdp_per_capita + health, data = training_set)

summary(multiple_lm_2019)