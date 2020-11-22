# Multiple Linear Regression

# Importing the dataset
dataset = read.csv('train.csv')
test = read.csv('test.csv')

# Encoding categorical data
dataset$Sex = factor(dataset$Sex,
                       levels = c('male', 'female'),
                       labels = c(1, 2))

dataset$Embarked = factor(dataset$Embarked,
                     levels = c('S', 'C', 'Q'),
                     labels = c(1, 2, 3, NA))

test$Sex = factor(test$Sex,
                     levels = c('male', 'female'),
                     labels = c(1, 2))

test$Embarked = factor(test$Embarked,
                          levels = c('S', 'C', 'Q'),
                          labels = c(1, 2, 3, NA))

# Taking care of missing data
columns <- c("Pclass","Sex","Age","SibSp","Parch","Fare","Embarked")
for(i in columns) {
  print(dataset[[i]])
dataset[[i]] = ifelse(is.na(dataset[[i]]),
                     ave(dataset[[i]], FUN = function(x) mean(x, na.rm = TRUE)),
                     dataset[[i]])
}


columns <- c("Pclass","Sex","Age","SibSp","Parch","Fare","Embarked")
for(i in columns) {
  print(test[[i]])
  test[[i]] = ifelse(is.na(test[[i]]),
                        ave(test[[i]], FUN = function(x) mean(x, na.rm = TRUE)),
                     test[[i]])
}

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Survived, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)

# Fitting Multiple Linear Regression to the Training set
regressor = lm(formula = Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
               data = dataset)

summary(regressor)

regressor = lm(formula = Survived ~ Pclass + Sex + Age + SibSp + Embarked,
               data = dataset)

summary(regressor)


# Predicting the Test set results
y_pred <- predict(regressor, newdata = test)
Survived <- format(round(y_pred))
PassengerId <- test$PassengerId


results <- data.frame(PassengerId, Survived)

write.csv(results, file = "results_R.csv", row.names=FALSE)
