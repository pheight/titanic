# Multiple Linear Regression

# Importing the dataset
dataset = read.csv('train.csv')

# Encoding categorical data
dataset$Sex = factor(dataset$Sex,
                       levels = c('male', 'female'),
                       labels = c(1, 2))

dataset$Embarked = factor(dataset$Embarked,
                     levels = c('S', 'C', 'Q'),
                     labels = c(1, 2, 3))


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
               data = training_set)

summary(regressor)


# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)