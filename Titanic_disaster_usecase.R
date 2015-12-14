# Ashish Kumar - 14 Dec 2015
#Developed for predicting survival of passengers on the "Titanic"!
#DataSet provided by kaggel!

# Set working directory and import datafiles
setwd("c:\Documents\R\Titanic")
train <- read.csv("train.csv")
test <- read.csv("test.csv")

#  load required packages for fancy decision tree plotting

library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# create the gender model
fit <- rpart(Survived ~ Sex, data=train, method="class")
fancyRpartPlot(fit)

# Build a greedy tree
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="class")
# Plot it with base-R
plot(fit)
text(fit)
# Apply fancyRpartPlot!
fancyRpartPlot(fit)
# grow the decision tree!
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train,
             method="class", control=rpart.control(minsplit=2, cp=0))
fancyRpartPlot(fit)

#  prediction 
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
# submission 
write.csv(submit, file = "Final.csv", row.names = FALSE)

#  trimming a decision tree
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train,
             method="class", control=rpart.control(minsplit=2, cp=0.005))
new.fit <- prp(fit,snip=TRUE)$obj
fancyRpartPlot(new.fit)
