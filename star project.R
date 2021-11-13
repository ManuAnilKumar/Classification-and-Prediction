
# Loading Packages--------------------------------------------------------------

library(e1071)
library(caTools)
library(caret)
library(class)
library(rpart)

#-------------------------------------------------------------------------------
# Loading Dataset

data <- read.csv(file.choose(), stringsAsFactors = T)
View(data)
str(data)

colnames(data) <- c("Temperature", "Luminosity", "Radius", "Magnitude", "Type", "Color", "Class")

data$Type <- as.factor(data$Type)

str(data)
summary(data)

# normalizing-------------------------------------------------------------------

preproc <- preProcess(data[,c(1:4)], method=c("center","scale"))
norm <- predict(preproc, data)
str(norm)
summary(norm)

#-------------------------------------------------------------------------------
# Splitting data into train and test data

split <- sample.split(norm, SplitRatio = 0.65)
train <- subset(norm, split == "TRUE")
test <- subset(norm, split == "FALSE")


set.seed(12)  # Setting Seed

# Model Building----------------------------------------------------------------

# Naive bayes

model_nb <- naiveBayes(Type ~ ., data = train)

# Predicting on test data
pred_nb <- predict(model_nb, newdata = test)

# Confusion Matrix
cm_nb <- confusionMatrix(test$Type, pred_nb)
cm_nb

#-------------------------------------------------------------------------------
# KNN

model_knn <- knn(train = train[1:4], test = test[1:4], cl = train$Type, k = 3)

cm_knn <- confusionMatrix(test$Type, model_knn)
cm_knn


#-------------------------------------------------------------------------------
# Decision tree

model_tree <- rpart(Type~., train, method = "class")

pred_tree <- predict(model_tree, test, type = "class")

cm_tree <- confusionMatrix(test$Type, pred_tree)
cm_tree

#-------------------------------------------------------------------------------

# SVM

model_svm = svm(formula = Type ~ ., data = train, type = 'C-classification', kernel = 'linear')

# Predicting the Test set results
pred_svm = predict(model_svm, newdata = test[-5])

# Making the Confusion Matrix
cm_svm = confusionMatrix(test[, 5], pred_svm)
cm_svm





