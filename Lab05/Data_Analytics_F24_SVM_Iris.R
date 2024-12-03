###############################
### Support Vector Machines ###
###############################


library("caret")
library(e1071)

## take copy
dataset <- iris

# ## split train/test
train.indexes <- sample(150,0.7*150)

train <- dataset[train.indexes,]
test <- dataset[-train.indexes,]

## separate x (features) & y (class labels)
x <- dataset[,1:4] 
y <- dataset[,5]

## feature boxplots
boxplot(x, main="iris features")

## class label distributions
plot(y)

## feature-class plots
featurePlot(x=x, y=y, plot="ellipse")

featurePlot(x=x, y=y, plot="box")

scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)

ggplot(dataset, aes(x = Petal.Length, y = Petal.Width, colour = Species)) +
  geom_point()


## train SVM model - linear kernel
svm.mod0 <- svm(Species ~ ., data = train, kernel = 'linear')

svm.mod0

train.pred <- predict(svm.mod0, train)

cm = as.matrix(table(Actual = train$Species, Predicted = train.pred))

cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

recall = diag / rowsums 
precision = diag / colsums
f1 = 2 * precision * recall / (precision + recall) 

data.frame(precision, recall, f1)


## train SVM model - polynomial kernel
svm.mod1 <- svm(Species ~ ., data = train, kernel = 'polynomial')

svm.mod1

train.pred <- predict(svm.mod1, train)

cm = as.matrix(table(Actual = train$Species, Predicted = train.pred))

cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

recall = diag / rowsums 
precision = diag / colsums
f1 = 2 * precision * recall / (precision + recall) 

data.frame(precision, recall, f1)


## Tuned SVM - polynomial
tuned.svm <- tune.svm(Species~., data = train, kernel = 'polynomial',gamma = seq(1/2^nrow(iris),1, .01), cost = 2^seq(-6, 4, 2))
tuned.svm

svm.mod2 <- svm(Species ~ ., data = train, kernel = 'polynomial', gamma = 0.69, cost = .25)

svm.mod2

train.pred <- predict(svm.mod2, train)

cm = as.matrix(table(Actual = train$Species, Predicted = train.pred))

cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

recall = diag / rowsums 
precision = diag / colsums
f1 = 2 * precision * recall / (precision + recall) 

data.frame(precision, recall, f1)


### Test set prediction ###

## model 0
test.pred <- predict(svm.mod0, test)

cm = as.matrix(table(Actual = test$Species, Predicted = test.pred))

cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

recall = diag / rowsums 
precision = diag / colsums
f1 = 2 * precision * recall / (precision + recall) 

data.frame(precision, recall, f1)

## model 1
test.pred <- predict(svm.mod1, test)

cm = as.matrix(table(Actual = test$Species, Predicted = test.pred))

cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

recall = diag / rowsums 
precision = diag / colsums
f1 = 2 * precision * recall / (precision + recall) 

data.frame(precision, recall, f1)

## model 2
test.pred <- predict(svm.mod2, test)

cm = as.matrix(table(Actual = test$Species, Predicted = test.pred))

cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

recall = diag / rowsums 
precision = diag / colsums
f1 = 2 * precision * recall / (precision + recall) 

data.frame(precision, recall, f1)


##########################################