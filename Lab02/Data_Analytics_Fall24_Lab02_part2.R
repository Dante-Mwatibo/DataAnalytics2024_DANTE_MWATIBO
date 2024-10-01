####### Data Analytics Fall 2024 Lab 02 port 2 ######

library(ggplot2)
library(class)
## Call the NaiveBayes Classifier Package e1071, which auto calls the Class package ##
library("e1071")

#Train classifier
classifier<-naiveBayes(iris[,1:4], iris[,5])

# evaluate classification
table(predict(classifier, iris[,-5]), iris[,5], dnn=list('predicted','actual'))

# examine class means and standard deviations for petal length
classifier$tables$Petal.Length

# plot normal distributions at the means of the classes
# one class
plot(function(x) dnorm(x, 1.462, 0.1736640), 0, 8, col="red", main="Petal length distribution for the 3 different species")

# another class
curve(dnorm(x, 4.260, 0.4699110), add=TRUE, col="blue")

# the final class
curve(dnorm(x, 5.552, 0.5518947 ), add=TRUE, col = "green")


############################# EXCERSISE 1 #####################################

##         Repeat the naive bayes analysis using the abalone dataset         ##

# reading in abalone data
abalone <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data"), header = FALSE,
                    sep = ",")
# rename columns
colnames(abalone) <- c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_weight', 'viscera_wieght', 'shell_weight',
                       'rings' ) 

#Train classifier
classifier.a<-naiveBayes(abalone[,-9], abalone[,9])

# evaluate classification
table(predict(classifier.a, abalone[,-9]), abalone[,9], dnn=list('predicted','actual'))

# examine class means and standard deviations for the length
classifier.a$tables$length

# plot normal distributions at the means of 3 of the classes
# one class
plot(function(x) dnorm(x, classifier.a$tables$length[4,1], classifier.a$tables$length[4,2]), 0, 8, col="red", main="Length distribution for the ages of 4, 12, and 20")

# another class
curve(dnorm(x, classifier.a$tables$length[12,1], classifier.a$tables$length[12, 2]), add=TRUE, col="blue")

# the final class
curve(dnorm(x, classifier.a$tables$length[12,1], classifier.a$tables$length[20, 2]), add=TRUE, col = "green")



##      Try 3 different subsets of features not just all features at once       ##

## Feature subset 1:

#Train classifier
classifier.a.s1<-naiveBayes(abalone[, c(-9, -1, -3, -4)], abalone[,9])

# evaluate classification
table(predict(classifier.a.s1, abalone[,-9]), abalone[,9], dnn=list('predicted','actual'))

# examine class means and standard deviations for the length
classifier.a.s1$tables$length

# plot normal distributions at the means of 3 of the classes
# one class
plot(function(x) dnorm(x, classifier.a.s1$tables$length[4,1], classifier.a.s1$tables$length[4,2]), 0, 1, col="red", main="Length distribution for the ages of 4, 12, and 20")

# another class
curve(dnorm(x, classifier.a.s1$tables$length[12,1], classifier.a.s1$tables$length[12, 2]), add=TRUE, col="blue")

# the final class
curve(dnorm(x, classifier.a.s1$tables$length[12,1], classifier.a.s1$tables$length[20, 2]), add=TRUE, col = "green")

## Feature subset 2:

#Train classifier
classifier.a.s2<-naiveBayes(abalone[, c(-9, -8, -7, -6)], abalone[,9])

# evaluate classification
table(predict(classifier.a.s2, abalone[,-9]), abalone[,9], dnn=list('predicted','actual'))

# examine class means and standard deviations for the length
classifier.a.s2$tables$length

# plot normal distributions at the means of 3 of the classes
# one class
plot(function(x) dnorm(x, classifier.a.s2$tables$length[4,1], classifier.a.s2$tables$length[4,2]), 0, 1, col="red", main="Length distribution for the ages of 4, 12, and 20")

# another class
curve(dnorm(x, classifier.a.s2$tables$length[12,1], classifier.a.s2$tables$length[12, 2]), add=TRUE, col="blue")

# the final class
curve(dnorm(x, classifier.a.s2$tables$length[12,1], classifier.a.s2$tables$length[20, 2]), add=TRUE, col = "green")

## Feature subset 3:

#Train classifier
classifier.a.s3<-naiveBayes(abalone[, c(-9, -8, -7, -6)], abalone[,9])

# evaluate classification
table(predict(classifier.a.s3, abalone[,-9]), abalone[,9], dnn=list('predicted','actual'))

# examine class means and standard deviations for the length
classifier.a.s3$tables$length

# plot normal distributions at the means of 3 of the classes
# one class
plot(function(x) dnorm(x, classifier.a.s3$tables$length[4,1], classifier.a.s3$tables$length[4,2]), 0, 1, col="red", main="Length distribution for the ages of 4, 12, and 20")

# another class
curve(dnorm(x, classifier.a.s3$tables$length[12,1], classifier.a.s3$tables$length[12, 2]), add=TRUE, col="blue")

# the final class
curve(dnorm(x, classifier.a.s3$tables$length[12,1], classifier.a.s3$tables$length[20, 2]), add=TRUE, col = "green")

## Feature subset 3:

#Train classifier
classifier.a.s3<-naiveBayes(abalone[, c(-9, -5, -1, -8)], abalone[,9])

# evaluate classification
table(predict(classifier.a.s3, abalone[,-9]), abalone[,9], dnn=list('predicted','actual'))

# examine class means and standard deviations for the length
classifier.a.s3$tables$length

# plot normal distributions at the means of 3 of the classes
# one class
plot(function(x) dnorm(x, classifier.a.s3$tables$length[4,1], classifier.a.s3$tables$length[4,2]), 0, 1, col="red", main="Length distribution for the ages of 4, 12, and 20")

# another class
curve(dnorm(x, classifier.a.s3$tables$length[12,1], classifier.a.s3$tables$length[12, 2]), add=TRUE, col="blue")

# the final class
curve(dnorm(x, classifier.a.s3$tables$length[12,1], classifier.a.s3$tables$length[20, 2]), add=TRUE, col = "green")


##                Compare the models using contingency tables                 ##

# Subset 1
table(predict(classifier.a.s1, abalone[,-9]), abalone[,9], dnn=list('predicted','actual'))

# Subset 2
table(predict(classifier.a.s2, abalone[,-9]), abalone[,9], dnn=list('predicted','actual'))

# Subset 3
table(predict(classifier.a.s3, abalone[,-9]), abalone[,9], dnn=list('predicted','actual'))


##        Plot the distribution of classes along 3 different features        ##
# Length
plot(function(x) dnorm(x, classifier.a$tables$length[3,1], classifier.a$tables$length[3,2]), 0, 1, col="red", main="Length distribution throughout life")
curve(dnorm(x, classifier.a$tables$length[4,1], classifier.a$tables$length[4, 2]), add=TRUE, col="blue")
curve(dnorm(x, classifier.a$tables$length[5,1], classifier.a$tables$length[5, 2]), add=TRUE, col = "green")
curve(dnorm(x, classifier.a$tables$length[6,1], classifier.a$tables$length[6, 2]), add=TRUE, col="orange")
curve(dnorm(x, classifier.a$tables$length[8,1], classifier.a$tables$length[8, 2]), add=TRUE, col = "yellow")
curve(dnorm(x, classifier.a$tables$length[10,1], classifier.a$tables$length[10, 2]), add=TRUE, col="brown")
curve(dnorm(x, classifier.a$tables$length[12,1], classifier.a$tables$length[12, 2]), add=TRUE, col = "black")
curve(dnorm(x, classifier.a$tables$length[14,1], classifier.a$tables$length[14, 2]), add=TRUE, col="pink")
curve(dnorm(x, classifier.a$tables$length[16,1], classifier.a$tables$length[16, 2]), add=TRUE, col = "turquoise")
curve(dnorm(x, classifier.a$tables$length[18,1], classifier.a$tables$length[18, 2]), add=TRUE, col="magenta")
curve(dnorm(x, classifier.a$tables$length[20,1], classifier.a$tables$length[20, 2]), add=TRUE, col = "grey")
curve(dnorm(x, classifier.a$tables$length[22,1], classifier.a$tables$length[22, 2]), add=TRUE, col="purple")
curve(dnorm(x, classifier.a$tables$length[23,1], classifier.a$tables$length[23, 2]), add=TRUE, col = "navy")
curve(dnorm(x, classifier.a$tables$length[24,1], classifier.a$tables$length[24, 2]), add=TRUE, col="maroon")
curve(dnorm(x, classifier.a$tables$length[27,1], classifier.a$tables$length[27, 2]), add=TRUE, col = "tan")

# shucked weight
plot(function(x) dnorm(x, classifier.a$tables$shucked_weight[3,1], classifier.a$tables$shucked_weight[3,2]), 0, 1, col="red", main="Shucked Weight distribution throughout life")
curve(dnorm(x, classifier.a$tables$shucked_weight[4,1], classifier.a$tables$shucked_weight[4, 2]), add=TRUE, col="blue")
curve(dnorm(x, classifier.a$tables$shucked_weight[5,1], classifier.a$tables$shucked_weight[5, 2]), add=TRUE, col = "green")
curve(dnorm(x, classifier.a$tables$shucked_weight[6,1], classifier.a$tables$shucked_weight[6, 2]), add=TRUE, col="orange")
curve(dnorm(x, classifier.a$tables$shucked_weight[8,1], classifier.a$tables$shucked_weight[8, 2]), add=TRUE, col = "yellow")
curve(dnorm(x, classifier.a$tables$shucked_weight[10,1], classifier.a$tables$shucked_weight[10, 2]), add=TRUE, col="brown")
curve(dnorm(x, classifier.a$tables$shucked_weight[12,1], classifier.a$tables$shucked_weight[12, 2]), add=TRUE, col = "black")
curve(dnorm(x, classifier.a$tables$shucked_weight[14,1], classifier.a$tables$shucked_weight[14, 2]), add=TRUE, col="pink")
curve(dnorm(x, classifier.a$tables$shucked_weight[16,1], classifier.a$tables$shucked_weight[16, 2]), add=TRUE, col = "turquoise")
curve(dnorm(x, classifier.a$tables$shucked_weight[18,1], classifier.a$tables$shucked_weight[18, 2]), add=TRUE, col="magenta")
curve(dnorm(x, classifier.a$tables$shucked_weight[20,1], classifier.a$tables$shucked_weight[20, 2]), add=TRUE, col = "grey")
curve(dnorm(x, classifier.a$tables$shucked_weight[22,1], classifier.a$tables$shucked_weight[22, 2]), add=TRUE, col="purple")
curve(dnorm(x, classifier.a$tables$shucked_weight[23,1], classifier.a$tables$shucked_weight[23, 2]), add=TRUE, col = "navy")
curve(dnorm(x, classifier.a$tables$shucked_weight[24,1], classifier.a$tables$shucked_weight[24, 2]), add=TRUE, col="maroon")
curve(dnorm(x, classifier.a$tables$shucked_weight[27,1], classifier.a$tables$shucked_weight[27, 2]), add=TRUE, col = "tan")

# diameter
plot(function(x) dnorm(x, classifier.a$tables$diameter[3,1], classifier.a$tables$diameter[3,2]), 0, 1, col="red", main="diameter distribution throughout life")
curve(dnorm(x, classifier.a$tables$diameter[4,1], classifier.a$tables$diameter[4, 2]), add=TRUE, col="blue")
curve(dnorm(x, classifier.a$tables$diameter[5,1], classifier.a$tables$diameter[5, 2]), add=TRUE, col = "green")
curve(dnorm(x, classifier.a$tables$diameter[6,1], classifier.a$tables$diameter[6, 2]), add=TRUE, col="orange")
curve(dnorm(x, classifier.a$tables$diameter[8,1], classifier.a$tables$diameter[8, 2]), add=TRUE, col = "yellow")
curve(dnorm(x, classifier.a$tables$diameter[10,1], classifier.a$tables$diameter[10, 2]), add=TRUE, col="brown")
curve(dnorm(x, classifier.a$tables$diameter[12,1], classifier.a$tables$diameter[12, 2]), add=TRUE, col = "black")
curve(dnorm(x, classifier.a$tables$diameter[14,1], classifier.a$tables$diameter[14, 2]), add=TRUE, col="pink")
curve(dnorm(x, classifier.a$tables$diameter[16,1], classifier.a$tables$diameter[16, 2]), add=TRUE, col = "turquoise")
curve(dnorm(x, classifier.a$tables$diameter[18,1], classifier.a$tables$diameter[18, 2]), add=TRUE, col="magenta")
curve(dnorm(x, classifier.a$tables$diameter[20,1], classifier.a$tables$diameter[20, 2]), add=TRUE, col = "grey")
curve(dnorm(x, classifier.a$tables$diameter[22,1], classifier.a$tables$diameter[22, 2]), add=TRUE, col="purple")
curve(dnorm(x, classifier.a$tables$diameter[23,1], classifier.a$tables$diameter[23, 2]), add=TRUE, col = "navy")
curve(dnorm(x, classifier.a$tables$diameter[24,1], classifier.a$tables$diameter[24, 2]), add=TRUE, col="maroon")
curve(dnorm(x, classifier.a$tables$diameter[27,1], classifier.a$tables$diameter[27, 2]), add=TRUE, col = "tan")


############################# END EXCERSISE 1 ###################################

# read dataset
abalone <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data"), header = FALSE,
                    sep = ",")
# rename columns
colnames(abalone) <- c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_wieght', 'viscera_wieght', 'shell_weight',
                       'rings' )

# add new column abalone$age.group with 3 values based on the number of rings
abalone$age.group <- cut(abalone$rings, br=c(0,8,11,35), labels = c("young", 'adult', 'old'))
# drop the sex column (categorical variable)
abalone.norm <- abalone[,-1]
# optionally normalize
normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x))) }
abalone.norm[1:7] <- as.data.frame(lapply(abalone.norm[1:7], normalize))

# sample 2924 from 4177 (~70%)
s_abalone <- sample(4177,2924)
## create train & test sets based on sampled indexes
abalone.norm.train <-abalone.norm[s_abalone,]
abalone.norm.test <-abalone.norm[-s_abalone,]

sqrt(2924)
k = 55
# k = 80
# train model & predict
KNNpred <- knn(train = abalone.norm.train[1:7], test = abalone.norm.test[1:7], cl = abalone.norm.train$age.group, k = k)
# create contingency table/ confusion matrix
contingency.table <- table(KNNpred,abalone.norm.test$age.group)

contingency.matrix = as.matrix(contingency.table)
sum(diag(contingency.matrix))/length(abalone.norm.test$age.group)
accuracy <- c()
ks <- c(35,45,55,65,75,85,95,105)
for (k in ks) {
  KNNpred <- knn(train = abalone.norm.train[1:7], test = abalone.norm.test[1:7], cl = abalone.norm.train$age.group, k = k)
  cm = as.matrix(table(Actual=KNNpred, Predicted = abalone.norm.test$age.group, dnn=list('predicted','actual')))
  accuracy <- c(accuracy,sum(diag(cm))/length(abalone.norm.test$age.group))
}
plot(ks,accuracy,type = "b", ylim = c(0.64,0.67))


############################# EXCERSISE 2 #####################################
##               Repeat the kNN analysis using the iris dataset               ##
# optionally normalize
iris.norm <- iris
iris.norm[1:4] <- as.data.frame(lapply(iris.norm[1:4], normalize))

# sample 2924 from 4177 (~70%)
s_iris <- sample(nrow(iris.norm), 105)
## create train & test sets based on sampled indexes
iris.norm.train <-iris.norm[s_iris,]
iris.norm.test <-iris.norm[-s_iris,]

sqrt(105)
k = 10
# k = 80
# train model & predict
KNNpred.iris <- knn(train = iris.norm.train[,1:4], test = iris.norm.test[,1:4], cl = iris.norm.train$Species, k = k)
# create contingency table/ confusion matrix
contingency.table.iris <- table(KNNpred.iris,iris.norm.test$Species)

contingency.matrix.iris = as.matrix(contingency.table.iris)
sum(diag(contingency.matrix.iris))/length(iris.norm.test$Species)
accuracy.iris <- c()
ks <- c(35,45,55,65,75,85,95,105)
for (k in ks) {
  KNNpred.iris <- knn(train = iris.norm.train[1:4], test = iris.norm.test[1:4], cl = iris.norm.train$Species, k = k)
  cm.iris = as.matrix(table(Actual=KNNpred.iris, Predicted = iris.norm.test$Species, dnn=list('predicted','actual')))
  accuracy.iris <- c(accuracy.iris,sum(diag(cm.iris))/length(iris.norm.test$Species))
}
plot(ks,accuracy.iris,type = "b", ylim = c(0.23,0.89))


##                     Try 2 diferent subsets of features                     ##
## Subset 1
abalone.norm[1:7] <- as.data.frame(lapply(abalone.norm[1:7], normalize))

# sample 2924 from 4177 (~70%)
s_abalone <- sample(4177,2924)
## create train & test sets based on sampled indexes
abalone.norm.train <-abalone.norm[s_abalone,]
abalone.norm.test <-abalone.norm[-s_abalone,]

sqrt(2924)
k = 55
# k = 80
# train model & predict
KNNpred.s1 <- knn(train = abalone.norm.train[c(1, 3, 5, 7)], test = abalone.norm.test[c(1, 3, 5, 7)], cl = abalone.norm.train$age.group, k = k)
# create contingency table/ confusion matrix
contingency.table.s1 <- table(KNNpred.s1,abalone.norm.test$age.group)

contingency.matrix.s1 = as.matrix(contingency.table.s1)
sum(diag(contingency.matrix.s1))/length(abalone.norm.test$age.group)
accuracy.s1 <- c()
ks <- c(35,45,55,65,75,85,95,105)
for (k in ks) {
  KNNpred.s1 <- knn(train = abalone.norm.train[c(1, 3, 5, 7)], test = abalone.norm.test[c(1, 3, 5, 7)], cl = abalone.norm.train$age.group, k = k)
  cm.s1 = as.matrix(table(Actual=KNNpred.s1, Predicted = abalone.norm.test$age.group, dnn=list('predicted','actual')))
  accuracy.s1 <- c(accuracy.s1,sum(diag(cm.s1))/length(abalone.norm.test$age.group))
}
plot(ks,accuracy.s1,type = "b", ylim = c(0.655,0.67))

## Subset 2
abalone.norm[1:7] <- as.data.frame(lapply(abalone.norm[1:7], normalize))

# sample 2924 from 4177 (~70%)
s_abalone <- sample(4177,2924)
## create train & test sets based on sampled indexes
abalone.norm.train <-abalone.norm[s_abalone,]
abalone.norm.test <-abalone.norm[-s_abalone,]

sqrt(2924)
k = 55
# k = 80
# train model & predict
KNNpred.s2 <- knn(train = abalone.norm.train[c(2, 4, 6)], test = abalone.norm.test[c(2, 4, 6)], cl = abalone.norm.train$age.group, k = k)
# create contingency table/ confusion matrix
contingency.table.s2 <- table(KNNpred.s2,abalone.norm.test$age.group)

contingency.matrix.s2 = as.matrix(contingency.table.s2)
sum(diag(contingency.matrix.s2))/length(abalone.norm.test$age.group)
accuracy.s2 <- c()
ks <- c(35,45,55,65,75,85,95,105)
for (k in ks) {
  KNNpred.s2 <- knn(train = abalone.norm.train[c(2, 4, 6)], test = abalone.norm.test[c(2, 4, 6)], cl = abalone.norm.train$age.group, k = k)
  cm.s2 = as.matrix(table(Actual=KNNpred.s2, Predicted = abalone.norm.test$age.group, dnn=list('predicted','actual')))
  accuracy.s2 <- c(accuracy.s2,sum(diag(cm.s2))/length(abalone.norm.test$age.group))
}
plot(ks,accuracy.s2,type = "b", ylim = c(0.655,0.67))

##         Compare models using contingency tables and accuracy plots          ##
# Subset 1
table(knn(train = abalone.norm.train[c(2, 4, 6)], test = abalone.norm[c(2, 4, 6)], cl = abalone.norm.train$age.group, k = k)
, abalone.norm[,9], dnn=list('predicted','actual'))

# Subset 2
table(knn(train = abalone.norm.train[c(1, 3, 5, 7)], test = abalone.norm[c(1, 3, 5, 7)], cl = abalone.norm.train$age.group, k = k)
      , abalone.norm[,9], dnn=list('predicted','actual'))

############################# EXCERSISE 2 END ##################################
# Plot iris petal length vs. petal width, color by species
ggplot(iris, aes(x = Petal.Length, y = Petal.Width, colour = Species)) +
  geom_point()

# set seed for random number generator
set.seed(123)

# run k-means
iris.km <- kmeans(iris[,-5], centers = 3)
assigned.clusters <- as.factor(iris.km$cluster)
ggplot(iris, aes(x = Petal.Length, y = Petal.Width, colour = assigned.clusters)) +
  geom_point()

wss <- c()
ks <- c(2,3,4,5)
for (k in ks) {
  iris.km <- kmeans(iris[,-5], centers = k)
  wss <- c(wss,iris.km$tot.withinss)
}
plot(ks,wss,type = "b")

labeled.clusters <- as.character(assigned.clusters)
labeled.clusters[labeled.clusters==1] <- "setosa"
labeled.clusters[labeled.clusters==2] <- "versivolor"
labeled.clusters[labeled.clusters==3] <- "virginica"
table(labeled.clusters, iris[,5])

############################## EXCERSISE 3 #####################################
##            Run K Means Analysis using the abalone & iris datasets          ##
## iris dataset
# Plot iris petal length vs. petal width, color by species
ggplot(iris, aes(x = Petal.Length, y = Petal.Width, colour = Species)) +
  geom_point()

# set seed for random number generator
set.seed(123)

# run k-means
iris.km <- kmeans(iris[,-5], centers = 3)
assigned.clusters.iris <- as.factor(iris.km$cluster)
ggplot(iris, aes(x = Petal.Length, y = Petal.Width, colour = assigned.clusters)) +
  geom_point()

wss <- c()
ks <- c(2,3,4,5)
for (k in ks) {
  iris.km <- kmeans(iris[,-5], centers = k)
  wss <- c(wss,iris.km$tot.withinss)
}
plot(ks,wss,type = "b")

labeled.clusters.iris <- as.character(assigned.clusters.iris)
labeled.clusters.iris[labeled.clusters.iris==1] <- "setosa"
labeled.clusters.iris[labeled.clusters.iris==2] <- "versivolor"
labeled.clusters.iris[labeled.clusters.iris==3] <- "virginica"
table(labeled.clusters.iris, iris[,5])

## Abalone dataset
# Plot abalone length vs. width, color by age
abalone.norm <- abalone.norm[,-8]
ggplot(abalone.norm, aes(x = whole_weight, y = diameter, colour = age.group)) +
  geom_point()

# set seed for random number generator
set.seed(123)

# run k-means
abalone.km <- kmeans(abalone.norm[,-8], centers = 3)
assigned.clusters <- as.factor(abalone.km$cluster)
ggplot(abalone.norm, aes(x = whole_weight, y = diameter, colour = age.group)) +
  geom_point()

wss.a <- c()
ks <- c(2,3,4,5)
for (k in ks) {
  abalone.km <- kmeans(abalone.norm[,-8], centers = k)
  wss <- c(wss.a,abalone.km$tot.withinss)
}
plot(ks,wss.a,type = "b")

labeled.clusters <- as.character(assigned.clusters)
labeled.clusters[labeled.clusters==1] <- "adult"
labeled.clusters[labeled.clusters==2] <- "young"
labeled.clusters[labeled.clusters==3] <- "old"
table(labeled.clusters, abalone.norm[,8])

##                     Try different values of k for both                     ####
##                                     &                                      ##
##    Evaluate clustering using Plot the best clustering output for both      ##
## Iris dataset
# run k-means
iris.km <- kmeans(iris[,-5], centers = 4)
assigned.clusters.iris <- as.factor(iris.km$cluster)
ggplot(iris, aes(x = Petal.Length, y = Petal.Width, colour = assigned.clusters.iris)) +
  geom_point()

# run k-means
iris.km <- kmeans(iris[,-5], centers = 5)
assigned.clusters.iris <- as.factor(iris.km$cluster)
ggplot(iris, aes(x = Petal.Length, y = Petal.Width, colour = assigned.clusters.iris)) +
  geom_point()

## Abalone dataset
abalone.km <- kmeans(abalone.norm[,-8], centers = 4)
assigned.clusters.a <- as.factor(abalone.km$cluster)
ggplot(abalone.norm, aes(x = whole_weight, y = diameter, colour = assigned.clusters.a)) +
  geom_point()

abalone.km <- kmeans(abalone.norm[,-8], centers = 5)
assigned.clusters.a <- as.factor(abalone.km$cluster)
ggplot(abalone.norm, aes(x = whole_weight, y = diameter, colour = assigned.clusters.a)) +
  geom_point()
























