#Diedre Brown; dbrow207@pratt.edu
#Info 640 | Data Analysis | Fall 2019
#Lab 10 Machine Learning

####PACKAGES & LIBRARIES####
install.packages("factoextra")
install.packages("rpart") #decision trees
install.packages("rpart.plot") #visualize decision trees
install.packages("rattle")
install.packages("RCcolorBrewer") #nice colors

library(tidyverse)
library(dplyr)
library(rpart)
library(rpart.plot)
library(rattle)
library(class)
library(factoextra)

####REGRESSION####
###Basic Linear Regression###
data("txhousing")
?txhousing
summary(txhousing)
head(txhousing)

#build a model representing median sale price as a function of the number of listings
#outcome variable (y) gets passed first and then the predictor variable (x) second
lm_housing <- lm(median ~ listings, data = txhousing)

#how much would the seller be about to get if there are 800 houses for sale?
unseen <- data.frame(listings = 800)
predict(lm_housing, unseen) #127009
summary(lm_housing) #R-squared value = 0.06

#prediction with another variable
tail(txhousing)
lm_housing1 <- lm(median ~ year, data = txhousing)
summary(lm_housing1) #r-squared = 0.249

lm_housing2 <- lm(median ~ volume, data = txhousing)
summary(lm_housing2) #r-squared = 0.1667

###Multiple Regression###
#which variables are most predictive of the sale price? let's train model on all of our numerical variables.
street<-read_csv("Desktop/Pratt/Fall2019/DataAnalysis_Info640_McSweeney/INFO640-WDB/info640-Lab Info and DataSets/DataSets/streeteasy.csv")
summary(street)

street_select6 <- street %>% 
  select(rent, bedrooms,bathrooms, size_sqft, min_to_subway,floor,building_age_yrs)

lm_street_select6 <- lm(rent ~ ., data=street_select6)
summary(lm_street_select6) #adjusted r-squared = 0.7293 use the adjusted r-squared when there are more variables (more complexity) in the model

#sq footage model
lm_street_size <- lm(rent ~ size_sqft, data=street_select6)
summary(lm_street_size) #mrsq = 0.6541; arsq = 0.6541

street_select16 <- street[,1:17]
#inspect data
str(street_select16)

lm_street_select16 <- lm(rent ~ ., data = street_select16)
summary(lm_street_select16) #arsq = 0.7329 better predictor and since i'm throwing out building_id, i think the best predictor would be if it has a patio

####CLASSIFICATION####
###Decision Trees###
#set seed for classifer to refer to. set.seed() is an rpart function
set.seed(1234)

#Titanic Dataset https://www.kaggle.com/c/titanic/data
train <- read.csv("Desktop/Pratt/Fall2019/DataAnalysis_Info640_McSweeney/INFO640-WDB/info640-Lab Info and DataSets/DataSets/titanic/train.csv")
test <- read.csv("Desktop/Pratt/Fall2019/DataAnalysis_Info640_McSweeney/INFO640-WDB/info640-Lab Info and DataSets/DataSets/titanic/test.csv")

#inspect sets
str(train)
str(test)

#narrow variables
train <- train %>%
  select(Pclass, Sex, Age, Survived)

test <- test %>%
  select(Pclass, Sex, Age, Survived)

#build a decision tree model to predict who survived.
tree <- rpart(Survived ~ Pclass + Sex + Age , train, method = "class")
fancyRpartPlot(tree)
#use predict function to create a table of predictions from test set
pred<- predict(tree, test, type = "class")
#use a confusion matrix (a table of real and predicted outcomes) to understand outcome of predictions
conf<- table(test$Survived, pred)
conf

#calculate the accuracy, precision, and recall
#Assign TP, FN, FP and TN using conf
TP <- conf[1,1] #this will be 212
FN <- conf[1, 2] # this will be 78
FP <- conf[2,1] # fill in
TN <- conf[2,2] # fill in
#calculate and print the accuracy: acc
acc <- sum(TP, TN)/sum(TP, TN, FP, FN)
acc #0.9688995
#calculate and print out the precision: prec
prec <- TP/sum(TP,FP)
prec #0.9809886
#calculate and print out the recall: rec
rec <- TP/sum(TP,FN)
rec #0.9699248
#shortcut to calculate accuracy
acc<-sum(diag(conf)/sum(conf))
acc #0.9688995

###OVERFITTING### - a really specific model. It works for the instances it was trained on but may not generalize to a larger, noisier environment.
#This is a common problem in machine learning, and results in highly interpretable but unreliable datasets.
#example:
overfit_tree <- rpart(Survived ~ Pclass + Sex + Age , train, method = "class", control = rpart.control(cp=0.00001))
fancyRpartPlot(overfit_tree)
#this model can be pruned to be more general with the prune function
pruned<- prune(tree, cp=.01)
fancyRpartPlot(pruned)

###k Nearest Neighbors kNN### - classification algorithms
#make vectors (lists) of just the outcome variables for both train and test sets; drop NA values
train <- drop_na(train)
test <- drop_na(test)
train_labels <- train$Survived
test_labels <- test$Survived

#drop the Survived column
knn_train <- train
knn_test <- test

knn_train$Sex <- as.factor(knn_train$Sex)
knn_test$Sex <- as.factor(knn_test$Sex)

knn_train$Sex <- as.factor(gsub("male", "1", knn_train$Sex))
knn_train$Sex <- as.factor(gsub("female", "0", knn_train$Sex))

knn_test$Sex <- as.factor(gsub("male", "1", knn_test$Sex))
knn_test$Sex <- as.factor(gsub("female", "0", knn_test$Sex))

knn_train$Sex <- as.numeric(knn_train$Sex)
knn_test$Sex <- as.numeric(knn_test$Sex)

knn_train$Survived <- NULL
knn_test$Survived <- NULL

#normalize Pclass so that all variables can be compared on the same scale
min_class <- min(knn_train$Pclass)
max_class <- max(knn_train$Pclass)
knn_train$Pclass <- (knn_train$Pclass - min_class) / (max_class - min_class)
knn_test$Pclass <- (knn_test$Pclass - min_class) / (max_class - min_class)

#normalize Age so that all variables can be compared on the same scale
min_age <- min(knn_train$Age)
max_age <- max(knn_train$Age)
knn_train$Age <- (knn_train$Age - min_age) / (max_age - min_age)
knn_test$Age <- (knn_test$Age - min_age)/(max_age - min_age)

#make predictions
k_pred <- knn(train=knn_train, test=knn_test, cl=train_labels, k=5)
k_pred
#make confusion table
conft <- table(test_labels, k_pred)
conft

my_titanic <- data.frame("Age" = .6, "Pclass" = .5, "Sex" = 0)
new_k_pred <- knn(train=knn_train, test=my_titanic, cl=train_labels, k=5)
new_k_pred

####CLUSTERING####
###Basic Clustering - Unsupervised###
#kmeans clustering on the iris dataset
#get a domain to know what the three groups are:
set.seed(1234)
data("iris")
#select dataset
my_iris <- iris[,1:4]
species <- iris$Species
#get clusters and ask r to restart 10 to get the average answer from all 10 trials
kmeans_iris <- kmeans(my_iris, centers = 3, nstart = 10)
kmeans_iris
#tabulate a confusion matrix
table(species, kmeans_iris$cluster)
#plot result of kmeans clustering to illustrate how the classifer performed
plot(Petal.Length ~ Petal.Width, data = my_iris, col = kmeans_iris$cluster)
#visualize with the factoextra package
fviz_cluster(kmeans_iris, data = my_iris)

###Clustering Unsupervised Unknown Clusters###
#If we didn't know how many clusters to ask for, we could try a lot by iterating through a for loop. 
#This look will allow us to try out values of k from 1 to 15. It will add each result in a list. 
#Cleverly, the number of k is the position in the list, so we can just add to our empty list. 
#We want to save the value of the Within Sum of Squares (wss) as a proportion of the Total Sum of Squares. We are looking for a value around .2

# Initialize total within sum of squares error: wss
wss_tot <- 0
for (i in 1:15) {
  km_out <- kmeans(my_iris, centers = i, nstart = 10)
  # Save total within sum of squares to wss variable
  wss_tot[i] <- km_out$tot.withinss/km_out$totss
}
#plot to find the optimal k. plot wss_tot ratio and look for the elbow (optimum number of clusters)
plot(1:15, wss_tot, type = "b",
     xlab = "Number of Clusters",
     ylab = "WSS/TSS")

#use the US Arrest dataset to:
data("USArrests")
str(USArrests)
#1. loop through the numbers 1:15 to get the wss/tot for each data point.
wss_tot <- 0
for (i in 1:15) {
  km_out <- kmeans(USArrests, centers = i, nstart = 10)
  # Save total within sum of squares to wss variable
  wss_tot[i] <- km_out$tot.withinss/km_out$totss
}
#2. Identify the best k for this data.
plot(1:15, wss_tot, type = "b",
     xlab = "Number of Clusters",
     ylab = "WSS/TSS")
#3. Use that k to build a new kmeans cluster.
kmeans_arrests <- kmeans(USArrests, centers= 2, nstart=10)
kmeans_arrests
#4. Plot the result.
plot(Murder ~ Assault, data = USArrests, col = kmeans_arrests$cluster)
fviz_cluster(kmeans_arrests, data = USArrests)
