install.packages("broom")
install.packages("GGally")
install.packages("tidyverse")

library(tidyverse)
library(lubridate)
library(broom) #broom helps clean things up and remerge dataframes
library(GGally) #GGally hsleps run multiple pair-wise correlations

####Trees Dataset Work####
?trees #the tree dataset

data("trees")
glimpse(trees)

#quick scatterplot of Tree Height by Girth
ggplot(trees, aes(x=trees$Girth, y=trees$Height)) +
  geom_point() +
  labs(title = "Tree Height by Girth")

#line of best fit stat_smooth(method="TYPE OF MODEL",se=FALSE) se is standard error
ggplot(trees, aes(x=trees$Girth, y=trees$Height)) +
  geom_point() +
  stat_smooth(method = "lm", se=FALSE) +
  labs(title = "Tree Height by Girth")

#the slope of the regression line is units agnostic, but to change the height to inches instead of feet and inches do this:
# trees$Height_inch <- trees$Height * 12

#linear model of tree data lm(y~x,data), where y is the dependent variable, x is the independent variable, and the dataset is called as data=
lm_trees <- lm(Height ~ Girth, data = trees)
lm_trees
#more information about the model
summary(lm_trees)
#call coefficients
coef(lm_trees)
#get a vector with all the fitted values (y'), for each data point, call for the fitted values. these tell us what the model predicted rather than what was measured.
fitted_trees <- fitted.values(lm_trees)
fitted_trees
#residuals - each fitted value returns a residual (the difference between the actual, measured value and the predicted (fitted) value). useful for identifying outliers and modeling the distribution of the noise.
residual_trees <- residuals(lm_trees)
residual_trees

#get one unified dataframe with our orig response and explanatory variables. Rejoin the lm calculations with the orig dataframe.
#Broom Package's Augment Command broom::augment
#rejoin calculations with the original data
lm_matrix_trees<-broom::augment(lm_trees)
head(lm_matrix_trees)

lm_matrix_trees %>%
  arrange(desc(.resid)) %>%
  head()

#see all the residuals at one, compart the absolute values, sort the absolute value of the residuals to id outliers
lm_matrix_trees$.resid_abs<-abs(lm_matrix_trees$.resid)

lm_matrix_trees %>%
  arrange(desc(.resid_abs)) %>%
head()

#see all the raw values at once - compare the absolute value of the residuals. Sort the absolute values of the residuals to identify the outliers - or the most extreme values.
lm_matrix_trees$.resid_abs <- abs(lm_matrix_trees$.resid)

lm_matrix_trees %>%
  arrange(desc(.resid_abs))%>%
  head()

#inspect the outlier 13.8 dia
trees %>%
  filter(Girth == 13.8)

#if we had a tree that was exactly 19in in dia it would be ~88in height
#make a new dataframe with just this one valu, call predict on it using the lm 
head(lm_trees)
new_trees<-data.frame("Girth" = 19.5)
predict(lm_trees, newdata=new_trees)

mytree <- broom::augment(lm_trees, newdata=new_trees)
mytree

#represent new predicted value on the same plot with other values
#color new value red and make it larger so it's easier to see.
ggplot(data=trees, aes(x=Girth, y=Height)) +
  geom_point() + geom_smooth(method = "lm") +
  geom_point(data = mytree, aes(y=.fitted), size=3, color="red") + 
  labs(title = "Tree Height by Girth")

#use the null model (every single tree is the same height) to find out how well our model performed
tree_null <- lm(Height ~1, data = trees)
#verify if null lm true
mean_height <- mean(trees$Height)

ggplot(data = trees, aes(x=Girth, y=Height)) +
  geom_point()+
  geom_hline(yintercept = mean_height) +
  labs(title = "Tree Height Null Model")

ggplot(data = trees, aes(x=Girth, y=Height)) +
  geom_point() +
  stat_smooth(method = "lm") +
  geom_hline(yintercept = mean_height) +
  labs(title = "Tree Height Null Model")

#assess error = Multiple R-squared
ggplot(data = trees, aes(x=Girth, y=Height)) +
  geom_point() +
  stat_smooth(method = "lm") +
  labs(title = "Tree Height Null Model")
summary(lm_trees)

#For EDA - Correlation grid to examine the varialbles we have reason to believe are either correlated with each other or are different measures of similar underlying factors
ggpairs(data=trees, columns=1:3)

####Longley Dataset Work####
data(longley)
?longley
#UNDERSTAND DATASET
glimpse(longley)
summary(longley)

#How do two different variables predict employment rates? 
#Which model is better able to predict the number of people employed?

#EDA - Correlation grid
ggpairs(data = longley, columns = 1:6) 
#This illustrated high correlation coeff's for:
#Year-GNP 0.995
#Year-Population 0.994
#GNP-GNP.deflator 0.992
#Year-GNP.deflator 0.991
#Population-GNP 0.991
#Populaiton-GNP.deflator 0.979

#QUICK VIEWS: YEAR-EMPLOYED; YEAR-POPULATION; YEAR-GNP
#UNDERSTAND YEAR-EMPLOYED; 
ggplot(longley, aes(x=longley$Year, y=longley$Employed)) +
  geom_point() +
  stat_smooth(method = "lm") +
  labs (title = "Employment by Year, 1947-1962")
#UNDERSTAND YEAR-POPULATION; 
ggplot(longley, aes(x=longley$Year, y=longley$Population)) +
  geom_point() +
  stat_smooth(method = "lm") +
  labs (title = "Population by Year, 1947-1962")
#UNDERSTAND YEAR-GNP; 
ggplot(longley, aes(x=longley$Year, y=longley$GNP)) +
  geom_point() +
  stat_smooth(method = "lm") +
  labs (title = "GNP by Year, 1947-1962")
#FROM GGPAIRS AND THESE SCATTERPLOTS WE SEE THAT OVER TIME EACH OF THESE VARIABLES INCREASED. 
#SO USING MODELS WE WILL EXPLORE THE RELATIONSHIPS OF:
#EMPLOYMENT-POPULATION
#EMPLOYMENT-GNP

#EMPLOYMENT-POPULATION
#UNDERSTAND EMPLOYED-POPULATION; 
ggplot(longley, aes(x=longley$Population, y=longley$Employed)) +
  geom_point() +
  stat_smooth(method = "lm") +
  labs (title = "Employment by Population, 1947-1962")
#linear model of longleydata lm(y~x,data), where y is the dependent variable, x is the independent variable, and the dataset is called as data=
lm_longleyempop <- lm(Employed ~ Population, data = longley)
lm_longleyempop
#more information about the model
summary(lm_longleyempop)
#call coefficients
coef(lm_longleyempop)
#get a vector with all the fitted values (y'), for each data point, call for the fitted values. these tell us what the model predicted rather than what was measured.
fitted_longleyempop <- fitted.values(lm_longleyempop)
fitted_longleyempop
#residuals - each fitted value returns a residual (the difference between the actual, measured value and the predicted (fitted) value). useful for identifying outliers and modeling the distribution of the noise.
residual_longleyempop <- residuals(lm_longleyempop)
residual_longleyempop
#get one unified dataframe with our orig response and explanatory variables. Rejoin the lm calculations with the orig dataframe.
#Broom Package's Augment Command broom::augment
#rejoin calculations with the original data
lm_matrix_lempop<-broom::augment(lm_longleyempop)
head(lm_matrix_lempop)
#resid abs values
lm_matrix_lempop$.resid_abs<-abs(lm_matrix_lempop$.resid)
lm_matrix_lempop %>%
  arrange(desc(.resid_abs))%>%
  head()
#null model plot
mean_employed<-mean(longley$Employed)
ggplot(data = longley, aes(x=longley$Population, y=longley$Employed))+
  geom_point() +
  geom_hline(yintercept = mean_employed) +
  labs(title = "Employment by Population (1947-1962) Null Model")
summary(lm_longleyempop) #MRS = 0.9224

#EMPLOYMENT-GNP
#UNDERSTAND EMPLOYED-GNP; 
ggplot(longley, aes(x=longley$GNP, y=longley$Employed)) +
  geom_point() +
  stat_smooth(method = "lm") +
  labs (title = "Employment by Gross National Product, 1947-1962")
#linear model of longleydata lm(y~x,data), where y is the dependent variable, x is the independent variable, and the dataset is called as data=
lm_longleyemgnp <- lm(Employed ~ GNP, data = longley)
lm_longleyemgnp
#more information about the model
summary(lm_longleyemgnp)
#call coefficients
coef(lm_longleyemgnp)
#get a vector with all the fitted values (y'), for each data point, call for the fitted values. these tell us what the model predicted rather than what was measured.
fitted_longleyemgnp <- fitted.values(lm_longleyemgnp)
fitted_longleyemgnp
#residuals - each fitted value returns a residual (the difference between the actual, measured value and the predicted (fitted) value). useful for identifying outliers and modeling the distribution of the noise.
residual_longleyemgnp <- residuals(lm_longleyemgnp)
residual_longleyemgnp
#get one unified dataframe with our orig response and explanatory variables. Rejoin the lm calculations with the orig dataframe.
#Broom Package's Augment Command broom::augment
#rejoin calculations with the original data
lm_matrix_lemgnp<-broom::augment(lm_longleyemgnp)
head(lm_matrix_lemgnp)
#resid abs values
lm_matrix_lemgnp$.resid_abs<-abs(lm_matrix_lemgnp$.resid)
lm_matrix_lemgnp %>%
  arrange(desc(.resid_abs))%>%
  head()
#null model plot
ggplot(data = longley, aes(x=longley$GNP, y=longley$Employed))+
  geom_point() +
  geom_hline(yintercept = mean_employed) +
  labs(title = "Employment by Gross National Product (1947-1962) Null Model")
summary(lm_longleyemgnp) #MRS = 0.967

#Based on comparison of the Multiple R-squared values 0.9224 (Employment by Population) and 0.967 (Employment by GNP), respectively, we can see that the GNP is the closest correlation to predict employment.
