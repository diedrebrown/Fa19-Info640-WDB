#Pratt Info 640 Fall 2019
#Data Cleaning R Lab
#Diedre Brown; dbrow207@pratt.edu

install.packages("lubridate")
install.packages("dplyr")

#call libraries
library(tidyverse)
library(lubridate)
library(dplyr)

#load dataset mealsplan.csv, skip the first 3 rows, specify that the header is the first row, tell R not to check if the header names are properly formatted
meals<-read.csv("Desktop/Pratt/Fall 2019/DataAnalysis_Info640_McSweeney/info640-Lab Info and DataSets/DataSets/mealplan.csv", skip=3, header = TRUE, check.names = FALSE)

#verify that meals.csv is being read as a dataframe
class(meals)
#get size of dataset meals
dim(meals)
#get variable names
names(meals) 
#get overview of the dataset
str(meals)
#get a more detailed overview of the dataset--no dots and commons; contains more information on columns
glimpse(meals)
#get a statistical summary of the data
summary(meals)
#check out the beginning of the dataset--the first six rows
head(meals)
#check out the end of the dataset--the last six rows
tail(meals)

#articulate the problems--data cleaning
#pivot the columns with data in the headers
meals1<-gather(meals,date,value,-Meal,-Location)
head(meals1)

#break the entree/price column
meals1<-separate(meals1, value,c("entree", "price"),sep=',')
str(meals1)

#fix the dates
meals1$date<-mdy(meals1$date)
head(meals1)

#remove dollar sign
meals1$price<-gsub("\\$","",meals1$price)
head(meals1)

#force the prices as numeric
meals1$price<-as.numeric(meals1$price)

#histogram to check values
hist(meals1$price)

#boxplot to check values
boxplot(meals1$price)

#examination of distribution of values
unique(meals1$price)
meals1$price[meals1$price==400]<-4.00
hist(meals1$price)
summary(meals1)

#remove whitespaces from our character columns
meals1$Meal <-str_trim(meals1$Meal)
meals1$entree <-str_trim(meals1$entree)
meals1$Location <-str_trim(meals1$Location)

#replace empty space in column with NA
meals1$entree[meals1$entree==""] <- "NA"

#make character columns
meals1$Meal <- as.character(meals1$Meal)
meals1$Location <- as.character(meals1$Location)
head(meals1)

#make copy of dataframe
meals2<-meals1[,]
head(meals2)

#more standardization
names(meals2)<- tolower(names(meals2))
head(meals2)

#lower the entree descriptions
meals2$entree <-tolower(meals2$entree)

#mean & median of prices of a meal in the cafeteria
mean(meals2$price,na.rm = TRUE) #This excludes the NA's
#mean(meals2$price,na.rm = FALSE) This treats the NA's as 0

#use the max price for an entree to replace NAs to figure out cost of eat at cafeteria for one week
#find the max price
maxprice<-max(meals2$price,na.rm = TRUE)
print(maxprice)

#new dataframe with max price filled in
meals2$price_imputed<-meals2$price
meals2$price_imputed[is.na(meals2$price_imputed)]<-maxprice

#add up the price column
sum(meals2$price_imputed)
