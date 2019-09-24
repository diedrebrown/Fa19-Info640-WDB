#Pratt Info 640 Fall 2019
#Diedre Brown; dbrow207@pratt.edu
#Descriptive Data Analysis Project - Due 24 Sept 2019

#General Notes:
#NYC Greenthumb Community Gardens (Source:https://data.cityofnewyork.us/Environment/NYC-Greenthumb-Community-Gardens/ajxm-kzmj)
#The NYC Greenthumb Community Gardens (NYCGCG) dataset was obtained from NYC Opendata.
#The NYCGCG was last updated by the Department of Parks and Recreation (DPR) on September 10, 2018.
#No information was provided as to why the data was collected, how the data was collected, what each record represents, how this data can be used, and any idiosyncrasies or limitations of the data that the user should be made aware of. 
#From NYC Parks/NYC Open Data:
#Established in 1978, NYC Parks GreenThumb is proud to be the nation's largest urban gardening program, assisting over 550 gardens and over 20,000 volunteer gardeners throughout New York City. 
#GreenThumb gardens create hubs of neighborhood pride and provide a myriad of environmental, economic and social benefits to the neighborhoods in which they thrive.
#GreenThumb's mission is to educate and support community gardens and urban farming across the five boroughs, while preserving open space. 
#By providing free garden materials, technical assistance, educational workshops, and seasonal programs, GreenThumb supports neighborhood volunteers who steward community gardens as active resources that strengthen communities.


#call libraries
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)

#import csv dataset
nyccomgard <-read.csv("NYC_Greenthumb_Community_Gardens.csv")

#Impressions of data:
class(nyccomgard)
str(nyccomgard) #NYCGCG is categorical data. The set contains 536 observations; 17 variables; 1099 NA's.
summary(nyccomgard) #This showed that the Brooklyn (Boro B) has the highest number of community gardens in NYC.
glimpse(nyccomgard)
head(nyccomgard)
tail(nyccomgard)
sum(is.na(nyccomgard)) #There are NA's everywhere. I am going to exclude the NA locations from any calculations because there is not enough documentation to postulate on how to compensate for the missing values.
#PROBLEMS:
#Though size of the garden is listed, there is no documentation as to what units this size is in (sf, acre, sm, etc.).
#Community Board values are a mix of alphanumeric information. As there is a column for Borough and the Census.Tract and NTA infomation, the alpha character of the Community Board values is redundant. I am not sure how to break an alphanumeric character when there is no separater.
#Though Latitude and Longitude are given, there is no documentation which states what coordinate system the data is referring to.

#PLAN:
#1. Manipulate data to remove variables not needed for this analysis (PropID, Council.District, Garden.Name, Address, Cross.Streets,BIN, BBL)
#2. Since there is a lot of missing information, remove NAs as much as possible.
#3. Visualize/Compare Average size of community gardens by borough.

#Total number of community gardens in NYC, gardens per boro, and percentages of gardens per boro
numNYCgard <- nyccomgard %>%
  filter(!is.na(Boro)) %>%
  group_by(Boro)%>%
  count()
numNYCgard
totalNYCgard <- sum(numNYCgard$n)
totalNYCgard
perborogard <- numNYCgard %>%
  mutate(perctbgard = ((n/536)*100) )
perborogard
ggplot(perborogard, aes(x=Boro, y=perctbgard)) +
  geom_bar(stat = 'identity',
           fill = "black",
           size = 0.5) +
  labs(x="Borough", y="Percentage of Gardens", title = "Percentage of Gardens Per Borough in NYC (Total Gardens in NYC = 536)")

#Data Cleaning and Distributions
#Remove columns not needed for this analysis
nycgardshort <- nyccomgard%>%
  select(-PropID, -Council.District, -Garden.Name, -Address, -Cross.Streets, -BIN, -BBL)%>%
  filter(Size != 'NA')%>%
  arrange(Size)
summary(nycgardshort)
glimpse(nycgardshort)
head(nycgardshort)

#Histogram of Average Size of Community Gardens by Boro
borosizegrouped <- nycgardshort%>% 
  group_by(Boro)%>%
  summarize(meanSize = mean(Size))
summary(borosizegrouped)
meanborogsize<-ggplot(borosizegrouped,aes(x=Boro, y=meanSize))+
  geom_bar(stat = "identity",
           fill = "black",
           size = 0.5)+
  labs(x="Borough", y="Mean Size of Garden", title = "Mean Size of Community Gardens by Borough") 
meanborogsize 
  
#Community Gardens Jurisdiction Per Borough
nycgardjur <- nycgardshort %>%
  filter(NeighborhoodName != "")%>%
  group_by(Boro, Jurisdiction)
summary(nycgardjur)
glimpse(nycgardjur)
head(nycgardjur)
#I wanted to visualize this but realized after distributing that there are more NAs and possible duplications, so I was unsure of what information is correct.
