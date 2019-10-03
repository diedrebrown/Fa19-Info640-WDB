#Pratt Info 640 Fall 2019
#Lab 4 - Data Visualization R Lab
#Diedre Brown; dbrow207@pratt.edu

install.packages("ggthemes")
library(ggthemes)
library(tidyverse)
library(dplyr)
library(lubridate)

#load and inspect dataset
?iris
data("iris")
summary(iris)
glimpse(iris)
head(iris)
sum(is.na(iris)) #null variables? 0


#Exploratory Analysis
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width)) + geom_point()
#jitter and add transparency to points
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width)) + geom_jitter(alpha =0.6)
#encode species variable with color and add title
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width)) + geom_jitter(alpha =0.6) + labs(title = "Sepal Length by Sepal Width in the Fisher-Anderson Iris Dataset")
#small multiples for dataset
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width)) + 
  geom_jitter(alpha =0.6) + 
  facet_grid(.~Species) +
  labs(title = "Sepal Length by Sepal Width (cm) in the Fisher-Anderson Iris Dataset")
#line of best fit - stat_smooth
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width)) + 
  geom_jitter(alpha =0.6) + 
  facet_grid(.~Species) +
  stat_smooth(method = "lm", se=FALSE, col="red") +
  labs(title = "Sepal Length by Sepal Width (cm) in the Fisher-Anderson Iris Dataset")

#Make an object (iris_plot) that contains the group of elements that will remain consistent
iris_plot <- ggplot(iris, aes(x = Sepal.Length, y=Sepal.Width, color=Species)) + 
  labs(title = "Sepal Length by Sepal Width (cm) in the Fisher-Anderson Iris Dataset")

iris_plot + geom_point() 
iris_plot + geom_jitter(alpha =0.6) 
iris_plot + geom_line()

posn <- position_jitter(width = 0.1)
iris_plot + geom_point(position = posn) 

#gives average value of Sepal Length & Sepal Width
iris_summary <- aggregate(iris[1:4], list(iris$Species), mean)
names(iris_summary)[1] <- "Species"
iris_summary

iris_plot+geom_point()+
  geom_point(data = iris_summary, shape=21, size=5, fill="#00000080") 

iris_plot+geom_point(position = posn, alpha = 0.6) +
  geom_point(data=iris_summary,shape=15, size=5) 

#line graph with intercept
iris_plot+geom_point(position = posn, alpha = 0.6) +
  geom_vline(data=iris_summary,aes(xintercept = Sepal.Length)) 

#horizontal lines--x and y intercepts
iris_plot+geom_point(position = posn, alpha = 0.6) +
  geom_vline(data=iris_summary,aes(xintercept = Sepal.Length)) +
  geom_hline(data=iris_summary,aes(yintercept = Sepal.Width))

#change the linetypes of the intercepts
iris_plot + geom_point(position = posn, alpha = 0.6) +
  geom_vline(data = iris_summary, linetype = 2, aes(xintercept = Sepal.Length)) +
  geom_hline(data = iris_summary, linetype = 3, aes(yintercept = Sepal.Width))


#Altering the Formatting 
#add coordinates, specify the scale, add facets, add line of best fit 
iris_plot +
  geom_point(position = posn, alpha = 0.5) +
  facet_grid(.~Species) +
  stat_smooth(method = "lm", se=FALSE, col = "red") +
  scale_y_continuous("Sepal Width (cm)", limits = c(1,5), expand = c(0,0)) +
  scale_x_continuous("Sepal Length (cm)", limits = c(4,8), breaks = seq(2,8,2), expand = c(0,0)) +
  coord_equal()
#modify lablels
iris_plot +
  geom_point(position = posn, alpha = 0.5) +
  facet_grid(.~Species) +
  stat_smooth(method = "lm", se=FALSE, col = "red") +
  scale_y_continuous("Sepal Width (cm)", limits = c(1,5), expand = c(0,0)) +
  scale_x_continuous("Sepal Length (cm)", limits = c(4,8), breaks = seq(2,8,2), expand = c(0,0)) +
  coord_equal() + 
  labs(x="Sepal Length", y="Sepal Width", col="Species")

#Themes!
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width)) +
  geom_jitter(alpha = 0.5) +
  facet_grid(.~Species) +
  stat_smooth(method = "lm", se=FALSE, col="red") +
  scale_y_continuous("Sepal Width (cm)", limits = c(1,5), expand = c(0,0)) +
  scale_x_continuous("Sepal Length (cm)", limits = c(4,8), breaks = seq(2,8,2), expand = c(0,0)) +
  coord_equal() + 
  labs(x="Sepal Length", y="Sepal Width", col="Species") +
  theme(panel.background = element_blank(),
        plot.background = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        strip.background = element_blank(),
        axis.text = element_text(colour = "black"),
        axis.ticks = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.text = element_blank(),
        panel.spacing = unit(1,"lines"))
#iris_theme object for theme
iris_theme <- theme(panel.background = element_blank(),
                    plot.background = element_blank(),
                    legend.background = element_blank(),
                    legend.key = element_blank(),
                    strip.background = element_blank(),
                    axis.text = element_text(colour = "black"),
                    axis.ticks = element_line(colour = "black"),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    axis.line = element_line(colour = "black"),
                    strip.text = element_blank(),
                    panel.spacing = unit(1,"lines"))
#plot with the iris_theme object
#this is a single graph 
iris_plot + 
  geom_point(position = posn) + 
  labs(x="Sepal Length (cm)", y="Sepal Width (cm)", col="Species") + 
  iris_theme
#this is a small multiple encoding species with color
iris_plot +
  geom_point(position = posn, alpha = 0.5) +
  facet_grid(.~Species) +
  stat_smooth(method = "lm", se=FALSE, col = "red") +
  scale_y_continuous("Sepal Width (cm)", limits = c(1,5), expand = c(0,0)) +
  scale_x_continuous("Sepal Length (cm)", limits = c(4,8), breaks = seq(2,8,2), expand = c(0,0)) +
  coord_equal() + 
  labs(x="Sepal Length", y="Sepal Width", col="Species") +
  iris_theme

??ggthemes

#iris_plot with a Tufte theme
iris_plot + geom_jitter() + theme_tufte()


#________________________________________________________________________
#CATEGORICAL CHICKENS
#Overview Dataset ChickWeight
??ChickWeight
#Dataset dealing with the body weights of four groups of chicks (measured every other day from birth to day 21) to compare different protein diets.
data("ChickWeight")
summary(ChickWeight) #R578;C4

#Base plot object--chicks
chicks<-ggplot(ChickWeight,aes(x=ChickWeight$weight))+ 
  labs(title = "Weight(gm) Versus Age of Chicks on Varying Protein Diets")

#DISTRIBUTION
#Histogram (because Chicken Weight is a QUANTATIVE variable) to see distribution of chicken weights
chicks + geom_histogram()
#calculate binwidth
diff(range(ChickWeight$weight))/30 #bin width 11.26667pt
diff(range(ChickWeight$weight))/40 #bin width 8.45pt
#histogram with a 40 binwidth
chicks + geom_histogram(binwidth = 8.45)

#WEIGHT AS A FUNCTION OF DIET TYPE
#Histogram of diet (Diet is the Categorical Variable)
ggplot(ChickWeight,aes(x=ChickWeight$weight,fill=factor(ChickWeight$Diet)))+
  geom_histogram(binwidth = 8.45) + 
  labs(title = "Weight(gm) Versus Age of Chicks on Varying Protein Diets")
#Breakdown of the fill-irrespective of how many chick are in each category-take the count for each item and divide the sum.
#The .. on either side of each metric tells R to do this on the fly for each unique item in the variable, in this case, Diet
ggplot(ChickWeight,aes(x=ChickWeight$weight,fill=factor(ChickWeight$Diet)))+
  geom_histogram(aes(y=..count../sum(..count..)),
                 binwidth = 8.45, position = "fill") + 
  labs(title = "Weight(gm) Versus Age of Chicks on Varying Protein Diets")

#WEIGHT VS AGE
#Time (age) in X Axis as it is an independent variable
ggplot(ChickWeight,aes(x=ChickWeight$Time, y=ChickWeight$weight))+
  geom_jitter(alpha = 0.6) + 
  labs(title = "Weight(gm) Versus Age of Chicks on Varying Protein Diets")
#Any trend in diet vs weight for older chicks? Check on age vs weight by diet
ggplot(ChickWeight,aes(x=ChickWeight$Time, y=ChickWeight$weight, color=ChickWeight$Diet))+
  geom_jitter(alpha = 0.6) + 
  stat_smooth(method = "lm", se=FALSE, col="grey") +
  labs(x="Age (in days)", y="Weight (gm)", col="Diet", title = "Weight(gm) Versus Age of Chicks on Varying Protein Diets")
#Color and Facet the information
ggplot(ChickWeight,aes(x=ChickWeight$Time, y=ChickWeight$weight, color=ChickWeight$Diet))+
  geom_jitter(alpha = 0.6) + 
  facet_grid(.~ChickWeight$Diet) +
  stat_smooth(method = "lm", se=FALSE, col="grey") +
  labs(x="Age (in days)", y="Weight (gm)", col="Diet", title = "Weight(gm) Versus Age of Chicks on Varying Protein Diets") +
  iris_theme
#________________________________________________________________________


#TIME SERIES VISUALIZATIONS IN GGPLOT
#Internet Usage
data("WWWusage")
??WWWusage
str(WWWusage) 

#Data is not a dataframe. Turn data into a matrix, then make it a dataframe
www<-data.frame(usage=as.matrix(WWWusage), use_time=time(WWWusage))

#Visualization
ggplot(www,aes(x=use_time, y=usage)) +
  geom_line() +
  labs(x="Time (min)", y="Number of Users", title = "Internet Usage Per Minute in 1998") +
  iris_theme

#Air Quality in NYC for 6 months in 1973
data("airquality")
??airquality
str(airquality)

#exploratory visualization
ggplot(airquality, aes(x=Day,y=Temp)) +
  geom_line()

#Transform the date into a time stamp using mdy()
airquality$new_date <- paste(airquality$Month, airquality$Day, "1973", sep = "-")
glimpse(airquality)
airquality$new_date <- mdy(airquality$new_date)
glimpse(airquality)

#Visualization with transformed date
ggplot(airquality,aes(x=new_date,y=Temp)) +
  geom_line()+
  labs(x="Date", y="Temperature (F)", title = "Maximum Daily Temperature for May 1-September 30, 1973 at La Guardia Airport, New York, NY") +
  iris_theme
