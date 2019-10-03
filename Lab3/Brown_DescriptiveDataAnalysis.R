#Pratt Info 640 Fall 2019
#Descriptive Data Analysis
#Diedre Brown; dbrow207@pratt.edu

install.packages("gapminder")

#call libraries
library(tidyverse)
library(dplyr)
library(gapminder) #the dataset - country populaation data, life expectancy, GDP per capita since the 1950s

??gapminder #this will tell us about the data set in the help tab at bottom right corner of the screen. you need the two ?? to do this

#summary() returns the distribution of both categorical and numberical variables
summary(gapminder)
#glimpse() tells you how many observations there are, how many variables, the type of data it is (including the type of number), and the first few values in each variable.
glimpse(gapminder)
#look at how the data is structured
head(gapminder)
tail(gapminder)
sum(is.na(gapminder)) #this tells you how many na values are in a dataset

#Distribution and Visualizations
#5 functions from dplyr that operate on 3 categories [VARIABLES(columns); OBSERVATIONS(rows)]
#select() picks only certain columns
#filter() picks only certain rows that meet the criteria of a certain column
#arrange() sorts a TABLE based on a VARIABLE (moves the rows)
#mutate() GROUPS (pre-grouped elements)
#summarize()
gp_cnt_life <- select(gapminder, country, lifeExp)
head(gp_cnt_life)

gp_no_pop <- select(gapminder, -pop)
head(gp_no_pop)

#filter for a specific year or country; piping is done with this %>% 
gp_1957 <- gapminder %>% filter(year==1957)
glimpse(gp_1957)
head(gp_1957, n=10)

gp_us <- gapminder %>% filter(country == "United States")
head(gp_us, n=15)

#filter for 2 variables
gp_1957_Asia <- gapminder %>% filter(year==1957, continent=="Asia")
head(gp_1957_Asia,8)

#write a new CSV for filtered Gapminder Data
write.csv(gp_1957_Asia, 'gapminder1957Asia.csv') #this file is saved in your route directory

#sort the table by population column to ge the smallest pop/year/country combination at the top
gapminder %>% arrange(pop)
#sort for the largest countries
gapminder %>% arrange(desc(pop))

#look at only 1957, and sort by population
gapminder %>% filter(year==1957) %>% arrange(desc(gdpPercap))

#view pop in millions rather than as raw numbers--use mutate on the column by dividing by 1,000,000
gapminder %>% mutate(pop = pop/1000000)
#mutate can also be used to add variables. gdp is reported per capita, make just gdp by multiplying by pop
gapminder %>% mutate(gdp = gdpPercap*pop)

#find the top 5 highest gdp countries in 1957. Mutate gdpPercap into gdp, filter for 1957, and arrange the table by gdp in descending order.
gapminder %>%
  mutate(gdp = gdpPercap * pop) %>%
  filter(year==1957) %>%
  arrange(desc(gdp))

head(gapminder) 
#the previous piping isn't saved in gapminder so, you have to create a new variable to store the filtered table into
gap_gdp_1957 <- gapminder %>%
  mutate(gdp = gdpPercap * pop) %>%
  filter(year==1957) %>%
  arrange(desc(gdp))

#what is the mean life expectancy?
gapminder %>%
  summarize(meanLifeExp = mean(lifeExp))

#find mean life exp for 1957 - filter by year, and then summarize by the mean life expectancy
gapminder %>%
  filter(year==1957)%>%
    summarize(meanLifeExp = mean(lifeExp))
#find the mean life exp for 2007
gapminder %>%
  filter(year==2007)%>%
  summarize(meanLifeExp = mean(lifeExp))
#find the global mean life exp for 1957 and the total popuation of the world that year
gapminder %>%
  filter(year=='1957') %>% #NOT SURE WHY NOW THIS IS REQUIRING ''???
  summarize(meanLifeExp = mean(lifeExp), totalPop = sum(as.numeric(pop)))
#global change in life expectancy
gapminder %>%
  group_by(year) %>%
  summarize(meanLifeExp = mean(lifeExp), totalPop = sum(as.numeric(pop)))
#mean life expectancy & total population of every continent in 1957
gapminder %>%
  filter(year==1957) %>%
  group_by(continent) %>%
  summarize(meanLifeExp = mean(lifeExp), totalPop = sum(as.numeric(pop)))

#mean life expectancy and gdp of US in the year 2007
gapminder %>%
  filter(year == 2007, country == "United States") %>%
  summarize(meanLifeExp = mean(lifeExp))
gap_gdp_US_2007 <- gapminder %>%
  mutate(gdp = gdpPercap * pop) %>%
  filter(year==2007, country == "United States") %>%
  arrange(desc(gdp))
gap_gdp_US_2007

#Mean of the meanLifeExp and totalPop for every continent every year
gapminder %>%
  group_by(continent,year)%>%
  summarize(meanLifeExp = mean(lifeExp), totalPop = sum(as.numeric(pop)))

#mean gdp for every country every year
gapminder %>%
  group_by(country,year)%>%
  mutate(gdp = gdpPercap * pop)%>%
  summarize(meangdp = mean(gdp), totalPop = sum(as.numeric(pop)))

#passing transformed dataset into a new dataset
by_year <- gapminder %>%
  group_by(year)%>%
  summarize(totalPop = sum(as.numeric(pop)),meanLifeExp = mean(lifeExp))
head(by_year)
#scatter plot
ggplot(by_year, aes(x=year, y=totalPop)) + geom_point()
#make y-axis start at 0
ggplot(by_year, aes(x=year,y=totalPop)) + geom_point() + expand_limits(y=0)
#breakdown by continent
by_year_continent <- gapminder %>%
  group_by(year,continent) %>%
  summarize(totalPop = sum(as.numeric(pop)), meanLifeExp = mean(lifeExp))
head(by_year_continent)

ggplot(by_year_continent, aes(x=year, y=totalPop, color=continent))+geom_point()+expand_limits(y=0)

#visualize mean gdp per continent over time
#passing mean gdp by continent into a new dataset
by_year_gdp_continent <- gapminder %>%
  group_by(year, continent)%>%
  mutate(gdp = gdpPercap * pop)%>%
  summarize(totalPop = sum(as.numeric(pop)), meanGdp = mean(gdp))
head(by_year_gdp_continent)
#plot the mean gdp by continent over time
ggplot(by_year_gdp_continent, aes(x=year, y=meanGdp, color=continent))+geom_point()+expand_limits(y=0)

#DDA Project Example using the gapminder dataset
#1 The Summary
summary(gapminder)
#2 Min & Max country for 2007
gap2007 <- gapminder %>% filter(year=="2007")
gap2007[which.min(gap2007$lifeExp),]
gap2007[which.min(gap2007$pop),]
gap2007[which.min(gap2007$gdpPercap),]
gap2007[which.max(gap2007$lifeExp),]
gap2007[which.max(gap2007$pop),]
gap2007[which.max(gap2007$gdpPercap),]
#3 Population growth rate
start_year <- min(gapminder['year'])
end_year <- max(gapminder['year'])
start_pop <- min(gapminder['pop'])
end_pop <- max(gapminder['pop'])
pop_growth_rate <-(end_pop-start_pop)/(end_year-start_year)
pop_growth_rate
#4 Visualizations 
gap_grouped <-gapminder %>%
  group_by(continent, year)%>%
  summarize(meanlifeExp = mean(lifeExp),
            totalPop = sum(as.numeric(pop)),
            meanGdpPercap = mean(gdpPercap))
summary(gap_grouped)

ggplot(gap_grouped, aes(x=year, y=meanlifeExp, color=continent)) + geom_line()
ggplot(gap_grouped, aes(x=year, y=totalPop, color=continent)) + geom_line()
ggplot(gap_grouped, aes(x=year, y=meanGdpPercap, color=continent)) + geom_line()
#5
#method 2
gapminder%>%
  group_by(continent)%>%
  summarize(maxLifeExp = max(lifeExp),
            minLifeExp = min(lifeExp),
            meanLifeExp = mean(lifeExp))
gapminder%>%
  group_by(continent)%>%
  summarize(maxtotalPop = max(sum(as.numeric(pop))),
            mintotalPop = min(sum(as.numeric(pop))),
            meantotalPop = mean(as.numeric(pop)))
gapminder%>%
  group_by(continent)%>%
  summarize(maxGdpPercap = max(gdpPercap),
            minGdpPercap = min(gdpPercap),
            meanGdpPercap = mean(gdpPercap))
#method 3
my_continents <- c("Africa", "Americas", "Asia", "Europe", "Oceania")
sapply(my_continents, function(cont){gapminder%>% filter(continent==cont)%>% summary()})
summary(gapminder)





