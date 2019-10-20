#Pratt Info 640 Fall 2019
#Lab 7-Confidence Interval R Lab
#Diedre Brown; dbrow207@pratt.edu

#install packages
install.packages("gmodels")

#call libraries
library(gmodels) #contains the CI function to calculate confidence
library(tidyverse)

#generate data: 10,000 people in the world; we want to know their body temperatures; generate a 
#normally distributed dataset with 10,000 values, mean=97.82, and standard deviation = 0.69
bodytemp <- rnorm(10000, mean = 97.82, sd=0.69)
glimpse (bodytemp)
hist(bodytemp)

#find the mean of a randomly selected sample
set.seed(1234)
bodysample <- sample(bodytemp, 10) #sample size 10
mean(bodysample) #[1] 97.74252 slightly lower than mean of population
#find the mean of a randomly selected sample
bodysample <- sample(bodytemp, 100) #sample size 100
mean(bodysample) #[1] 97.73896 slightly lower than mean of population
#find the mean of a randomly selected sample
bodysample <- sample(bodytemp, 1000) #sample size 1000 - the larger the sample size the mean will be closer to the overall pop
mean(bodysample) #[1] 97.83352 slightly higher than the mean of population

#use a for loop to visualize the distribution of these samples - how often would it be very high versus very low?
our_sample <- numeric(10000)
for(i in 1:10000){
  a_sample <- sample(bodytemp, 50)
  our_sample[i] <- mean(a_sample)
  }
hist(our_sample, breaks = 50)

#calculate the confidence interval manually using the point estimate (or the mean), and the standard deviation from the mean
temp_mean <- mean(bodytemp)
temp_stdev <- sd(bodytemp, na.rm = TRUE)
sample_size = length(bodytemp)
temp_mean #[1] 97.81927
#qnorm calculates teh difference fromthe mean assuming a normal distribution.
#with a large sample use the z-score, assuming a normal distribution. With a large sample, degrees of freedom doesn't matter too much.
#with a small sample use the t-test, which takes into account your degrees of freedome which the probability curve doesn't
#normal distr
error_n <- qnorm(0.975)*temp_stdev/sqrt(sample_size)
left_n <- temp_mean - error_n
right_n <- temp_mean + error_n
#t-test
error_t <- qt(0.975, df=sample_size-1)
left_t <- temp_mean - error_t
right_t <- temp_mean + error_t
#print normal dist and t-test, respectively
print(left_n) #[1] 97.8059
print(right_n) #[1] 97.83265
print(left_t) #[1] 95.85907
print(right_t) #[1] 99.77948

#use the confidence interval function in r (ci) to calculate for the generated data @ 95%
ci(bodytemp, confidence = 0.95) 
#Estimate   CI lower   CI upper Std. Error 
#97.819275  97.805895  97.832655   0.006826
#use r's t-test fuction (t.test) for one sample
t.test(bodytemp, mu=temp_mean, conf.level = 0.95)
#	One Sample t-test
#data:  bodytemp
#t = 0, df = 9999, p-value = 1
#alternative hypothesis: true mean is not equal to 97.81927
#95 percent confidence interval:
#  97.80589 97.83266
#sample estimates:
#  mean of x 
#97.81927

#confidence interval with Mackowiak dataset
#read in Mackowiak dataset
realtemps <- read.csv("Normtemp.csv", header = TRUE)
glimpse(realtemps)
#turn Gender from a string to a factor
realtemps$Gender <- as.factor(realtemps$Gender)
#histogram of body temps
summary(realtemps)
hist(realtemps$Body.Temp)
#find the confidence interval for 95% certainty that our body temps lie
body_mean = mean(realtemps$Body.Temp)
body_mean #[1] 98.24923
t.test(realtemps$Body.Temp, mu=body_mean, conf.level = 0.95)
#	One Sample t-test
#data:  realtemps$Body.Temp
#t = 0, df = 129, p-value = 1
#alternative hypothesis: true mean is not equal to 98.24923
#95 percent confidence interval:
#  98.12200 98.37646
#sample estimates:
#  mean of x 
#98.24923 




