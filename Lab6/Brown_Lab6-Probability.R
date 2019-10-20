#Pratt Info 640 Fall 2019
#Lab 6-Probabilty R Lab
#Diedre Brown; dbrow207@pratt.edu

#binomial distribution - 1 coin flipped 1=heads, 0=tails
rbinom(1,1,.5)
#ans: 0, 1, 1, 1, 0
#binomial distribution - 10 coins flipped 1=heads, 0=tails
rbinom(10,1,.5)
d1<-rbinom(10,1,.5)
sum(d1) #sum=3 three times all coins came up heads
#flip one coin 10 times
rbinom(1,10,.5) #4 times this coin came up heads
#flip 10 coins 10 times each
rbinom(10,10, .5) #vector of the outputs [1] 4 4 3 4 3 7 8 4 6 3
#flip 10 coins 10 times each, probability of getting heads more likely (80%)
rbinom(10,10,.8) # [1]  8 10 10  9 10  7  6  8  8  9 Higher numbers because P more likely.
#flip 10 coins 10 times each, probability of getting heads less likely (20%)/irrelevant class/no
rbinom(10,10,.2) # [1] 1 2 0 2 1 2 2 1 1 3 Lower numbers because P less likely.

#visualize the distribution to prove that the liklihood of any given outcome is normally distributed
flips<-rbinom(100000, 10, .5)
hist(flips)
#liklihoos of getting exactly 5 heads
flips == 5
#mean of flips to get probability b/c 1==TRUE and 0==FALSE
mean(flips==5) #0.24636
#DBINOM() mathematical likilhood, assuming a normal distribution of getting getting exactly 5 heads; 
#does not need to know how many coins (from here to infinity)
dbinom(5,10,.5) #[1] 0.2460938 

#PBINOM() gives the probability rather than a vector. 
#5 coins with 10 flips each-equally likely
pbinom(5, 10, .5) #[1] 0.6230469
1-pbinom(5, 10, .5) #[1] 0.3769531

#heights of 10,000 american women
rnorm(10000,65,3.5)
heights <-rnorm(10000,65,3.5)
hist(heights)

#find out how likely it is to randomly select an american woman from the population who is over 5'10"
#define a funtion that takes any random observation and plots it along a normal distribution
#use integrate () to take the intergral of the fuction to calculate the area under the area under
#the normal curve from the point we defined as it approaches infinity.
f<-function(x){dnorm(x, mean = 65, sd=3.5)}
integrate(f,70,Inf)
#0.07656373 with absolute error < 2.2e-06 7.66% chance that a randomly selected woman would be over 5'10"

#PNORM() The function that will take the integral
pnorm(70,65,3.5) #[1] 0.9234363 The probability of randomly selecting a woman that is less than 70"H
1-pnorm(70,65,3.5) #[1] 0.07656373 The probability of randomly selecting a woman that is more than 70"H

