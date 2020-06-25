dat <- read.csv("femaleMiceWeights.csv")
head(dat) 
View(dat)
library(dplyr)
control <- filter(dat,Diet=="chow") %>% select(Bodyweight) %>% unlist
treatment <- filter(dat,Diet=="hf") %>% select(Bodyweight) %>% unlist
print( mean(treatment) )
print( mean(control) )
obsdiff <- mean(treatment) - mean(control)
print(obsdiff)

#random variable 
population <- read.csv("femaleControlsPopulation.csv")
population <- unlist(population)

control <- sample(population,12)
mean(control)

control <- sample(population,12)
mean(control)

##average values are changeing

#NULL HYPOTHESIS

##12 control mice
control <- sample(population,12)
##another 12 control mice that we act as if they were not
treatment <- sample(population,12)
print(mean(treatment) - mean(control))

#creating a null distribution to see a bigger picture
n <- 10000
null <- vector("numeric",n)
for (i in 1:n) {
  control <- sample(population,12)
  treatment <- sample(population,12)
  null[i] <- mean(treatment) - mean(control)
}

p_value=mean(null >= obsdiff)
