##Functions and loops

#Complete the R Programming Basics tutorial at https://rstudio.cloud/learn/primers/1.2 before the next meeting in two weeks. Watch the videos and do the exercises. Requires maybe 30-45 minutes

##Functions 

#Functions are a packaged set of instructions that convert inputs into outputs

#There are built-in functions (sum, mean, sd, colmeans, table, etc). Compiled list of useful functions

#User can also construct its own functions.


#Loops

##For, if, while

##Examples

##For

##Lets use the FARS dataset (Fatality Analysis Recording System) from 2018 and using a For loop
##let's calculate the mean age.
#It contains 33654 observations of 62 different features. 

setwd("/Users/bombus/Documents/jupyter/BMI_6106_2020/Week_2/")
FARS = read.csv("PERSON2.csv", header = T)

##how to calculate mean

mean_age <- NULL
total <- NULL
for(i in 1:length(FARS$AGE)){
  total <- sum(total, FARS$AGE[i])
}
mean_age <- total/length(FARS$AGE)
mean_age

#Apply Family
apply(FARS[27],2, mean)

mean(FARS$AGE)

##How about by state
FARS$STATE = as.factor(FARS$STATE)


tapply(FARS$AGE, FARS$STATE, mean)

library(tidyverse)
FARS %>% group_by(STATE) %>% 
  summarise(age= mean(AGE))

##Nested if loops
##Calculate mean age for people younger than 40
mean_age <- NULL
total <- NULL
for(i in 1:length(FARS$AGE)){
  if (FARS$AGE[i] < 40){
    total <- sum(total, FARS$AGE[i])
  }
}
mean_age <- total/length(FARS$AGE[FARS$AGE<40])
mean_age

#How can I do the same with the mean Function, tydiverse package??

####Calculate mean age for people younger than 40 and get gender
mean_age <- NULL
total <- NULL
gender <- vector()
age <- NULL
for(i in 1:length(FARS$AGE)){
  if (FARS$AGE[i] < 40){
    total <- sum(total, FARS$AGE[i])
    gender = c(gender,FARS$SEX[i])
    age = c(age,FARS$AGE[i])
    
  }
}

ageGen = as.data.frame(cbind(gender,age))
ageGen$gender = as.factor(ageGen$gender)
ggplot(ageGen, aes(x= gender,y=age))+geom_boxplot()

##Ifelse
urb_young = ifelse(FARS$RUR_URB==1 & FARS$AGE <=40, FARS$AGE,NA)
hist(urb_young)

##Functions
doubleVal = function(n){
  doub = 2 * n
  return(doub) ## or simply doub
}

doubleVal(10)
doubleVal(c(5,6,4,5,6))

# function example - get measures of central tendency
# and spread for a numeric vector x. The user has a
# choice of measures and whether the results are printed.

#from https://www.statmethods.net/management/userfunctions.html
mysummary <- function(x,npar=TRUE,print=TRUE) {
  if (!npar) {
    center <- mean(x); spread <- sd(x)
  } else {
    center <- median(x); spread <- mad(x)
  }
  if (print & !npar) {
    cat("Mean=", center, "\n", "SD=", spread, "\n")
  } else if (print & npar) {
    cat("Median=", center, "\n", "MAD=", spread, "\n")
  }
  result <- list(center=center,spread=spread)
  return(result)
}

# invoking the function
set.seed(1234)
x <- rpois(500, 4)
y <- mysummary(x)
Median= 4
MAD= 1.4826
# y$center is the median (4)
# y$spread is the median absolute deviation (1.4826)

y <- mysummary(x, npar=FALSE, print=FALSE)
# no output
# y$center is the mean (4.052)
# y$spread is the standard deviation (2.01927)

more on functions

https://psyr.org/functions.html

##dice function

roll = function(NumDice){
  x= sample(6,NumDice)
  return(x)
}
roll(2)


Uncertainty
noise in system 
and nature of finite size of datasets

B random box selection (event)
Omega or sample space has two events (r(red) or b(blue))

F fruit is also random with two events (a(apple) or o(orange))

parakket example

6/26 + 7/26 = 1/2

2/6 + 3/6 - 1/6 = 4/6




