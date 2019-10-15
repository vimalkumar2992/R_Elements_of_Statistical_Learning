#######################################################
# Statistical Data Mining I - EAS 506
# Home work I
# Q 4
# Vimal Kumarasamy
# Created: 9/15/2019
# Modified: 9/15/2019
#######################################################

rm(list = ls())
setwd("C:/UB/Studies/Semester 1/Statistical Data Mining I/Homework 1")

#######################################################
# Loading the libraries
library(DAAG)
library(lattice)
library(MASS)
library(geneplotter)
library(ggplot2)
library(DT)

# checking the contents of MASS package
ls("package:MASS")

data(Boston)
?Boston
dim(Boston)

# Obs. - There are 506 datapoints and 14 variables in it 

# Quick saniity checks before making pairwise scatterplots
any(is.na(Boston))

###########################################################################
## 4 A - Pairwise scatterplot matrix
pairs(~.,data=Boston, 
   main="Boston Scatterplot Matrix")

## creating a serial number as a reference in the dataset
Boston$suburb_id <- seq.int(nrow(Boston))

# Findings

# crim and age has positive relationship.
# Inference: Higher the proportion of units built prior to 1940, higher the crime in the suburb

# crim and medv has negative relationship.
# Inference: Higher the median value of owner-occupied homes, lower the crime rate

# zn vs indus and zn vs nox - Positive relationship.
# Inference: Higher the nox - nitrogen oxide concentration, higher the proportion of land zoned for lots
# Similarly higher the nox, higher the proportion of non-retail business acres per town.

# Chas vs other variables
# Inference: Chas river doesn't seem to have any strong realtion with any variable
# There suburbs with Charles river seem to be similar to those without

# rm vs lstat and medv - negative and positive relationship respectively.
# As expected, rm - number of rooms increases with decrease in lstat - percentage of lower status of population in the suburb
# higher the number of the rooms in dwelling - residential setup, higher then median value of homes in the suburb.

# nox vs dis - negative relationship
# Inference: Higher the nitrous oxide concentration in the suburb, farther the boston eomployment centres

# lstat vs medv 
# Inference: Stating the obvious, higher goes the median value of the homes, percent lower status of the population


###########################################################################
## 4 B - Crime rate per capita in the suburb associations
# age - positive association
# Inference: Higher the proportion of the buildings built before 1940, higher the crime rate in the suburb

# dis - negative association
# Inference: Higher the crime rate per capita, lower the mean distance of the suburb from 5 boston employment centers

# lstat - positive association
# Higher the crime rates, higher the percentage of population with lower status

# medv - negative association
# Higher the crime rate in the suburb, lower the median price of the homes

###########################################################################
## 4 C - high crime rates? Tax rates? Pupil-teacher ratios?

boxplot(Boston$crim,xlab="crim rate per capita", horizontal = TRUE, Title="crim")
summary(Boston$crim)

# Looking at different percentile points 
quantile(Boston$crim,c(0,0.25,.5, .9,0.95,0.99,1))
#0%       25%       50%       90%       95%       99%      100% 
#0.006320  0.082045  0.256510 10.753000 15.789150 41.370330 88.976200 
Boston_crime_p99=subset(Boston,crim>quantile(Boston$crim,c(0.99)))
dim(Boston_crime_p99)

# The range of the crime rate per capita is between 0.006320 and 88.976200
# Median is at 0.256510 and 99th percentile is at 41.37
# There are 6 suburbs having crime rate per capita of more than 41.37 

boxplot(Boston$tax,xlab="tax per $10,000", horizontal = TRUE)
summary(Boston$tax)

# Looking at different percentile points 
quantile(Boston$tax,c(0,0.25,.5, .9,0.95,0.99,0.999,1))
#0%   25%   50%   90%   95%   99% 99.9%  100% 
#187   279   330   666   666   666   711   711 
Boston_tax_p99=subset(Boston,tax>quantile(Boston$tax,c(0.99)))
dim(Boston_tax_p99)

# The range of property tax per $10,000 is between $187 and $711
# Median is at $330 and 99th percentile is at $666
# There are 5 suburbs having property tax more than $666 

boxplot(Boston$ptratio,xlab="Pupil to teacher ratio", horizontal = TRUE)
summary(Boston$ptratio)

# Looking at different percentile points 
quantile(Boston$ptratio,c(0,0.25,.5, .9,0.95,0.99,0.999,1))
#0%   25%   50%   90%   95%   99% 99.9%  100% 
#12.60 17.40 19.05 20.90 21.00 21.20 22.00 22.00 
Boston_ptratio_p99=subset(Boston,ptratio>quantile(Boston$ptratio,c(0.99)))
dim(Boston_ptratio_p99)
There are 
# The range of pupil to teacher ratio is between 12.6 and 22.00
# Median is at 19.05 and 99th percentile is at 21.20
# There are 2 suburbs having pupil to teacher ratio higher than 21.20


 
###########################################################################
## 4 D - Suburs with avg. rooms per dwelling more than 7 / 8 
dim(subset(Boston,rm>7))[1]
# There are 64 suburbs having more than 7 average rooms per dwelling 

dim(subset(Boston,rm>8))[1]
# There are 13 suburbs having more than 8 average rooms per dwelling 

# Quick study of suburbs with rm > 8 

Boston$rm_ge_8 <- ifelse(Boston$rm  > 8,'>8','<=8')
# Crime rate 
boxplot(split(Boston$crim,Boston$rm_ge_8), ylab = "Crime rate per  capita", xlab="rooms per dwelling")
# The range of crime rates are far lesser among the suburbs where avg. rooms per dwelling is greater than 8 

# Age of the bulidings
boxplot(split(Boston$age,Boston$rm_ge_8), ylab = "Proportion of buildings built before 1940",xlab="rooms per dwelling")
# Suburbs with average rooms higher than 8, are not that different from the rest of the suburbs with respect to the proportion of the age of the buildings

# Median value of the houses
boxplot(split(Boston$medv,Boston$rm_ge_8), ylab = "Median value of the houses",xlab="rooms per dwelling")
# As expected, the suburbs with avg. rooms per dwelling higher than 8 have higher median value of houses




