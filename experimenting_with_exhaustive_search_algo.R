#######################################################
# Statistical Data Mining I - EAS 506
# Home work II
#
# Vimal Kumarasamy
# Created: 9/30/2019
# Modified: 10/14/2019
#######################################################

rm(list = ls())
setwd("C:/UB/Studies/Semester 1/Statistical Data Mining I/Homework 2")

#######################################################
# Loading the libraries
library(DAAG)
library(lattice)
library(MASS)
library(geneplotter)
library(ggplot2)
library(DT)
library(plyr) 
library(dplyr)
library(sqldf)
library(DT)
library(corrplot)
library(glmnet)

########################################################

# Creating a random dataset with 1000 observations and 20 features

NCols=20
NRows=1000

data_matrix<-matrix(rnorm(NCols*NRows), ncol=NCols)
data_matrix <- data_matrix*10
dataset_x = as.data.frame(data_matrix)

beta_count = 5
zero_count = 20-beta_count
set.seed(12345)
estimates_df<-as.data.frame(matrix(runif(20), ncol=20))
vec <- c(rep(0, zero_count), rep(1, beta_count))
vec_random<-sample(vec, size = 20, replace = FALSE)      
vec_random_df=as.data.frame(matrix(vec_random, ncol=20))
estimates_df_treated <- vec_random_df*estimates_df
replicate_mat = matrix(rep(1, 10000), ncol=20)
beta_matrix = as.matrix(estimates_df_treated)
err= rnorm(1000)
dataset_y <- as.data.frame(as.matrix(dataset_x) %*% t(beta_matrix) + err)
colnames(dataset_y) <- 'response'

dataset_y$response <- dataset_y$response + err
colnames(dataset_y) <- 'response'

dataset_final<-cbind(dataset_x,dataset_y)

set.seed(12345)
indices <- sample(1:nrow(dataset_final),100)

dataset_train<-dataset_final[indices,]
dataset_test<-dataset_final[-indices,]

regfit.full <- regsubsets(response~., data = dataset_train, nbest = 1, nvmax = 20, method = "exhaustive")

my_sum <- summary(regfit.full)

par(mfrow = c(2,2))
plot(my_sum$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
plot(my_sum$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")

plot(my_sum$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(my_sum$adjr2, xlab = "Number of Variables", ylab = "Adjusted R^2", type = "l")

X_train_mat <- model.matrix(response~.,data = dataset_train )
X_test_mat <- model.matrix(response~.,data = dataset_test )

errors_train=rep(0,20)
errors_test=rep(0,20)
for(i in 1:20){
  best_coef <- coef(regfit.full,id=i)
  pred_values_train <- X_train_mat[,names(best_coef)]%*%best_coef
  pred_values_test <- X_test_mat[,names(best_coef)]%*%best_coef
  errors_train[i] <- mean((pred_values_train-dataset_train$response)^2)
  errors_test[i] <- mean((pred_values_test-dataset_test$response)^2)
}

par(mfrow = c(1,2))
# plot(errors_train, xlab = 'Number of variables selected', ylab = 'MSE', type = 'l', main = 'Train - Chosen Beta count = 5')
# plot(errors_test, xlab = 'Number of variables selected', ylab = 'MSE', type = 'l', main = 'Test - Chosen Beta count = 5')

plot(errors_train, xlab = 'Number of variables selected', ylab = 'MSE', main = 'Train - Chosen Beta count = 5')
plot(errors_test, xlab = 'Number of variables selected', ylab = 'MSE', main = 'Test - Chosen Beta count = 5')


# Studying the coefficients 

best_coef_5 <- coef(regfit.full,id=5)

best_coef_5
beta_matrix
# 
# > best_coef_5
# (Intercept)               V7            V9                                   V15           V17          V20 
# 0.42207165             0.32465291      0.71449835                            0.38201244    0.04590932   0.94904069 
# > beta_matrix
# V1 V2 V3 V4 V5 V6        V7 V8         V9 V10 V11 V12 V13         V14        V15 V16       V17 V18 V19  V20
# [1,]  0  0  0  0  0  0 0.3250954  0    0.7277053   0   0   0   0 0.001136587 0.3912033 0 0 0   0        0.9516588
