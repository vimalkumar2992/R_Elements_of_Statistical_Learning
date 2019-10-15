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

#install.packages("ISLR")
library(ISLR)

#######################################################
# Getting to know about the dataset
?College
dim(College)

# There are 777 observations and 18 variables 
# response variable: Apps - Number of applications received
# There is no other categorical variable representing the unique primary key of the dataset


#Q. 1.a
set.seed(12345)
indices <- sample(1:nrow(College),round((dim(College)[1])*0.8))

College_Train<-College[indices,]
College_Test<-College[-indices,]

linear_model <- lm (Apps~., data=College_Train )
summary(linear_model)

College_Test_pred=subset(College_Test,select=-c(Apps))
College_lm_test<-predict(linear_model,College_Test_pred)

length(College_lm_test)
head(College_lm_test)

compare_results<-as.data.frame(cbind(College_Test$Apps, College_lm_test))
colnames(compare_results)[1] <- "Apps_Actual"
colnames(compare_results)[2] <- "Apps_Predicted"

lm_rmse=sqrt(mean((compare_results$Apps_Actual-compare_results$Apps_Predicted)^2))
lm_rmse
# 790.8539

#Q. 1.b

# Fitting Ridge regression model

# Creating the X and Y matrices for glmnet

# Creating one-hot encoding variables for the factors and separating X and Y
X_train <- model.matrix(College_Train$Apps~.,College_Train[,-2]) 
Y_train <- College_Train$Apps
X_test <- model.matrix(College_Test$Apps~.,College_Test[,-2]) 
Y_test <- College_Test$Apps
ridge.mod = glmnet(X_train, Y_train, alpha=0)

###################################
# Model Selection 
###################################

cv.out <- cv.glmnet(X_train, Y_train, alpha = 0)
plot(cv.out)

names(cv.out)
bestlam <- cv.out$lambda.min
bestlam
ridge.pred <- predict(ridge.mod, s= bestlam, type = "coefficients")
ridge.pred2 <- predict(ridge.mod, s = bestlam, newx = X_test, type = "response")

dim(ridge.pred2)

compare_results_ridge_df<-as.data.frame(cbind(College_Test$Apps,ridge.pred2))
colnames(compare_results_ridge_df)[1] <- "Apps_Actual"
colnames(compare_results_ridge_df)[2] <- "Apps_Predicted"

ridge_rmse=sqrt(mean((compare_results_ridge_df$Apps_Actual-compare_results_ridge_df$Apps_Predicted)^2))
ridge_rmse

# 783.519

# Q 1.c

lasso.mod = glmnet(X_train, Y_train, alpha=1)

names(lasso.mod)
coef(lasso.mod)
dim(coef(lasso.mod))


###################################
#
# Model Selection 
###################################
?cv.glmnet
set.seed(12345)

cv.out <- cv.glmnet(X_train, Y_train, alpha = 1)
plot(cv.out)

names(cv.out)
bestlam <- cv.out$lambda.min
bestlam
lasso.pred <- predict(lasso.mod, s= bestlam, type = "coefficients")
lasso.pred2 <- predict(lasso.mod, s = bestlam, newx = X_test, type = "response")

dim(lasso.pred2)

compare_results_lasso_df<-as.data.frame(cbind(College_Test$Apps,lasso.pred2))
colnames(compare_results_lasso_df)[1] <- "Apps_Actual"
colnames(compare_results_lasso_df)[2] <- "Apps_Predicted"

lasso_rmse=sqrt(mean((compare_results_lasso_df$Apps_Actual-compare_results_lasso_df$Apps_Predicted)^2))
lasso_rmse
# 787.6368


# let's look at the coefficients
# the best lambda corresponds to the position 68
lasso.mod$lambda[81]
x=81
coef(ridge.mod)[,x]

# (Intercept)   (Intercept)    PrivateYes        Accept        Enroll     Top10perc     Top25perc 
# -2.262999e+03  0.000000e+00 -5.737865e+02  5.577314e-01  8.015879e-01  1.563583e+01  8.076037e+00 
# F.Undergrad   P.Undergrad      Outstate    Room.Board         Books      Personal           PhD 
# 1.288131e-01  7.225456e-02  9.148044e-03  1.765674e-01  3.506866e-01  6.631382e-03  2.344869e+00 
# Terminal     S.F.Ratio   perc.alumni        Expend     Grad.Rate 
# 8.696527e-01  1.211376e+01 -1.023287e+01  5.389010e-02  1.170007e+01

# outstate, personal and expend variables have very low coefficient - these can be considered as almost 0


# e PCR model

#install.packages('pls')
require(pls)
set.seed(12345)

pcr_model <- pcr(Apps~., data = College_Train, scale = TRUE, validation = "CV")

validationplot(pcr_model)
# The plot shows that when k=5, the error is around 1200 and incrasing components further doesn't help much

validationplot(pcr_model, val.type="MSEP")

# Let's look at R^2 to understand the explained variance
validationplot(pcr_model, val.type = "R2")



# The sweet spot is when k = 8, as beyond which there is no incrmental reduction in MSEP
College_Test_pred=subset(College_Test,select=-c(Apps))
PCR_pred <- predict(pcr_model,College_Test_pred,ncomp=8)
# PCR_pred


compare_results_pcr_df<-as.data.frame(cbind(College_Test$Apps,PCR_pred))
colnames(compare_results_pcr_df)[1] <- "Apps_Actual"
colnames(compare_results_pcr_df)[2] <- "Apps_Predicted"

pcr_rmse=sqrt(mean((compare_results_pcr_df$Apps_Actual-compare_results_pcr_df$Apps_Predicted)^2))
pcr_rmse

# 1105.95

# f PLS model

install.packages("caret")
library(caret)
set.seed(12345)
myfolds <- createMultiFolds(College_Train$Apps, k = 5, times = 10)
control <- trainControl("repeatedcv", index = myfolds, selectionFunction = "oneSE")

# Train PLS model
pls_model <- train(Apps ~ ., data = College_Train,
              method = "pls",
              tuneLength = 20,
              trControl = control,
              preProc = c("zv","center","scale"))

# Check CV profile
plot(pls_model)

pls_model_2 = plsr(Apps ~ ., data = College_Train, ncomp = 8)
PLS_pred <- predict(pls_model_2, College_Test_pred)
compare_results_pls_df<-as.data.frame(cbind(College_Test$Apps,PLS_pred))
colnames(compare_results_pls_df)[1] <- "Apps_Actual"
colnames(compare_results_pls_df)[2] <- "Apps_Predicted"

pls_rmse=sqrt(mean((compare_results_pls_df$Apps_Actual-compare_results_pls_df$Apps_Predicted)^2))
pls_rmse

# 922.3273

# The rmse numbers are showing lm model being the best, however while closely observing the PLS model with just 8 predictors
# is not that bad compared to the rest of the models 

# This also would lead to parsimonius model fit





