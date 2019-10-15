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
#install.packages("leaps")
library(leaps)

## Reading data

Train <- read.csv("ticdata2000.txt", sep='\t', header= FALSE)
Test <- read.csv("ticeval2000.txt", sep='\t', header= FALSE)
Test_Y <- read.csv("tictgts2000.txt", sep='\t', header= FALSE)
colnames(Test_Y) <- "V86"
head(Train)
head(Test)

# Quick study on the first variable
sort(unique(Train$V1))

# Taking a quick look at the class distribution in the dataset
Train %>% group_by(V86) %>% tally()


# V86     n
# <int> <int>
# 1     0  5474
# 2     1   348

linear_model <- lm (V86~., data=Train)
summary(linear_model)

caravan_pred<-predict(linear_model,Test)
# Studying the summary of the predicted values
# As we are forcefitting a classification problem through LR, we need to decide on a cutoff for 1/0 prediction

summary(caravan_pred)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -0.12796  0.01631  0.05435  0.05929  0.09379  0.80824 

# As the median predicted scores are way below 0.5, lets stick wit 0.5 to get an essence of the prediction 
# Keeping 0.5 as the cutoff resulted in poor accuracy, lets reduce the cutoff to 0.1

caravan_pred_class<-ifelse(caravan_pred>0.1,1,0)

compare_results<-as.data.frame(cbind(Test_Y$V86, caravan_pred_class))
colnames(compare_results)[1] <- "caravan_Actual"
colnames(compare_results)[2] <- "caravan_Predicted"

lm_error = sum(abs(compare_results$caravan_Actual-compare_results$caravan_Predicted)) / dim(compare_results)[1]
lm_error

lm_confusion_mat = compare_results %>% group_by(caravan_Predicted,caravan_Actual) %>% tally()

lm_confusion_mat
# 
# caravan_Predicted caravan_Actual     n
# <dbl>          <dbl> <int>
# 1                 0              0  3013
# 2                 0              1   112
# 3                 1              0   749
# 4                 1              1   126

lm_recall <- (126)/(126+112)
lm_precision <- (126)/(749+126)

lm_recall
# 0.52

lm_precision
# 0.144

# Model Interpretability
# 
# The customers who scored high on the below fields have higher likelihood of buying Caravan Insurance
# V4, V47, V57, V 58, V59, V76 , V78, V82
# The customers who scored low on the below fields have lower likelihood
# V 55


# Forward subset selection

caravan.fwd <- regsubsets(V86~., data = Train, nvmax = 86, method = "forward")
my_sum <- summary(caravan.fwd)

par(mfrow = c(2,2))
plot(my_sum$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
plot(my_sum$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")

plot(my_sum$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(my_sum$adjr2, xlab = "Number of Variables", ylab = "Adjusted R^2", type = "l")

# When variables = 22, there seems to be good Adj R^2 and other metrics seem pretty good 
caravan_fwd_model <- regsubsets(V86~., data = Train, nvmax = 22, method = "forward")
caravan_fwd_pred <- predict(caravan_fwd_model, data=Test)

caravan_fwd_model_22_coeffs <- coef(caravan_fwd_model ,22)

caravan_fwd_model_22 <- lm (V86~(V4+V7+V10+V16+V18+V21+V35+V36+V41+V42+V43+V44+V46+V47+V57+V58+V59+V79+V80+V82+V83+V85), data=Train)
summary(caravan_fwd_model_22)


caravan_fwd_pred<-predict(caravan_fwd_model_22,Test)

summary(caravan_fwd_pred)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -0.10027  0.01937  0.05405  0.05944  0.09304  0.73627 

# As the median predicted scores are way below 0.5, lets stick wit 0.5 to get an essence of the prediction 
# Keeping 0.5 as the cutoff resulted in poor accuracy, lets reduce the cutoff to 0.1

caravan_fwd_pred_class<-ifelse(caravan_fwd_pred>0.1,1,0)

compare_fwd_results<-as.data.frame(cbind(Test_Y$V86, caravan_fwd_pred_class))
colnames(compare_fwd_results)[1] <- "caravan_Actual"
colnames(compare_fwd_results)[2] <- "caravan_Predicted"

fwd_error = sum(abs(compare_fwd_results$caravan_Actual-compare_fwd_results$caravan_Predicted)) / dim(compare_fwd_results)[1]
fwd_error

#0.21375

fwd_confusion_mat = compare_fwd_results %>% group_by(caravan_Predicted,caravan_Actual) %>% tally()
fwd_confusion_mat

fwd_recall <- (122)/(238)
fwd_precision <- (122)/(739+122)

fwd_recall
# 0.512

fwd_precision
# 0.1416

# backward subset selection

caravan.bwd <- regsubsets(V86~., data = Train, nvmax = 86, method = "backward")
my_sum <- summary(caravan.bwd)

par(mfrow = c(2,2))
plot(my_sum$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
plot(my_sum$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")

plot(my_sum$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(my_sum$adjr2, xlab = "Number of Variables", ylab = "Adjusted R^2", type = "l")

# When variables = 22, there seems to be good Adj R^2 and other metrics seem pretty good 
caravan_bwd_model <- regsubsets(V86~., data = Train, nvmax = 22, method = "backward")


caravan_bwd_model_22_coeffs <- coef(caravan_bwd_model ,22)

caravan_bwd_model_22 <- lm (V86~(V4+V10+V18+V21+V30+V41+V42+V44+V46+V47+V55+V57+V58+V59+V63+V76+V78+V80+V82+V83+V84+V85), data=Train)
summary(caravan_bwd_model_22)


caravan_bwd_pred<-predict(caravan_bwd_model_22,Test)

summary(caravan_bwd_pred)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -0.09516  0.01870  0.05354  0.05877  0.09199  0.73846 

# As the median predicted scores are way below 0.5, lets stick wit 0.5 to get an essence of the prediction 
# Keeping 0.5 as the cutoff resulted in poor accuracy, lets reduce the cutoff to 0.1

caravan_bwd_pred_class<-ifelse(caravan_bwd_pred>0.1,1,0)

compare_bwd_results<-as.data.frame(cbind(Test_Y$V86, caravan_bwd_pred_class))
colnames(compare_bwd_results)[1] <- "caravan_Actual"
colnames(compare_bwd_results)[2] <- "caravan_Predicted"

bwd_error = sum(abs(compare_bwd_results$caravan_Actual-compare_bwd_results$caravan_Predicted)) / dim(compare_bwd_results)[1]
bwd_error

#0.2125

bwd_confusion_mat = compare_bwd_results %>% group_by(caravan_Predicted,caravan_Actual) %>% tally()
bwd_confusion_mat
# caravan_Predicted caravan_Actual     n
# <dbl>          <dbl> <int>
# 1                 0              0  3037
# 2                 0              1   125
# 3                 1              0   725
# 4                 1              1   113

bwd_recall <- (113)/(238)
bwd_precision <- (113)/(725+113)

bwd_recall
# 0.4747

bwd_precision
# 0.134

# Lasso Model

Train
Test_all<-cbind(Test,Test_Y)
Test_Y

X_Train <- model.matrix(Train$V86~.,Train[,-86])
X_Test <- model.matrix(Test_all$V86~.,Test_all[,-86])


Y_Train <- Train$V86
Y_Test <- Test_Y

lasso.mod = glmnet(X_Train, Y_Train, alpha=1)

###################################
# Model Selection 
###################################

cv.out <- cv.glmnet(X_Train, Y_Train, alpha = 1)
plot(cv.out)

names(cv.out)
bestlam <- cv.out$lambda.min
bestlam
lasso.pred <- predict(lasso.mod, s= bestlam, type = "coefficients")
lasso.pred2 <- predict(lasso.mod, s = bestlam, newx = X_Test, type = "response")

summary(lasso.pred2)
caravan_lasso_pred_class<-ifelse(lasso.pred2>0.1,1,0)

compare_results_lasso_df<-as.data.frame(cbind(Test_Y,caravan_lasso_pred_class))
colnames(compare_results_lasso_df)[1] <- "Caravan_Actual"
colnames(compare_results_lasso_df)[2] <- "Caravan_Predicted"

lasso_confusion_mat = compare_results_lasso_df %>% group_by(Caravan_Predicted,Caravan_Actual) %>% tally()
lasso_confusion_mat

lasso_recall <- (105)/(238)
lasso_precision <- (105)/(105+636)

lasso_recall
# 0.44
lasso_precision
# 0.14

# ridge Model

Train
Test_all<-cbind(Test,Test_Y)
Test_Y

X_Train <- model.matrix(Train$V86~.,Train[,-86])
X_Test <- model.matrix(Test_all$V86~.,Test_all[,-86])


Y_Train <- Train$V86
Y_Test <- Test_Y

ridge.mod = glmnet(X_Train, Y_Train, alpha=0)

###################################
# Model Selection 
###################################

cv.out <- cv.glmnet(X_Train, Y_Train, alpha = 0)
plot(cv.out)

names(cv.out)
bestlam <- cv.out$lambda.min
bestlam
ridge.pred <- predict(ridge.mod, s= bestlam, type = "coefficients")
ridge.pred2 <- predict(ridge.mod, s = bestlam, newx = X_Test, type = "response")

summary(ridge.pred2)
caravan_ridge_pred_class<-ifelse(ridge.pred2>0.1,1,0)

compare_results_ridge_df<-as.data.frame(cbind(Test_Y,caravan_ridge_pred_class))
colnames(compare_results_ridge_df)[1] <- "Caravan_Actual"
colnames(compare_results_ridge_df)[2] <- "Caravan_Predicted"

ridge_confusion_mat = compare_results_ridge_df %>% group_by(Caravan_Predicted,Caravan_Actual) %>% tally()
ridge_confusion_mat

ridge_recall <- (102)/(238)
ridge_precision <- (102)/(102+592)

ridge_recall
# 0.428
ridge_precision
# 0.14

