#####################################################################
## Vimal Kumarasamy                                                ##
#####################################################################

rm(list = ls())
library(ISLR)
library(e1071)

dim(OJ)
OJ_raw<-OJ
OJ_raw$Purchase<-as.numeric(OJ_raw$Purchase)-1
indices=sample(1:nrow(OJ_raw),.70*nrow(OJ_raw))
train<-OJ_raw[indices,]
test<-OJ_raw[-indices,]
train$Purchase<-as.factor(train$Purchase)


# fitting SVM over a list of cost parameters
svm_fit_linear <- tune(svm,Purchase~.,data=train,kernel="linear",ranges=list(cost=c(0.01,0.05,0.1,0.5,1,5,10)))

# Fitting a linear SVM classifier
cost_parameters=c(0.01,0.05,0.1,0.5,1,5,10)
train_error=rep(0,length(cost_parameters))
test_error=rep(0,length(cost_parameters))

for ( i in 1: length(cost_parameters))
{
  
  svm_fit_linear <- tune(svm,Purchase~.,data=train,kernel="linear",ranges=list(cost=c(cost_parameters[i])))
  best_mod_linear <- svm_fit_linear$best.model
  
  y_hat_test_linear_svm <- predict(best_mod_linear, newdata = test)
  test_pred<-as.numeric(y_hat_test_linear_svm)-1
  test_true<-test$Purchase
  test_error[i]=sum(abs(test_true-test_pred))/length(test_true)
  
  y_hat_train_linear_svm <- predict(best_mod_linear, newdata = train)
  train_pred<-as.numeric(y_hat_train_linear_svm)-1
  train_true<-as.numeric(train$Purchase)-1
  train_error[i]=sum(abs(train_true-train_pred))/length(train_true)
}

# plotting

axes=c(1,2,3,4,5,6,7)

plot(axes,test_error, type="l",col="green", xlab = "Cost Parameters", ylab = "Test error", main ='Cost parameters (0.01, 0.05, 0.1, 0.5, 1, 5, 10)')
plot(axes,train_error, type="l",col="green", xlab = "Cost Parameters", ylab = "Train error", main ='Cost parameters (0.01, 0.05, 0.1, 0.5, 1, 5, 10)')

# Fitting a radial kernel SVM classifier

cost_parameters=c(0.01,0.05,0.1,0.5,1,5,10)
train_error_radial=rep(0,length(cost_parameters))
test_error_radial=rep(0,length(cost_parameters))

for ( i in 1: length(cost_parameters))
{
  
  svm_fit__radial <- tune(svm,Purchase~.,data=train,kernel="radial",ranges=list(cost=c(cost_parameters[i])))
  best_mod__radial <- svm_fit__radial$best.model
  
  y_hat_test__radial_svm <- predict(best_mod__radial, newdata = test)
  test_pred<-as.numeric(y_hat_test__radial_svm)-1
  test_true<-test$Purchase
  test_error_radial[i]=sum(abs(test_true-test_pred))/length(test_true)
  
  y_hat_train__radial_svm <- predict(best_mod__radial, newdata = train)
  train_pred<-as.numeric(y_hat_train__radial_svm)-1
  train_true<-as.numeric(train$Purchase)-1
  train_error_radial[i]=sum(abs(train_true-train_pred))/length(train_true)
}

# plotting

axes=c(1,2,3,4,5,6,7)

plot(axes,test_error_radial, type="l",col="green", xlab = "Cost Parameters", ylab = "Test error Radial", main ='Cost parameters (0.01, 0.05, 0.1, 0.5, 1, 5, 10)')
plot(axes,train_error_radial, type="l",col="green", xlab = "Cost Parameters", ylab = "Train error Radial", main ='Cost parameters (0.01, 0.05, 0.1, 0.5, 1, 5, 10)')



# Fitting a polynomial kernel 

cost_parameters=c(0.01,0.05,0.1,0.5,1,2,4,6,8,10)
train_error_poly=rep(0,length(cost_parameters))
test_error_poly=rep(0,length(cost_parameters))

for ( i in 1: length(cost_parameters))
{
  
  svm_fit_poly <- tune(svm,Purchase~.,data=train,kernel="polynomial",degree=2,ranges=list(cost=c(cost_parameters[i])))
  best_mod_poly <- svm_fit__radial$best.model
  
  y_hat_test_poly <- predict(best_mod_poly, newdata = test)
  test_pred<-as.numeric(y_hat_test_poly)-1
  test_true<-test$Purchase
  test_error_poly[i]=sum(abs(test_true-test_pred))/length(test_true)
  
  y_hat_train_poly <- predict(best_mod_poly, newdata = train)
  train_pred<-as.numeric(y_hat_train_poly)-1
  train_true<-as.numeric(train$Purchase)-1
  train_error_poly[i]=sum(abs(train_true-train_pred))/length(train_true)
}

# plotting

axes=c(1,2,3,4,5,6,7,8,9,10)

plot(axes,test_error_poly, type="l",col="green", xlab = "Cost Parameters", ylab = "Test error Polynomial", main ='Cost parameters (0.01, 0.05, 0.1, 0.5, 1, 5, 10)')
plot(axes,train_error_poly, type="l",col="green", xlab = "Cost Parameters", ylab = "Train error Polynomial", main ='Cost parameters (0.01, 0.05, 0.1, 0.5, 1, 5, 10)')

