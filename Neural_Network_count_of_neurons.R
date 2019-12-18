#####################################################################
## Vimal Kumarasamy                                                ##
#####################################################################

rm(list = ls())

# loading the required libraries
library(ElemStatLearn)
library(neuralnet)
library(randomForest) 

# treating the spam data for classification
spam_raw<-spam
spam_raw$spam<-as.numeric(spam_raw$spam)
spam_raw$spam<- ifelse(spam_raw$spam==1,0,spam_raw$spam)
spam_raw$spam<- ifelse(spam_raw$spam==2,1,spam_raw$spam)

# Fitting a random forest to find the important variables
spam_raw$spam<-as.factor(spam_raw$spam)
rf_fit <- randomForest(spam~.,data=spam_raw, n.trees=500,na.action=na.roughfix)
#x11()
varImpPlot(rf_fit) # shows which variables are important
# important variables
# A.52+A.53+A.7+A.55+A.56+A.16+A.21+A.25+A.57+A.5+A.24+A.19


# recreating the spam data and splitting it into train and test
spam_raw<-spam
spam_raw$spam<-as.numeric(spam_raw$spam)
spam_raw$spam<- ifelse(spam_raw$spam==1,0,spam_raw$spam)
spam_raw$spam<- ifelse(spam_raw$spam==2,1,spam_raw$spam)
indices=sample(1:nrow(spam_raw),.70*nrow(spam_raw))
train<-spam_raw[indices,]
test<-spam_raw[-indices,]

# creating a list of possible neurons
# building a test error matrix
# iterating through the list to identify the suitable number of neurons

neurons=(seq(4, 10, by=1))
test_error_mat<-rep(0,length(neurons))

test_error_4<-0.06806662
test_error_5<-0.08616944
test_error_6<-0.07385952
test_error_7<-0.0782042
test_error_8<-0.0724113
test_error_9<-0.08037654
logistic_regression_test_error<-0.1100652

# sample code for fitting neural network and performing prediction
nn<-neuralnet(spam~A.52+A.53+A.7+A.55+A.56+A.16+A.21+A.25+A.57+A.5+A.24+A.19,data=train,hidden=9,stepmax=1e+05,threshold=0.20,err.fct='ce',linear.output = FALSE)
test_matrix=data.frame(test$A.52,test$A.53,test$A.7,test$A.55,test$A.56,test$A.16,test$A.21,test$A.25,test$A.57,test$A.5,test$A.24,test$A.19)
test_matrix<-as.matrix(test_matrix)
colnames(test_matrix) <- NULL
pred_raw <- compute(nn, covariate = test_matrix)
pred<-as.numeric(round(pred_raw$net.result))
true_class = test$spam
pred_class = pred
test_error = sum(abs(true_class - pred_class))/length(pred_class)
test_error

for (i in 1:length(neurons))
{
  
  nn<-neuralnet(spam~A.52+A.53+A.7+A.55+A.56+A.16+A.21+A.25+A.57+A.5+A.24+A.19,data=train,hidden=neurons[i],stepmax=1e+05,threshold=0.15,err.fct='ce',linear.output = FALSE)
  test_matrix=data.frame(test$A.52,test$A.53,test$A.7,test$A.55,test$A.56,test$A.16,test$A.21,test$A.25,test$A.57,test$A.5,test$A.24,test$A.19)
  test_matrix<-as.matrix(test_matrix)
  colnames(test_matrix) <- NULL
  pred_raw <- compute(nn, covariate = test_matrix)
  pred<-as.numeric(round(pred_raw$net.result))
  true_class = test$spam
  pred_class = pred
  test_error = sum(abs(true_class - pred_class))/length(pred_class)
  test_error_mat[i]=test_error
}


# fitting a logistic regression to compare the results of neural network
train$spam<-as.factor(train$spam)
glm.fit <- glm(spam~A.52+A.53+A.7+A.55+A.56+A.16+A.21+A.25+A.57+A.5+A.24+A.19,data=train,family = "binomial")
glm.probs.test <- predict(glm.fit, newdata = test, type = "response")
test_pred <- round(glm.probs.test)
test_error = sum(abs(true_class - test_pred))/length(test_pred)
test_error
# 0.1100652
