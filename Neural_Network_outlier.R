#####################################################################
## Vimal Kumarasamy                                                ##
#####################################################################

rm(list = ls())

# loading the required libraries
library(ElemStatLearn)
library(neuralnet)
library(randomForest) 

# recreating the spam data and splitting it into train and test
spam_raw<-spam

# choosing a few important variables for classification
vars<- c("A.52","A.53","A.7","A.55","A.56","A.16","A.21","A.25","A.57","A.5","A.24","A.19","spam")
spam_raw<-spam_raw[vars]

spam_raw$spam<-as.numeric(spam_raw$spam)
spam_raw$spam<- ifelse(spam_raw$spam==1,0,spam_raw$spam)
spam_raw$spam<- ifelse(spam_raw$spam==2,1,spam_raw$spam)
indices=sample(1:nrow(spam_raw),.70*nrow(spam_raw))
train<-spam_raw[indices,]
test<-spam_raw[-indices,]


# fitting a neural network on the original dataset

nn<-neuralnet(spam~.,data=train,hidden=4,stepmax=1e+05,threshold=0.2,err.fct='ce',linear.output = FALSE)
test_matrix=data.frame(test$A.52,test$A.53,test$A.7,test$A.55,test$A.56,test$A.16,test$A.21,test$A.25,test$A.57,test$A.5,test$A.24,test$A.19)
test_matrix<-as.matrix(test_matrix)
colnames(test_matrix) <- NULL
pred_raw <- compute(nn, covariate = test_matrix)
pred<-as.numeric(round(pred_raw$net.result))
true_class = test$spam
pred_class = pred
test_error = sum(abs(true_class - pred_class))/length(pred_class)
test_error
# 0.08689365

# creating a duplicate of the train dataset to introduce an outlier



out_error=(rep(0, 20))

for (i in 20:length(out_error))
{
  
  train_outlier<-train
  
  # adding a constant to all the features for a datapoint
  train_outlier[1,1] <- train_outlier[1,1]+i
  
  # fitting neural network on the modified dataset
  nn<-neuralnet(spam~.,data=train_outlier,hidden=4,stepmax=1e+05,threshold=0.3,err.fct='ce',linear.output = FALSE)
  test_matrix=data.frame(test$A.52,test$A.53,test$A.7,test$A.55,test$A.56,test$A.16,test$A.21,test$A.25,test$A.57,test$A.5,test$A.24,test$A.19)
  test_matrix<-as.matrix(test_matrix)
  colnames(test_matrix) <- NULL
  pred_raw <- compute(nn, covariate = test_matrix)
  pred<-as.numeric(round(pred_raw$net.result))
  true_class = test$spam
  pred_class = pred
  test_error = sum(abs(true_class - pred_class))/length(pred_class)
  out_error[i]=test_error
  
}

out_error_1<-out_error


out_error_multiple=(rep(0, 10))

for (i in 1:length(out_error))
{
  
  train_outlier<-train
  
  # adding a constant to all the features for a datapoint
  train_outlier[1,1] <- train_outlier[1,1]+(i*10)
  
  # fitting neural network on the modified dataset
  nn<-neuralnet(spam~.,data=train_outlier,hidden=4,stepmax=1e+05,threshold=0.3,err.fct='ce',linear.output = FALSE)
  test_matrix=data.frame(test$A.52,test$A.53,test$A.7,test$A.55,test$A.56,test$A.16,test$A.21,test$A.25,test$A.57,test$A.5,test$A.24,test$A.19)
  test_matrix<-as.matrix(test_matrix)
  colnames(test_matrix) <- NULL
  pred_raw <- compute(nn, covariate = test_matrix)
  pred<-as.numeric(round(pred_raw$net.result))
  true_class = test$spam
  pred_class = pred
  test_error = sum(abs(true_class - pred_class))/length(pred_class)
  out_error_multiple[i]=test_error
  
}


# 
# summary(train_outlier[,1])
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.0000   0.0000   0.0000   0.2856   0.3100 100.5200 

m=0.2856


fine_error=(rep(0, 10))
fine_error_sd=(rep(0, 10))

for (i in 1:length(fine_error))
{
  
  train_outlier<-train
  
  # adding a constant to all the features for a datapoint
  train_outlier[1,1] <- train_outlier[1,1]+((i-5)*m)
  fine_error_sd[i]=i-5
  
  # fitting neural network on the modified dataset
  nn<-neuralnet(spam~.,data=train_outlier,hidden=4,stepmax=1e+05,threshold=0.3,err.fct='ce',linear.output = FALSE)
  test_matrix=data.frame(test$A.52,test$A.53,test$A.7,test$A.55,test$A.56,test$A.16,test$A.21,test$A.25,test$A.57,test$A.5,test$A.24,test$A.19)
  test_matrix<-as.matrix(test_matrix)
  colnames(test_matrix) <- NULL
  pred_raw <- compute(nn, covariate = test_matrix)
  pred<-as.numeric(round(pred_raw$net.result))
  true_class = test$spam
  pred_class = pred
  test_error = sum(abs(true_class - pred_class))/length(pred_class)
  fine_error[i]=test_error
  
}






