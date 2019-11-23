##############################################
# This code is to experiment with Neural Networks in R
# Vimal Kumar Kumarasamy
# Created: November 22, 2019
##############################################

# install.packages("neuralnet")
library(neuralnet)
library(ISLR)

# Load the data
data(Carseats)
car <- Carseats

# Recode sales as a binary respons
High <- ifelse(car$Sales<=8, "No", "Yes")
my_car <- data.frame(car[,-1], High) 

# set the seed, and put aside a test set
set.seed(12345)
test_indis <- sample(1:nrow(my_car), .20*nrow(my_car))
test <- my_car[test_indis, ]
train <- my_car[-test_indis, ]
y_train_true <- as.numeric(train$High)-1 
y_test_true <- as.numeric(test$High)-1

nn_train<-data.frame(train[,-11],y_train_true)
nn_test<-data.frame(test[,-11],y_test_true)

n <- names(nn_train)
f <- as.formula(paste("y_train_true ~", paste(n[!n %in% "y_train_true"], collapse = " + ")))

US<-as.numeric(nn_train$US)-1
nn_train<-data.frame(nn_train[,-10],US)
US<-as.numeric(nn_test$US)-1
nn_test<-data.frame(nn_test[,-10],US)



nn=neuralnet(y_train_true~Income+Price+Advertising+Education+US,data=nn_train, hidden=3,act.fct = "logistic",
             linear.output = FALSE)


pred<-predict(nn,newdata=nn_test)
pred_class<-ifelse(pred>0.5,1,0)
test_accuracy <- sum(abs(pred_class-y_test_true))/length(y_test_true)
test_accuracy
plot(nn)

