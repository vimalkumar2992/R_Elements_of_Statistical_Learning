#######################################################
# Statistical Data Mining I - EAS 506
# Home work I
#
# Vimal Kumarasamy
# Created: 9/7/2019
# Modified: 9/14/2019
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
library(plyr) # We need plyr library to get summary at a varible level 
# install.packages("dplyr")
library(dplyr)
# install.packages("sqldf") # sqldf for performing data aggregations
library(sqldf)
# installing DT package, for the ease of understanding data
# install.packages("DT")
library(DT)
# Installing corrplot for visualizing correlation plot
# install.packages("corrplot")
library(corrplot)

# Loading the dataset
# Observations(Obs.) - Headers are present, delimiter is comma
cereal <- read.delim("cereal.csv", sep = ",", header= TRUE)

# There are 77 datapoints and 16 variables in the dataset
dim(cereal)

# Studying the column names 
col <- colnames(cereal)

# Quick summary of the variables, to study if they are continuous or categorical 
# Referring to the context of the dataset from http://statweb.stanford.edu/~owen/courses/202/Cereals.txt 

# Obs. - name, mfr, type are categorical and won't have the quartile ranges
# carbo, sugars and potass variables have negative values. 
# As per reference, -1 means a missing observation. We can't omit these datapoints based on just one field
# The available datapoints are already very less
# We can makes these values nulls so that it won't be used for any plots
summary (cereal)

# Replacing the negative values of 3 variables with NA value
# looking at the histogram of carbo before imputing

hist(cereal$carbo)
# Obs. - The range is starting from -5

cereal$carbo[cereal$carbo<0] <- NA
cereal$sugars[cereal$sugars<0] <- NA
cereal$potass[cereal$potass<0] <- NA

hist(cereal$carbo)
# Obs. - The range starts from 5 after imputing

# What is the unique primary key of the dataset? 
# Are there duplicated at the name level?
# 77 
# Obs. The name seems to be the primary key  
length(unique(cereal$name))

# 7 
# There are 7 manufacturers in the dataset
unique(cereal$mfr)


# Let's look at how many datapoints are there for every manufacturer and type 
# Obs. - 'N' and 'Q' mfr makes both cold and hot cereals 
# H - A,N,Q
# C - G,K,N,P,Q

count(cereal,vars=c("mfr","type"))

#   mfr type freq
#1   A    H    1
#2   G    C   22
#3   K    C   23
#4   N    C    5
#5   N    H    1
#6   P    C    9
#7   Q    C    7
#8   Q    H    1
#9   R    C    8

# Lets look at the response variable 
x11()
boxplot(cereal$rating, horizontal = TRUE, xlab="Rating")

# Obs. - There is an outlier with a very high rating
# let's look at the data point 

rating_max=max(cereal$rating)
subset(cereal, rating==rating_max)

# It's a cold cereal - All-Bran with Extra Fiber - mfr - K


# Are the rating different based on the type of cereal?
cereal_H=subset(cereal, type=='H')
cereal_C=subset(cereal, type=='C')

par(mfrow = c(2,1))
boxplot(cereal_H$rating, horizontal = TRUE) 
title("Hot cereal")
boxplot(cereal_C$rating, horizontal = TRUE) 
title("Cold cereal")
# Obs. - Hot cereals have significantly higher rating compared to cold cereals

#####################################################################################
# Let's study the correlation  matrix of all the variables 
#####################################################################################
# Ignoring the observations with NA values - imputed values
# Removing cup size from the correlation matrix, as its numeric but not related to the rating
cereal_1<-select(cereal,-cups,-mfr,-name,-type)
cormat<-signif(cor(cereal_1 , use="complete.obs"),2)
x11()
corrplot.mixed(cormat,lower.col="black", number.cex=0.7,tl.pos="lt")

# Obs. - Fiber, Protein and potassium seem to have positive correlation with rating
# The different data elements of the dataset are per serving, and so it requires no normalization

xyplot(rating ~ fiber, type = c("p", "smooth"), data=cereal) 
xyplot(log(rating) ~ log(fiber), type = c("p", "smooth"), data=cereal) 
# Obs. There are few outlier values in fiber - which are significantly higher
# These outlie will skew the prediction 
# Lets look at a boxplot to get a clearer picture with respect to Inter Quartile Range 
boxplot(cereal$fiber, horizontal = TRUE)
# Its evident from the boxplot that the outliers are present 
# Capping or removing doesn't seem correct as the data seem to be out of trend but its correct/possible
# Bucketing the variables or variable transformation can be helpful 

xyplot(rating ~ (log(fiber)), type = c("p", "smooth"), data=cereal) 
# Log transformation seems to bring the trend almost linear, however bucketing might also be a good idea
cereal$fiber_log<-log(cereal$fiber)

summary(cereal$fiber)
# Let's make 4 buckets based on the quartiles - and one hot encoding to make 4 1/0 variables
# less than 1.0001 / Less than 2.0001 / Less than 3.0001/ Rest 

cereal$fiber_q1 <- ifelse(cereal$fiber  < 1.0001,1,0)
cereal$fiber_q2 <- ifelse(cereal$fiber  < 2.0001 & cereal$fiber>1.0001,1,0)
cereal$fiber_q3 <- ifelse(cereal$fiber  < 3.0001 & cereal$fiber>2.0001,1,0)
cereal$fiber_q4 <- ifelse(cereal$fiber  > 3.0001,1,0)

xyplot(rating ~ protein, type = c("p", "smooth"), data=cereal) 
# Obs. - We could see that there is a positive relationship between Protein and rating
# However after a point, increase in protein doesn't result in significant increase in rating
# Log transformation might make the plot linear 

xyplot(rating ~ log(protein), type = c("p", "smooth"), data=cereal) 
cereal$protein_log<-log(cereal$protein)
# Log transformation of protein has helped in making the plot linear

xyplot(rating ~ potass, type = c("p", "smooth"), data=cereal) 
# There is no clear trend, but there us some influence due to outlier
# 300 mg of potassium seems to be higher compared to the other datapoints 
# An average banana contains about 422 mg of potassium, so the data seems credible but out of trend 

xyplot(rating ~ potass^0.5, type = c("p", "smooth"), data=cereal)
# In order to reduce the span of potassium, taking a square root of potassium seem to help here
cereal$potass_sq_rt <- cereal$potass^0.5


##################################################################################################
# Lets look at few variables that are having negative correlation with rating
##################################################################################################
# Sugars and Calories 
xyplot(rating ~ sugars, type = c("p", "smooth"), data=cereal) 

# The scatterplot seems to be pretty clean with a negative linear relationship 
# Lets look at boxplot if there are any visible outliers 

boxplot(cereal$sugars, horizontal = TRUE)
# Box plot also shows that there are no drastic outliers, we can directly consume this variable


xyplot(rating ~ calories, type = c("p", "smooth"), data=cereal) 
# The plot looks like a inverted sigmoid

# Lets experiment with some transformations
xyplot(rating ~ log(calories), type = c("p", "smooth"), data=cereal)
# making calories into polynomial / log transformation doesn't seem to help in making it look linear
# Lets take it in its raw form, and see if it adds value to prediction
# Provided it has high correlation with sugar, may be one of them is all we would need - lets experiment

################################################################################################
# Creating few interaction variables as an experiment
#Studying a composite a score
cereal$score_1<-(cereal$protein*cereal$vitamins/(cereal$fat*cereal$calories))
xyplot(cereal$score_1 ~ rating, type = c("p", "smooth"), data=cereal)

# Obs. - There seems to be a strong positive relationship between rating and composite score_1
# however, there are multiple Inf and NaN values due to the absence of Numerator or denominator being 0

################################################################################################
# Studying other variables to get an idea 
# Making a ggplot, with x = weight, fill under aes accepts the feature based on which y will be populated
# for Histogram, the rest of the features are choosing color and labels

weight_plot<-ggplot(cereal,aes(x = weight, fill = mfr)) +
geom_histogram() +
scale_fill_brewer(palette = "Set1") +
scale_x_continuous(name = "weight", expand = c(0,0)) +
scale_y_continuous(name = "Count", expand = c(0,0), limits = c(0, 70)) +
labs(fill = "mfr", title = "weights across mfr") +
theme_minimal()

plot(weight_plot)
# majority of the cereals have 1 ounce per serving

# Are there high level trends between rating and mfr
# N has significantly higher rating, however its just based on 1 data point
bwplot(mfr ~ rating, xlab = "Rating", data = cereal)

xyplot(vitamins ~ rating, type = c("p", "smooth"), data=cereal)
# Vitamins lacks variability, and its better we treat it as a factor variable 
# Also the range is pretty vast, using it as a continuous variable might not add value in predicting rating

unique(cereal$vitamins)
# There are 3 unique values, lets make 3 variables out of it 

cereal$vitamins_0 <- ifelse(cereal$vitamins ==0,1,0)
cereal$vitamins_25 <- ifelse(cereal$vitamins ==25,1,0)
cereal$vitamins_100 <- ifelse(cereal$vitamins ==100,1,0)

xyplot(rating ~ sodium, type = c("p", "smooth"), data=cereal)
# Sodium shows a non-linear trend such that a mid-range in the distribution has a lower rating observed

# Lets bucket it into 3 different variables based on percentile points
summary(cereal$sodium)
# Let the break points be 130, 210
cereal$sodium_1 <- ifelse(cereal$sodium <130.001,1,0)
cereal$sodium_2 <- ifelse(cereal$sodium <210.001 & cereal$sodium >130.001,1,0)
cereal$sodium_3 <- ifelse(cereal$sodium >210.001,1,0)


xyplot(rating ~ shelf, type = c("p", "smooth"), data=cereal)
# As shelf is a variable that tells the position of the cereal, we can use it a factor variable
bwplot(shelf ~ rating, xlab = "Rating", data = cereal)
# There seems to be no direct relationship between shelf and rating

cereal$shelf_1 <- ifelse(cereal$shelf==1,1,0)
cereal$shelf_2 <- ifelse(cereal$shelf==2,1,0)
cereal$shelf_3 <- ifelse(cereal$shelf==3,1,0)


xyplot(rating ~ fat, type = c("p", "smooth"), data=cereal)
# There is weak linear relationship between fat and rating, we can consume it in its raw form


xyplot(rating ~ carbo, type = c("p", "smooth"), data=cereal)
# There is weak linear relationship between carbo and rating, we can consume it in its raw form

###################################################################################################
# Predict Rating with the features built so far 
###################################################################################################

# Using all the variables that were analyzed earlier and observing the least squared model
linear_model <- lm (rating~
                      fiber_q1+fiber_q2+fiber_q3+
                      vitamins_0+ vitamins_25+  
                      sugars+ fat + calories + 
                      sodium_1 + sodium_2 + 
                      protein_log +
                      potass_sq_rt, data=cereal )
summary(linear_model)

# Obs. - Vitamins_0, fiber_q1, fiber_q2, fiber_q3 are turning out to be insignificant 
# Obs. - Lets remove vitamins_0 and include vitamins_100

linear_model <- lm (rating~
                      fiber_q1+fiber_q2+fiber_q3+
                      vitamins_100+ vitamins_25+  
                      sugars+ fat + calories + 
                      sodium_1 + sodium_2 + 
                      protein_log +
                      potass_sq_rt, data=cereal )
summary(linear_model)

# Obs - Including vitamins_100 is a bad idea, as it has also made vitamins_25 insignificant 
# Lets retain vitamins_25, remove other vitamins variables

linear_model <- lm (rating~
                      fiber_q1+fiber_q2+fiber_q3+
                      vitamins_25+  
                      sugars+ fat + calories + 
                      sodium_1 + sodium_2 + 
                      protein_log +
                      potass_sq_rt, data=cereal )
summary(linear_model)

# Obs. - Other than fiber, all other variables are significant 
# fiber is correlated with protein, which is represented by log_protein - its significant already
# lets drop fiber variables one by one

linear_model <- lm (rating~
                      fiber_q1+fiber_q2+
                      vitamins_25+  
                      sugars+ fat + calories + 
                      sodium_1 + sodium_2 + 
                      protein_log +
                      potass_sq_rt, data=cereal )
summary(linear_model)


# Obs. fiber_q2 is the only insignificant variable
# Lets remove that as well 

linear_model <- lm (rating~
                      fiber_q1+
                      vitamins_25+  
                      sugars+ fat + calories + 
                      sodium_1 + sodium_2 + 
                      protein_log +
                      potass_sq_rt, data=cereal )
summary(linear_model)

# Obs. - fiber_q1 also turned out to be insignificant.
# Obs. - Let's retain fiber_q2 and remove fiber_q1

linear_model <- lm (rating~
                      fiber_q2+
                      vitamins_25+  
                      sugars+ fat + calories + 
                      sodium_1 + sodium_2 + 
                      protein_log +
                      potass_sq_rt, data=cereal )
summary(linear_model)

# Even then fiber_q2 turns out to be insignificant, lets remove all fiber variables and check if R sq drops

linear_model <- lm (rating~
                      vitamins_25+  
                      sugars+ fat + calories + 
                      sodium_1 + sodium_2 + 
                      protein_log +
                      potass_sq_rt, data=cereal )
summary(linear_model)

# There is no significant drop in adj. R sq, so this model seems to be stable and parsimonious 
# Lets study residual plots and experiment a bit more 

linear_model <- lm (rating~
                      vitamins_25+  
                      sugars+ fat + calories + 
                      sodium_1 + sodium_2 + 
                      protein_log +
                      potass_sq_rt +
                      carbo, data=cereal )
summary(linear_model)

# Carbo is not significant 

linear_model <- lm (rating~
                      vitamins_25+  
                      sugars+ fat + calories + 
                      sodium_1 + sodium_2 + 
                      protein_log +
                      potass_sq_rt + 
                      weight, data=cereal )
summary(linear_model)

# weight is also not significant 

# Lets check if shelf is adding value in the prediction 
linear_model <- lm (rating~
                      vitamins_25+  
                      sugars+ fat + calories + 
                      sodium_1 + sodium_2 + 
                      protein_log +
                      potass_sq_rt + 
                      shelf_1 + shelf_2, data=cereal )
summary(linear_model)

# both the variables are not significant 
# Lets check one at a time

linear_model <- lm (rating~
                      vitamins_25+  
                      sugars+ fat + calories + 
                      sodium_1 + sodium_2 + 
                      protein_log +
                      potass_sq_rt + 
                      shelf_1 , data=cereal )
summary(linear_model)

linear_model <- lm (rating~
                      vitamins_25+  
                      sugars+ fat + calories + 
                      sodium_1 + sodium_2 + 
                      protein_log +
                      potass_sq_rt + 
                      shelf_2, data=cereal )
summary(linear_model)

# Obs. - both the shelf variables are not significant



linear_model <- lm (rating~
                      vitamins_25+  
                      sugars+ fat + calories + 
                      sodium_1 + sodium_2 + 
                      protein_log +
                      potass_sq_rt, data=cereal )
summary(linear_model)

# Residuals (Parsimonious and Stable model)
#  Min      1Q  Median      3Q     Max 
# -6.2331 -2.3175 -0.6409  1.7151 12.3545 


# Residuals (Least squared model)
#  Min      1Q  Median      3Q     Max 
# -6.5781 -2.1612 -0.3042  1.5197 14.2951 

# Comparing the stable model residual distribution against the least squared model shows that both are similar
# Additional variables which are not significant are also not helping in reducing the residuals 


# 2. a
# Significant predictors 
# vitamin_25, sugars, fat, calories, sodium_1, sodium_2, protein_log, potassium_sq_rt 

# 2. b 
# Coffecient of sugars:
# -1.88748 
# It has a negative relationship with rating
# Inference: For every gram increase of sugar in the cereal, there is a -1.88748 units drop in the rating

# Checking the interaction variables 
# using * considers the product of the 2 variables and also the individual variables 

linear_model <- lm (rating ~
                      fiber_q1 * potass + 
                      fiber_q2 * potass + 
                      fiber_q3 * potass + 
                      vitamins_25 +  
                      sugars+ fat + calories + 
                      sodium_1 + sodium_2 + 
                      protein_log +
                      potass_sq_rt, data=cereal )
summary(linear_model)

# Obs. - Neither product not individual variables are turning out to be significant 

linear_model <- lm (rating ~
                      sugars : calories + 
                      vitamins_25 +  
                      sugars+ fat + calories + 
                      sodium_1 + sodium_2 + 
                      protein_log +
                      potass_sq_rt, data=cereal )
summary(linear_model)

# obs. - Sugars and Calories are turning out to be significant as an interaction variable
# This doesn't affect the stability of the other variables as well 

# 2. c
# sugars and calories as an interaction variable is significant using : operator (only product)
# using * no other interactions were identified 
# We can't combine unrelated variables through */: as the interaction is a product here 



####################################################################################################
# Accessing Zip code dataset from ESL package
# install.packages("ElemStatLearn")
library(ElemStatLearn)
ls("package:ElemStatLearn")
####################################################################################################
# Installing the packages for KNN
# install.packages("class")
library(class)
library(dplyr)

dim(zip.train)
# contains 7291 data points and 257 columns 

# Studying the variables
head(zip.train)

# Obs. The first column seems like it shows the number and the rest of the columns are the features 
# Checking the range of the columns, to decide on whether it needs normalization

max(zip.train[,2])
min(zip.train[,2])

# Removing the response variable from the test and train dataset
# Creating a vector for the y labels 
zip_test_all=as.data.frame(zip.test)
zip_test_2_3=subset(zip_test_all,V1==2|V1==3)
zip_train_all=as.data.frame(zip.train)
zip_train_2_3=subset(zip_train_all,V1==2|V1==3)

zip_test = select(zip_test_2_3, -1)
zip_test_y= select(zip_test_2_3, 1)
zip_train = select(zip_train_2_3, -1)
zip_train_y= select(zip_train_2_3, 1)
zip_train_y_vec=(zip_train_y$V1)
zip_test_y_vec=(zip_test_y$V1)

# k=13
# KNN <- knn(zip_train, zip_test, zip_train_y_vec, k)
# zip_test_y_pred <- KNN
# zip_test_y_pred<-as.numeric(as.character(zip_test_y_pred))
# zip_test_y_vec<-as.data.frame(zip_test_y_vec)
# zip_test_y_pred<-as.data.frame(zip_test_y_pred)
# test_results=cbind(zip_test_y_vec,zip_test_y_pred)
# test_results$match <- ifelse(test_results$zip_test_y_vec==test_results$zip_test_y_pred,1,0)
# test_accuracy=sum(test_results$match)/dim(test_results)[1]
# 
# KNN <- knn(zip_train, zip_train, zip_train_y_vec, k)
# 
# zip_train_y_pred <- KNN
# zip_train_y_pred<-as.numeric(as.character(zip_train_y_pred))
# zip_train_y_vec<-as.data.frame(zip_train_y_vec)
# zip_train_y_pred<-as.data.frame(zip_train_y_pred)
# train_results=cbind(zip_train_y_vec,zip_train_y_pred)
# train_results$match <- ifelse(train_results$zip_train_y_vec==train_results$zip_train_y_pred,1,0)
# train_accuracy=sum(train_results$match)/dim(train_results)[1]
# 
# train_accuracy
# test_accuracy


##################################################################################################
## Building a linear regression based classifier 
## Leaving the response variable as it is, the prediction is not bound to be either 2/3. It can be greater or lesser
## May be we can consider everything below 2.5 as 2 and everything above 2.5 as 3 
linear_classifier <- lm (V1 ~., data=zip_train_2_3)

zip_linear_classifier_test_pred<-predict(linear_classifier,zip_test)
zip_linear_classifier_test_pred<-as.data.frame(zip_linear_classifier_test_pred)
zip_linear_classifier_test_pred<-ifelse(zip_linear_classifier_test_pred>2.4999999,3,2)
zip_linear_classifier_test_pred<-as.data.frame(zip_linear_classifier_test_pred)
test_linear_classifier_results=cbind(zip_test_y_vec,zip_linear_classifier_test_pred)
test_linear_classifier_results$match <- ifelse(test_linear_classifier_results$zip_linear_classifier_test_pred==test_linear_classifier_results$zip_test_y_vec,1,0)
test_accuracy_linear_classifier=sum(test_linear_classifier_results$match)/dim(test_linear_classifier_results)[1]

zip_linear_classifier_train_pred<-predict(linear_classifier,zip_train)
zip_linear_classifier_train_pred<-as.data.frame(zip_linear_classifier_train_pred)
zip_linear_classifier_train_pred<-ifelse(zip_linear_classifier_train_pred>2.4999999,3,2)
zip_linear_classifier_train_pred<-as.data.frame(zip_linear_classifier_train_pred)
train_linear_classifier_results=cbind(zip_train_y_vec,zip_linear_classifier_train_pred)
train_linear_classifier_results$match <- ifelse(train_linear_classifier_results$zip_linear_classifier_train_pred==train_linear_classifier_results$zip_train_y_vec,1,0)
train_accuracy_linear_classifier=sum(train_linear_classifier_results$match)/dim(train_linear_classifier_results)[1]
train_accuracy_linear_classifier
test_accuracy_linear_classifier
# knn_accuracy=c(train_accuracy,test_accuracy)


#######################################################################################################

#Defining KNN loop
knn_classifier <- function(k){
  KNN <- knn(zip_train, zip_test, zip_train_y_vec, k)
  zip_test_y_pred <- KNN
  zip_test_y_pred<-as.numeric(as.character(zip_test_y_pred))
  zip_test_y_vec<-as.data.frame(zip_test_y_vec)
  zip_test_y_pred<-as.data.frame(zip_test_y_pred)
  test_results=cbind(zip_test_y_vec,zip_test_y_pred)
  test_results$match <- ifelse(test_results$zip_test_y_vec==test_results$zip_test_y_pred,1,0)
  test_accuracy=sum(test_results$match)/dim(test_results)[1]
  
  KNN <- knn(zip_train, zip_train, zip_train_y_vec, k)
  zip_train_y_pred <- KNN
  zip_train_y_pred<-as.numeric(as.character(zip_train_y_pred))
  zip_train_y_vec<-as.data.frame(zip_train_y_vec)
  zip_train_y_pred<-as.data.frame(zip_train_y_pred)
  train_results=cbind(zip_train_y_vec,zip_train_y_pred)
  train_results$match <- ifelse(train_results$zip_train_y_vec==train_results$zip_train_y_pred,1,0)
  train_accuracy=sum(train_results$match)/dim(train_results)[1]
  knn_accuracy=c(train_accuracy,test_accuracy)
  return(knn_accuracy)
  
}


# Creating a loop to run the KNN with different K values 
k_list=c(1,3,5,7,9,11,13,15)

solution_matrix = matrix(c(0),nrow=32, ncol=3)
cnt=1
for (k in k_list) {
  knn_accuracy<-knn_classifier(k)
  solution_matrix[cnt,1]=k
  solution_matrix[cnt+1,1]=k
  solution_matrix[cnt+2,1]=k
  solution_matrix[cnt+3,1]=k
  solution_matrix[cnt,2]="knn_train_accuracy"
  solution_matrix[cnt+1,2]="knn_test_accuracy"
  solution_matrix[cnt+2,2]="lr_train_accuracy"
  solution_matrix[cnt+3,2]="lr_test_accuracy"
  solution_matrix[cnt,3]=knn_accuracy[1]
  solution_matrix[cnt+1,3]=knn_accuracy[2]
  solution_matrix[cnt+2,3]=train_accuracy_linear_classifier
  solution_matrix[cnt+3,3]=test_accuracy_linear_classifier
  cnt=cnt+4
  }

solution_matrix_df=as.data.frame(solution_matrix)
names(solution_matrix_df) <- c ("K","Metric","Value")
solution_matrix_df$K=as.numeric(as.character(solution_matrix_df$K))
solution_matrix_df$Value=as.numeric(as.character(solution_matrix_df$Value))
solution_matrix_df$Value <- solution_matrix_df$Value 

#solution_matrix_df$Value<-format(round(solution_matrix_df$Value, 4))

# library(lattice)
# 
# xyplot(Value~K,type=c('l','p'),groups= Metric,data=solution_matrix_df,
#        auto.key=list(lines=TRUE, points=FALSE),xlab = "K values", ylab="Accuracy" ,
#        par.settings = list(superpose.line = list(lwd=2)),
#        main="KNN vs Linear Regression"
#        )

ggplot(solution_matrix_df, aes(x = K, y = Value)) + 
  ggtitle("KNN vs Linear Regression Classifier") +
  geom_line(aes(color = Metric)) + 
  scale_color_manual(values = c("red", "steelblue","green","grey")) +
  scale_y_continuous(labels = scales::percent) + 
  scale_x_continuous(breaks=seq(1,15,2)) + 
  xlab("K value") + 
  ylab("Accuracy")







# Saving the RDA file
save(cereal, file = "cereal_eda_lm.rda")
saveRDS(cereal, "cereal_eda_dataset.RData")
readRDS("cereal_eda_dataset.RData")