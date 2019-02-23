#Group 6 DSO530
#----------------------------------------------------------------------------------
#1st version of naives bayes model
#----------------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(naivebayes)
?naive_bayes
#read all the original data 
clean_data = read.csv(file = "all_data5.csv")
summary(clean_data)
#change the label"hotel culster" which is numerical to categorical 
clean_data$hotel_cluster = as.factor(clean_data$hotel_cluster)
#select the train data and the test data, training data is sampled 70% of all original data.
set.seed(1)
train_id = sample(nrow(clean_data), nrow(clean_data)*0.7)
train_data = clean_data[train_id,]
test_data = clean_data[-train_id,]
test_data_y = test_data$hotel_cluster
#make sure the label is categorical, so that the bayes algorithm can work properlly
class(test_data$hotel_cluster)
#train the naive bayes model
model = naive_bayes(hotel_cluster ~ ., data = train_data)
#use bayes model to predict the probability of hotel culster for each instance 
predict_test_data_prob = predict(model, test_data, type = "prob")
#creat 10 columns to store the 10 hignest probability from naive bayes model for each instance
test_data$pred1 = -1
test_data$pred2 = -1
test_data$pred3 = -1
test_data$pred4 = -1
test_data$pred5 = -1
test_data$pred6 = -1
test_data$pred7 = -1
test_data$pred8 = -1
test_data$pred9 = -1
test_data$pred10 = -1
test_data$outcome = FALSE
#for each insa=tance, inply hte model and store 10 hignest probability
#from naive bayes model for each instance, and if one of the 19 finals 
#outcomes matches the labal ,it means success.
for (i in 1:10) {
  for (j in 1: nrow(predict_test_data_prob)) {
    temp = order(predict_test_data_prob[j,], decreasing = T)
    test_data[j, 22+i] = temp[i] -1
    if(test_data[j, 22+i] == test_data[j, "hotel_cluster"]){
      test_data[j, "outcome"] = TRUE
    }
  }
}
#check the outcome
summary(test_data$outcome)
#the final accuracy is 0.2994011 for the 1st version naive bayes model
17247/(40358+17247) 
#----------------------------------------------------------------------------------
#2nd version of naives bayes model, which convert all the attributes to categorical
#----------------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(naivebayes)
?naive_bayes
#read all the original data 
clean_data = read.csv(file = "all_data5.csv")
summary(clean_data)
#change all the data which is numerical before to categorical 
for (i in 1:24) {
  clean_data[,i] = as.factor(clean_data[,i])
}
clean_data$hotel_cluster = as.factor(clean_data$hotel_cluster)
#select the train data and the test data, training data is sampled 70% of all original data.
set.seed(1)
train_id = sample(nrow(clean_data), nrow(clean_data)*0.7)
train_data = clean_data[train_id,]
test_data = clean_data[-train_id,]
test_data_y = test_data$hotel_cluster
#make sure the label is categorical, so that the bayes algorithm can work properlly
class(test_data$hotel_cluster)
#train the naive bayes model
model = naive_bayes(hotel_cluster ~ ., data = train_data)
#use bayes model to predict the probability of hotel culster for each instance 
predict_test_data_prob = predict(model, test_data, type = "prob")
#creat 10 columns to store the 10 hignest probability from naive bayes model for each instance
test_data$pred1 = -1
test_data$pred2 = -1
test_data$pred3 = -1
test_data$pred4 = -1
test_data$pred5 = -1
test_data$pred6 = -1
test_data$pred7 = -1
test_data$pred8 = -1
test_data$pred9 = -1
test_data$pred10 = -1
test_data$outcome = FALSE
#for each insa=tance, inply hte model and store 10 hignest probability
#from naive bayes model for each instance, and if one of the 19 finals 
#outcomes matches the labal ,it means success.
for (i in 1:10) {
  for (j in 1: nrow(predict_test_data_prob)) {
    temp = order(predict_test_data_prob[j,], decreasing = T)
    test_data[j, 22+i] = temp[i] -1
    if(test_data[j, 22+i] == test_data[j, "hotel_cluster"]){
      test_data[j, "outcome"] = TRUE
    }
  }
}
#check the outcome
summary(test_data$outcome)
#the final accuracy is 0.4293898 for the 2nd version naive bayes model
24735/57605#10  0.4293898
#----------------------------------------------------------------------------------
#3rd version of naives bayes model, which adds Laplace smoothing preventing the 0 value condition
#----------------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(naivebayes)
?naive_bayes
#read all the original data 
clean_data = read.csv(file = "all_data5.csv")
summary(clean_data)
#change all the data which is numerical before to categorical 
for (i in 1:24) {
  clean_data[,i] = as.factor(clean_data[,i])
}
clean_data$hotel_cluster = as.factor(clean_data$hotel_cluster)
#select the train data and the test data, training data is sampled 70% of all original data.
set.seed(1)
train_id = sample(nrow(clean_data), nrow(clean_data)*0.7)
train_data = clean_data[train_id,]
test_data = clean_data[-train_id,]
test_data_y = test_data$hotel_cluster
#make sure the label is categorical, so that the bayes algorithm can work properlly
class(test_data$hotel_cluster)
#train the naive bayes model
model = naive_bayes(hotel_cluster ~ ., data = train_data, laplace = 1)
#use bayes model to predict the probability of hotel culster for each instance 
predict_test_data_prob = predict(model, test_data, type = "prob")
#creat 10 columns to store the 10 hignest probability from naive bayes model for each instance
test_data$pred1 = -1
test_data$pred2 = -1
test_data$pred3 = -1
test_data$pred4 = -1
test_data$pred5 = -1
test_data$pred6 = -1
test_data$pred7 = -1
test_data$pred8 = -1
test_data$pred9 = -1
test_data$pred10 = -1
test_data$outcome = FALSE
#for each insa=tance, inply hte model and store 10 hignest probability
#from naive bayes model for each instance, and if one of the 19 finals 
#outcomes matches the labal ,it means success.
for (i in 1:10) {
  for (j in 1: nrow(predict_test_data_prob)) {
    temp = order(predict_test_data_prob[j,], decreasing = T)
    test_data[j, 22+i] = temp[i] -1
    if(test_data[j, 22+i] == test_data[j, "hotel_cluster"]){
      test_data[j, "outcome"] = TRUE
    }
  }
}
#check the outcome
summary(test_data$outcome)
#the final accuracy is 0.5960767 for the 3rd version naive bayes model
34337/57605#10  0.5960767
#----------------------------------------------------------------------------------
#4th version of naives bayes model, which remove all the highly-correalted attributes
#which are "user_location_country", "hotel_continent"
#----------------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(naivebayes)
?naive_bayes
#read all the original data 
clean_data = read.csv(file = "all_data5.csv")
summary(clean_data)
#remove the attributes "user_location_country", "hotel_continent"
clean_data = subset(clean_data, select = -c(user_location_country, hotel_continent))
#change all the data which is numerical before to categorical 
for (i in 1:22) {
  clean_data[,i] = as.factor(clean_data[,i])
}
clean_data$hotel_cluster = as.factor(clean_data$hotel_cluster)
#select the train data and the test data, training data is sampled 70% of all original data.
set.seed(1)
train_id = sample(nrow(clean_data), nrow(clean_data)*0.7)
train_data = clean_data[train_id,]
test_data = clean_data[-train_id,]
test_data_y = test_data$hotel_cluster
#make sure the label is categorical, so that the bayes algorithm can work properlly
class(test_data$hotel_cluster)
#train the naive bayes model
model = naive_bayes(hotel_cluster ~ ., data = train_data, laplace = 1)
#use bayes model to predict the probability of hotel culster for each instance 
predict_test_data_prob = predict(model, test_data, type = "prob")
#creat 10 columns to store the 10 hignest probability from naive bayes model for each instance
test_data$pred1 = -1
test_data$pred2 = -1
test_data$pred3 = -1
test_data$pred4 = -1
test_data$pred5 = -1
test_data$pred6 = -1
test_data$pred7 = -1
test_data$pred8 = -1
test_data$pred9 = -1
test_data$pred10 = -1
test_data$outcome = FALSE
#for each insa=tance, inply hte model and store 10 hignest probability
#from naive bayes model for each instance, and if one of the 19 finals 
#outcomes matches the labal ,it means success.
for (i in 1:4) {
  for (j in 1: nrow(predict_test_data_prob)) {
    temp = order(predict_test_data_prob[j,], decreasing = T)
    test_data[j, 22+i] = temp[i] -1
    if(test_data[j, 22+i] == test_data[j, "hotel_cluster"]){
      test_data[j, "outcome"] = TRUE
    }
  }
}
#check the outcome
summary(test_data$outcome)
#the final accuracy is 0.6032636 for the 4th(final) version naive bayes model
34751/57605#10  

#----------------------------------------------------------------------------------
#*********************************************************************************
#So the final accuracy for Naive Bayes Model is 0.6032636
#*********************************************************************************
#----------------------------------------------------------------------------------


