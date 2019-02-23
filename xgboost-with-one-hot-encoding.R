#xgboost
#We tried using R to run xgboost, but we did not figure out a way to 
#output 10 predictions ranked by probability so we used R to do one hot encoding
#and used Python to actually run the model.

library(data.table)
library(Matrix)
library(xgboost)
library(dplyr)
library(mltools)

all_data=read.csv("all_data5.csv")
data_xgb=as.data.frame(clean_data)

#one hot encoding: convert categorical variables into dummies

shouldBeCategorical=c("site_name",'posa_continent', "user_location_country",
                      "user_location_region","user_location_city",
                      "is_mobile","is_package","channel",
                      "srch_destination_id","srch_destination_type_id",
                      "is_booking","hotel_continent","hotel_country",
                      "hotel_market","srch_month","check_in_month",
                      "hotel_cluster")

for(v in shouldBeCategorical) {
  data_xgb[[v]] <- as.factor(data_xgb[[v]])
}

data_xgb=model.matrix(hotel_cluster~posa_continent+orig_destination_distance+
                        is_mobile+is_package+channel+srch_adults_cnt+
                        srch_children_cnt+srch_rm_cnt+srch_destination_type_id+
                        is_booking+cnt+hotel_continent+month+
                        new_user_location_country+new_user_location_region+
                        hotel_duration_day+srch_ahead_day+
                        check_in_month+new_hotel_country+
                        new_hotel_market,data=data_xgb)

data_xgb=cbind(data_xgb,clean_data$hotel_cluster)
data_xgb=as.data.frame(data_xgb)
names(data_xgb)[76]='hotel_cluster'
str(data_xgb)
write.csv(data_xgb,"data_xgb.csv")
#we used Python to run the model. Please refer to XGBoost jupyter notebook.

# example code to run xgboost in R to get one output
# set.seed(530)
# train_id=sample(1:299603,239682)
# train_xgb=data_xgb[train_id,]
# test_xgb=data_xgb[-train_id,]

# define parameters for xgboost
# params=list(booster = 'gbtree',eta=0.1,max_depth=6,subsample=0.7,gamma=1,lambda=1,
#             colsample_bytree =1,min_child_weight = 1,objective = 'multi:softprob',
#             num_class=100,
#             eval_metric = 'merror',max_delta_step=30)

# which(names(data_xgb)=="hotel_cluster")

# cross validation
# cvFoldsList = createFolds(1:nrow(train_xgb), k = 5)

# train_xgb=train_xgb[complete.cases(train_xgb), ]
# predictors_train = as.matrix(train_xgb[,-76])
# response_train = train_xgb[,76]
# predictors_test = as.matrix(test_xgb[,-76])
# response_test = test_xgb[,76]

# Converting to DMatrix data type to be memory efficient

# train_xgb1 = xgb.DMatrix(predictors_train, label = response_train)
# test_xgb1= xgb.DMatrix(predictors_test, label = response_test)
# nrow(predictors_train)

# Use training dataset to do cross validation

# xgb_cv = xgb.cv(data = train_xgb1,
#                  params = params,
#                  nrounds = 5,
#                  maximize = FALSE,
#                  prediction = TRUE,
#                  folds = cvFoldsList,
#                  print_every_n = 5,
#                  early_stop_round = 5); gc()

# rounds = which.min(xgb_cv$dt[, test.rmse.mean])
# 
# y = train_xgb$hotel_cluster
# num.class = length(levels(y))
# levels(y) = 1:num.class
# head(y)
# y = as.matrix(y)
# x= as.matrix(train_xgb[,1:75])

# watchlist = list(train = train_xgb1 , eval = test_xgb1)
# model_xgb = xgb.train(data = train_xgb1, params = params, 
#                       watchlist = watchlist,
#                       nrounds = 5,
#                       early_stopping_rounds = 5)

# names(model_xgb)
# summary(model_xgb)

#train
#I did not find an option for R to predict multiple values

# p_train_xgb=predict(model_xgb,train_xgb1)
# head(as.data.frame(p_train_xgb))
# train_xgb=train_xgb %>%
#   mutate(predict_value=p_train_xgb) %>%
#   arrange(desc(predict_value))

#test
# p_test_xgb=predict(model_xgb,test_xgb1)
# test_xgb=test_xgb %>%
#   mutate(predict_value=p_test_xgb) %>%
#   arrange(desc(predict_value))
