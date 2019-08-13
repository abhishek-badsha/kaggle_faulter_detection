library(xgboost)
library(Matrix)
library(rBayesianOptimization)

library(caret)
master_data$ID <- NULL
master_data$TransactionID <- NULL
master_data$DeviceInfo <- NULL
dummy_for <- dummyVars(" ~ .",data = master_data)

train$ID <- NULL
train$TransactionID <- NULL
train$DeviceInfo <- NULL
train_mat <-data.frame(predict(dummy_for, newdata = train))

test$ID <- NULL
test$TransactionID <- NULL
test$DeviceInfo <- NULL
#test_mat <- dummyVars(" ~ .",data = test)
test_mat <-data.frame(predict(dummy_for, newdata = test))

# train_mat <- dummyVars(" ~ .",data = Filter(is.factor, train))
# train_mat <-data.frame(predict(train_mat, newdata = Filter(is.factor, train)))

# train_num_mat <- cbind(Filter(is.numeric,train),train_mat)

dtrain <- xgb.DMatrix(as.matrix(train_mat[,-1]),
                      label = train_mat$isFraud)
cv_folds <- KFold(train_mat$isFraud, nfolds = 5,
                  stratified = TRUE, seed = 0)
xgb_cv_bayes <- function(max_depth, min_child_weight, subsample) {
  cv <- xgb.cv(params = list(booster = "gbtree", eta = 0.01,
                             max_depth = max_depth,
                             min_child_weight = min_child_weight,
                             subsample = subsample, colsample_bytree = 0.3,
                             lambda = 1, alpha = 0,
                             objective = "binary:logistic",
                             eval_metric = "auc"),
               data = dtrain, nround = 100,
               folds = cv_folds, prediction = TRUE, showsd = TRUE,
               early_stopping_rounds = 5, maximize = TRUE, verbose = 0)
  list(Score = cv$evaluation_log$test_auc_mean[cv$best_iteration],
       Pred = cv$pred)
}
OPT_Res <- BayesianOptimization(xgb_cv_bayes,
                                bounds = list(max_depth = c(2L, 6L),
                                              min_child_weight = c(1L, 10L),
                                              subsample = c(0.5, 0.8)),
                                init_grid_dt = NULL, init_points = 10, n_iter = 20,
                                acq = "ucb", kappa = 2.576, eps = 0.0,
                                verbose = TRUE)

OPT_Res$Best_Par


xgbmpdel <- xgboost(data = dtrain,params = list(booster = "gbtree", eta = 0.01,
                                    max_depth = 6,
                                    min_child_weight = 5,
                                    subsample = 0.5, colsample_bytree = 0.3,
                                    lambda = 1, alpha = 0,
                                    objective = "binary:logistic",
                                    eval_metric = "auc"),nrounds = 100,early_stopping_rounds = 5,maximize = T)


test_mat$isFraudFALSE <- NULL
test_mat$isFraudTRUE <- NULL

dtest <- xgb.DMatrix(as.matrix(test_mat))
pred <- predict(xgbmpdel,dtest)



submission <- fread("../ieee-fraud-detection/sample_submission.csv")
submission[,isFraud := pred]
fwrite(submission,"../ieee-fraud-detection/xgb_submission_v0.1.csv")

