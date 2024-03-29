pakInstall(c("xgboost","Matrix","rBayesianOptimization","caret"))
load("processed_data.rda")

master_data$ID <- NULL
master_data$TransactionID <- NULL
master_data$DeviceInfo <- NULL
dummy_for <- dummyVars(" ~ .",data = master_data)
rm(master_data)
gc()

train$ID <- NULL
train$TransactionID <- NULL
train$DeviceInfo <- NULL
train_mat <-data.frame(predict(dummy_for, newdata = train))
rm(train)
gc()

# train_mat <- dummyVars(" ~ .",data = Filter(is.factor, train))
# train_mat <-data.frame(predict(train_mat, newdata = Filter(is.factor, train)))

# train_num_mat <- cbind(Filter(is.numeric,train),train_mat)

dtrain <- xgb.DMatrix(as.matrix(train_mat[,-1]),
                      label = train_mat$isFraud)

cv_folds <- KFold(train_mat$isFraud, nfolds = 5,
                  stratified = TRUE, seed = 0)
rm(train_mat)
gc()
saveRDS(dtrain,"dtrain.rds")

xgb_cv_bayes <- function(eta,max_depth, min_child_weight, subsample,colsample_bytree,alpha,lambda) {
  cv <- xgb.cv(params = list(booster = "gbtree", eta = eta,
                             max_depth = max_depth,
                             min_child_weight = min_child_weight,
                             subsample = subsample, colsample_bytree = colsample_bytree,
                             lambda = lambda, alpha = alpha,
                             objective = "binary:logistic",
                             eval_metric = "auc"),
               data = dtrain, nround = 100,
               folds = cv_folds, prediction = TRUE, showsd = TRUE,
               early_stopping_rounds = 15, maximize = TRUE, verbose = 0)
  list(Score = cv$evaluation_log$test_auc_mean[cv$best_iteration],
       Pred = cv$pred)
}
OPT_Res <- BayesianOptimization(xgb_cv_bayes,
                                bounds = list(eta = c(1e-09,1),
                                              max_depth = c(2L, 10L),
                                              min_child_weight = c(1L, 10L),
                                              subsample = c(0, 1),
                                              colsample_bytree = c(0,1),
                                              alpha = c(0,1),
                                              lambda = c(0,1)),
                                init_grid_dt = NULL, init_points = 20, n_iter = 50,
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


test$ID <- NULL
test$TransactionID <- NULL
test$DeviceInfo <- NULL
#test_mat <- dummyVars(" ~ .",data = test)
test_mat <-data.frame(predict(dummy_for, newdata = test))


test_mat$isFraudFALSE <- NULL
test_mat$isFraudTRUE <- NULL

dtest <- xgb.DMatrix(as.matrix(test_mat))
pred <- predict(xgbmpdel,dtest)



submission <- fread("../ieee-fraud-detection/sample_submission.csv")
submission[,isFraud := pred]
fwrite(submission,"../ieee-fraud-detection/xgb_submission_v0.1.csv")

