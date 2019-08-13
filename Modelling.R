#install.packages("MlBayesOpt")
devtools::install_github("ymattu/MlBayesOpt")
library(MlBayesOpt)


train_data <- master_data[ID == "train"]
train_data[,c("TransactionID","ID") := NULL]
setDF(train_data)
train_data$isFraud <- as.factor(train_data$isFraud)

## 75% of the sample size
smp_size <- floor(0.75 * nrow(train_data))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(train_data)), size = smp_size)

rf_1 <- rf_opt(train_data = train_data[train_ind,],
               train_label = isFraud,
               test_data = train_data[-train_ind,],
               test_label = isFraud,
                   acq = "ucb",
                   init_points = 10,
                   n_iter = 20)

train[,c("TransactionID","ID") := NULL]
setDF(train)
train$isFraud <- as.factor(train$isFraud)

x_train <- train
x_train[,"isFraud"] <- NULL

xgb_1 <- xgb_cv_opt(data = train_mat,
                   label = train$isFraud,
                   objectfun = "binary:logistic",
                   evalmetric = "auc",
                   n_folds = 5,
                   acq = "ucb",
                   init_points = 10,
                   n_iter = 5)

