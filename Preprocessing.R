source("functions.R")
pakInstall(c("data.table","ggplot2","zoo"))

### Read Data ###
files <- dir(pattern = ".csv")

for (i in files) {
  file <- fread(i)
  name <- strsplit(i,split = ".",fixed = T)[[1]][1]
  assign(name,file)  
  rm("file")
}

### merge data 

train <- merge(train_transaction,train_identity,all.x = T,by = "TransactionID")
test <- merge(test_transaction,test_identity,all.x = T,by = "TransactionID")

rm(list = setdiff(unlist(strsplit(files,split = ".",fixed = T)),"csv"))

### master data

train[,ID := "train"]
test[,ID := "test"]
test[,isFraud := NA]

master_data <- rbind(train,test)

### EDA ###

str(master_data)

## Counting NAs
train_NA <-sapply(train, function(y) sum(length(which(is.na(y)))))

## Counting Unique Records
train_Unique <-sapply(train, uniqueN)

### Unique text records
sapply(Filter(is.character,train), uniqueN)
sapply(Filter(is.character,train), function(y) sum(length(which(is.na(y)))))

# convert to categorical; can try target encoding
train[,.(.N,sum(isFraud),round(sum(isFraud)/.N,2)),P_emaildomain][order(V3)]
test[,.N,P_emaildomain]

# convert to categorical; can try target encoding
train[,.(.N,sum(isFraud),round(sum(isFraud)/.N,2)),R_emaildomain][order(V3)]
test[,.N,R_emaildomain]

# mostly missing can try splitting up the columns or try any tf_idf features
train[,.(.N,sum(isFraud),round(sum(isFraud)/.N,2)),id_30][order(V3)]
test[,.N,id_30]

train[,.(.N,sum(isFraud),round(sum(isFraud)/.N,2)),id_31][order(V3)]
test[,.N,id_31]

train[,.(.N,sum(isFraud),round(sum(isFraud)/.N,2)),id_33][order(V3)]
test[,.N,id_33]

train[,.(.N,sum(isFraud),round(sum(isFraud)/.N,2)),DeviceInfo][order(V3)]
test[,.N,DeviceInfo]

### convert to factors
fac_cols <- names(Filter(is.character,train))
#fac_cols <- setdiff(fac_cols,"DeviceInfo")
master_data[,(fac_cols):= lapply(.SD, as.factor), .SDcols = fac_cols]
train[,(fac_cols):= lapply(.SD, as.factor), .SDcols = fac_cols]

str(master_data)


### save files ###

save(train,test,master_data,file = "processed_data.rda")
