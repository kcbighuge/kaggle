# BNP Paribas Cardif Claims Management
# https://www.kaggle.com/c/bnp-paribas-cardif-claims-management

library(ggplot2)
library(gridExtra)
library(psych)
library(dplyr)
library(scales)
library(memisc)
library(readr)

setwd('~/Documents/kaggle/bnp-paribas-cardif/')
train_file <- '/input/train.csv'
d_train <- read_csv('~/Documents/kaggle/bnp-paribas-cardif/input/train.csv')
d_test <- read_csv('~/Documents/kaggle/bnp-paribas-cardif/input/test.csv')

# Look at the dataset
dim(d_train)
names(d_train)
str(d_train)
summary(d_train)

# Plots ----
# look at the target var
qplot(target, data = d_train, binwidth=.5)
# look at independent vars, features
qplot(v1, data = d_train, binwidth=.1) + 
  coord_cartesian(xlim = c(0, 10))

qplot(v71, data = d_train)
table(d_train$v71)
sum(is.na(d_train$v71))

# Predict using xgboost ----
require(xgboost)
require(methods)

labels = d_train['target']
df_train = d_train[-grep('target', colnames(d_train))]

# combine train and test data
df_all = rbind(df_train, d_test)
str(df_all)

# one-hot-encoding categorical features
str(df_all)
ohe_feats = c('v3', 'v22', 'v24', 'v30', 'v31', 'v47', 'v52', 'v56', 'v66',
              'v71', 'v74', 'v75', 'v79', 'v91')

# use subset of numeric features
df_train[1,5:22]

# see how many null values
for (i in 5:22) {
  print(sum(is.na(df_train[,i])))
}

# replace nulls
X <- df_train[, 5:22]
X[is.na(X)] <- 0
df_train[,5:22] <- X
str(X)

model <- xgboost(data=X, label=labels, nrounds=50,
                 objective='binary:logistic')

# Make prediction ----
pred = predict(bst,x[teind,])
pred = matrix(pred,9,length(pred)/9)
pred = t(pred)

# Output submission
pred = format(pred, digits=2,scientific=F) # shrink the size of submission
pred = data.frame(1:nrow(pred),pred)
names(pred) = c('id', paste0('Class_',1:9))
write.csv(pred,file='submission.csv', quote=FALSE,row.names=FALSE)




