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
df_train <- read_csv('~/Documents/kaggle/bnp-paribas-cardif/input/train.csv')
df_test <- read_csv('~/Documents/kaggle/bnp-paribas-cardif/input/test.csv')

# Look at the dataset
dim(df_train)
names(df_train)
str(df_train)
summary(df_train)

# Convert character attributes to factors
# look at v3
class(df_train$v3)
df_train$v3 <- factor(df_train$v3)
levels(df_train$v3)
summary(df_train$v3)

# convert all character attributes to factors
df_train <- as.data.frame(unclass(df_train))
summary(df_train)

# Most of targets have value 1 (76%)
# Feature v3 has 3 levels. Most are 'C'.
# Feature v10 has only 84 NA values, median 1.313, max 18.534.
  
X_train <- df_train[names(d_train) != 'target']
y_train <- df_train['target']

# Plots ----
# look at the target var
qplot(target, data = df_train, binwidth=.5)

# look at independent vars, features
qplot(v10, data = df_train)
qplot(v10, data = df_train, binwidth = 0.1) + 
  scale_y_log10()

qplot(v1, data = df_train, binwidth=.1) + 
  coord_cartesian(xlim = c(0, 10))

qplot(v71, data = df_train)
table(df_train$v71)
sum(is.na(df_train$v71))

# Predict using xgboost ----
require(xgboost)
require(methods)

labels = df_train['target']
df_train = df_train[-grep('target', colnames(df_train))]

# combine train and test data
df_all = rbind(df_train, df_test)
str(df_all)
table(df_all$v56)
table(df_all$v22)

# one-hot-encoding categorical features
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
write.csv(pred,file='../submit/xgb_submission.csv', quote=FALSE,row.names=FALSE)




