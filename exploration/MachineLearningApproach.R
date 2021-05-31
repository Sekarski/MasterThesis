#test
source("AuxFunctions.R")

library(tidyverse)
library(lubridate)

tsla_price <- read.csv("../data/data_h_corrected.csv")
tsla_trend <- read.csv("../data/GT_tsla_h.csv",col.names=c("Date","tsla"))
musk_trend <- read.csv("../data/GT_musk_h.csv",col.names=c("Date","musk"))

tsla_price <- data.frame(apply(tsla_price[1:1],2,ymd_hms),tsla_price[2:2])
tsla_trend <- data.frame(apply(tsla_trend[1:1],2,ymd_hms),tsla_trend[2:2])
musk_trend <- data.frame(apply(musk_trend[1:1],2,ymd_hms),musk_trend[2:2])

# crop trend data to match price data start en end
start_ts <- tsla_price$Date[[1]]
end_ts <- tsla_price$Date[[length(tsla_price$Date)]]

tsla_trend <- tsla_trend[which(tsla_trend$Date>=start_ts & tsla_trend$Date<=end_ts),]
musk_trend <- musk_trend[which(musk_trend$Date>=start_ts & musk_trend$Date<=end_ts),]

data <- merge(tsla_price,tsla_trend,by="Date")
data <- merge(data,musk_trend,by="Date")

#perfect it already dropped the NAs



returns <- convert_to_logret(tsla_price$Open)
returns <- c(NA,returns)
data$returns <- returns

list_shifts = seq(from=-35*4,to=35*4)

data <- shift(data,'returns',list_shifts)

data <- data %>% drop_na()

#train/test cut index:
cut_date = ymd_h("2021-01-04 10")
cut_date_numeric <- as.numeric(cut_date)
cut_idx <- which(data$Date==cut_date_numeric)


#split into features and responses
features_list <- c('tsla','musk','Date')
responses_list <- c('returns')

for (i in seq(from=-35*4,to=-1)) {
  features_list <- c(features_list,paste('returns',toString(i)))
}
for (i in seq(from=1,to=35*4)) {
  responses_list <- c(responses_list,paste('returns',toString(i)))
}

features <- data[features_list]
responses <- data[responses_list]

#split into training and test
temp <- split_data(features,cut_idx)
x_train <-temp$train
x_test <- temp$test

temp <- split_data(responses,cut_idx)
y_train <- temp$train
y_test <- temp$test

qplot(Date,returns,data=data)
qplot(Date,Open,data=data)
#let's try some ML now :D
#library(caret)
#model_lm = train(x_train,y_train)
#doesn't work because i'm trying to do multivariate output...

test <- nnet::nnet(x=x_train,y=y_train,size=50,MaxNWts=50000)
y_predict <- predict(test,x_test[1,])
#only getting zeros...

#new try:
library(keras)
xtrain <- as.matrix(x_train)
ytrain <- as.matrix(y_train)
xtest <- as.matrix(x_test)
ytest <- as.matrix(y_test)

in_dim <- dim(xtrain)[2]
out_dim <- dim(ytrain)[2]

model <- keras_model_sequential() %>%
  layer_dense(units=100,activation="relu",input_shape=in_dim) %>%
  layer_dense(units=32,activation="relu") %>%
  layer_dense(units=out_dim,activation="linear")

model %>% compile(
  loss="mse",
  optimizer="adam")

model %>% summary()
#also not working. IT uses python and tensorflow apparently

#new try n°2:
library(MultivariateRandomForest)
n_tree=2
m_feature=5
min_leaf=5
ypredict=build_forest_predict(xtrain,ytrain,n_tree,m_feature,min_leaf,xtest)
