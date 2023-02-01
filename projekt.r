install.packages("readxl")
library(readxl)
my_data <- read_excel(file.choose("kohkiloyeh.xlsx"))

#naivebayes

library(naivebayes)
dane=my_data
head(dane)
xtabs(~topic, dane)
str(dane)
length(dane$topic)

idx=sample(2,nrow(dane),replace=T,prob=c(0.8,0.2))
train=dane[idx==1,]
test=dane[idx==2,]

model=naive_bayes(topic ~ .,data=train, usekernel = TRUE)

plot(model)

p=predict(model,test)

cf=table(p,test$topic)
cf

pcf=cf/sum(cf)
pcf

head(dane)
xtabs(~topic, dane)
str(dane)
length(dane$topic)

idx=sample(2,nrow(dane),replace=T,prob=c(0.7,0.3))
train=dane[idx==1,]
test=dane[idx==2,]

model=naive_bayes(topic ~ .,data=train, usekernel = TRUE)

plot(model)

p=predict(model,test)

cf=table(p,test$topic)
cf

pcf=cf/sum(cf)
pcf

head(dane)
xtabs(~topic, dane)
str(dane)
length(dane$topic)

idx=sample(2,nrow(dane),replace=T,prob=c(0.9,0.1))
train=dane[idx==1,]
test=dane[idx==2,]

model=naive_bayes(topic ~ .,data=train, usekernel = TRUE)

plot(model)

p=predict(model,test)

cf=table(p,test$topic)
cf

pcf=cf/sum(cf)
pcf

sd(p)

#random forest

my_data <- read_excel(file.choose("kohkiloyeh.xlsx"))

install.packages("randomForest")

library(randomForest)
library(datasets)

dane=my_data
str(my_data)
df=dane

dane$topic <- as.factor(dane$topic)
table(dane$topic)

set.seed(222)
ind <- sample(2, nrow(dane), replace = TRUE, prob = c(0.7, 0.3))
train <- dane[ind==1,]
test <- dane[ind==2,]

rf <- randomForest(topic~., dane=train, proximity=TRUE) print(rf)
randomForest(formula = topic ~ ., data = train)

plot(dane)

library(randomForest)
library(datasets)

dane=my_data
str(my_data)
df=dane

dane$topic <- as.factor(dane$topic)
table(dane$topic)

set.seed(222)
ind <- sample(2, nrow(dane), replace = TRUE, prob = c(0.8, 0.2))
train <- dane[ind==1,]
test <- dane[ind==2,]

rf <- randomForest(topic~., dane=train, proximity=TRUE) print(rf)
randomForest(formula = topic ~ ., data = train)

plot(dane)


library(randomForest)
library(datasets)

dane=my_data
str(my_data)
df=dane

dane$topic <- as.factor(dane$topic)
table(dane$topic)

set.seed(222)
ind <- sample(2, nrow(dane), replace = TRUE, prob = c(0.9, 0.1))
train <- dane[ind==1,]
test <- dane[ind==2,]

rf <- randomForest(topic~., dane=train, proximity=TRUE) print(rf)
randomForest(formula = topic ~ ., data = train)

plot(dane)


#nn

install.packages("neuralnet")
install.packages("plyr")
install.packages("readr")
install.packages("dplyr")
install.packages("caret")

library(plyr)
library(readr)
library(dplyr)
library(caret)
library(neuralnet)
library(nnet)
library(caTools)

my_data <- read_excel(file.choose("kohkiloyeh.xlsx"))

glimpse(my_data)
dane=my_data
df=dane

set.seed(100)

spl = sample.split(my_data$topic, SplitRatio = 0.7)
train = subset(my_data, spl==TRUE)
test = subset(my_data, spl==FALSE)

print(dim(train)); print(dim(test))

train_params <- trainControl(method = "repeatedcv", number = 10, repeats=5)

nnet_model <- train(train[,-6], train$topic,
                    method = "nnet",
                    trControl= train_params,
                    preProcess=c("scale","center"),
                    na.action = na.omit
)

prop.table(table(train$topic))   #Baseline Accuracy

# Predictions on the training set
nnet_predictions_train <-predict(nnet_model, train)

# Confusion matrix on training data
table(train$topic, nnet_predictions_train)
(69)/nrow(train)                    

#Predictions on the test set
nnet_predictions_test <-predict(nnet_model, test)

# Confusion matrix on test set
table(test$topic, nnet_predictions_test)
31/nrow(test)  

plot(dane)


glimpse(my_data)
dane=my_data
df=dane

set.seed(100)

spl = sample.split(my_data$topic, SplitRatio = 0.8)
train = subset(my_data, spl==TRUE)
test = subset(my_data, spl==FALSE)

print(dim(train)); print(dim(test))

train_params <- trainControl(method = "repeatedcv", number = 10, repeats=5)

nnet_model <- train(train[,-6], train$topic,
                    method = "nnet",
                    trControl= train_params,
                    preProcess=c("scale","center"),
                    na.action = na.omit
)

prop.table(table(train$topic))   #Baseline Accuracy

# Predictions on the training set
nnet_predictions_train <-predict(nnet_model, train)

# Confusion matrix on training data
table(train$topic, nnet_predictions_train)
(69)/nrow(train)                    

#Predictions on the test set
nnet_predictions_test <-predict(nnet_model, test)

# Confusion matrix on test set
table(test$topic, nnet_predictions_test)
31/nrow(test)  

plot(dane)



glimpse(my_data)
dane=my_data
df=dane

set.seed(100)

spl = sample.split(my_data$topic, SplitRatio = 0.9)
train = subset(my_data, spl==TRUE)
test = subset(my_data, spl==FALSE)

print(dim(train)); print(dim(test))

train_params <- trainControl(method = "repeatedcv", number = 10, repeats=5)

nnet_model <- train(train[,-6], train$topic,
                    method = "nnet",
                    trControl= train_params,
                    preProcess=c("scale","center"),
                    na.action = na.omit
)

prop.table(table(train$topic))   #Baseline Accuracy

# Predictions on the training set
nnet_predictions_train <-predict(nnet_model, train)

# Confusion matrix on training data
table(train$topic, nnet_predictions_train)
(69)/nrow(train)                    

#Predictions on the test set
nnet_predictions_test <-predict(nnet_model, test)

# Confusion matrix on test set
table(test$topic, nnet_predictions_test)
31/nrow(test)  

plot(dane)

