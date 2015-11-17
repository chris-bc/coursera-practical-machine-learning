# Quiz 4

# question 1
library(caret)
library(ElemStatLearn)
data("vowel.train")
data("vowel.test")

vowel.train$y = as.factor(vowel.train$y)
vowel.test$y = as.factor(vowel.test$y)
set.seed(33833)
forest = train(y~., data = vowel.train, method="rf")
boost = train(y~., data = vowel.train, method="gbm")

fPredict = predict(forest, vowel.test[-1])
bPredict = predict(boost, vowel.test[-1])

fAccu = vowel.test$y == fPredict
bAccu = vowel.test$y == bPredict
mean(fAccu)
mean(bAccu)

mean(fAccu == bAccu)

df_comb <- data.frame(fPredict,bPredict,y=vowel.test$y,agree=fPredict==bPredict)
sum(fPredict[df_comb$agree] == df_comb$y[df_comb$agree])/sum(df_comb$agree)


# Q2
library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[inTrain,]
testing = adData[-inTrain,]

set.seed(62433)
forest<-train(diagnosis~.,data=training,method="rf")
boost<-train(diagnosis~.,data=training,method="gbm")
linear<-train(diagnosis~.,data=training,method="lda")
fPredict <- predict(forest, testing)
bPredict <- predict(boost, testing)
lPredict <- predict(linear, testing)
combined<-data.frame(fPredict,bPredict,lPredict,diagnosis=testing$diagnosis)
combinedModel <- train(diagnosis~., data=combined, method="rf")
cPredict <- predict(combinedModel, testing)
confusionMatrix(fPredict, testing$diagnosis)
confusionMatrix(bPredict, testing$diagnosis)
confusionMatrix(lPredict, testing$diagnosis)
confusionMatrix(cPredict, testing$diagnosis)

#q3
set.seed(3523)
library(AppliedPredictiveModeling)
data("concrete")
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain, ]
testing = concrete[ -inTrain, ]
set.seed(233)
concreteModel <- train(CompressiveStrength~., data=training, method="lasso")

#q4
download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv",destfile="gaData.csv",method="libcurl")
library(lubridate)
dat = read.csv("gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)

library(forecast)
# TODO


# Q5
set.seed(3523)
library(AppliedPredictiveModeling)
data("concrete")
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[inTrain,]
testing = concrete[-inTrain,]
set.seed(325)
library(e1071)
library(caret)
mySVM<-svm(CompressiveStrength~., data=training)
prediction<-predict(mySVM,testing)
sqrt(mean((prediction - testing$CompressiveStrength)^2))