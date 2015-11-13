# Quiz 3
#1
library(AppliedPredictiveModeling)
data("segmentationOriginal")
library(caret)

train = subset(segmentationOriginal, Case=="Train")
test = subset(segmentationOriginal, Case=="Test")

set.seed(125)

model<-train(Class~.,data=train,method="rpart")
model$finalModel

#garbage below
# The following indicate that none of them are possible to predict. Hmmmmmm
foo <- train[1,]
foo$TotalIntenCh1 = 23000
foo$FiberWidthCh1 = 10
foo$PerimStatusCh1 = 2
predict(model, foo)

foo$TotalIntenCh1 = 50000
foo$FiberWidthCh1 = 10
foo$VarIntenCh4 = 100
predict(model, foo)

foo$TotalIntenCh1 = 57000
foo$FiberWidthCh1 = 8
foo$VarIntenCh4 = 100
predict(model, foo)

foo$FiberWidthCh1 = 8
foo$VarIntenCh4 = 100
foo$PerimStatusCh1 = 2
predict(model, foo)

# 3
library(pgmm)
data(olive)
olive = olive[,-1]

oliveModel <- train(Area~.,data=olive,method="rpart")
newdata = as.data.frame(t(colMeans(olive)))
predict(oliveModel,newdata)

# 4
library(ElemStatLearn)
data(SAheart)
#SAheart$chd<-as.factor(SAheart$chd)
set.seed(8484)
train = sample(1:dim(SAheart)[1], size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)
logreg <- train(chd~age+alcohol+obesity+tobacco+typea+ldl,data=trainSA, family="binomial", method="glm")

missClass = function(values,prediction){sum(((prediction>0.5)*1) != values)/length(values)}

trainPredict <- predict(logreg, trainSA[-10])
testPredict <- predict(logreg, testSA[-10])

missClass(trainSA$chd, trainPredict)
missClass(testSA$chd, testPredict)

# 5

library(ElemStatLearn)
data("vowel.train")
data("vowel.test")

vowel.test$y <- as.factor(vowel.test$y)
vowel.train$y <- as.factor(vowel.train$y)
set.seed(33833)
forest <- train(y~.,data=vowel.train,method="rf")
library(caret)
varImp(forest)
