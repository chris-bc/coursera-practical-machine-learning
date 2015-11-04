## Question 3

library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis,p=3/4)[[1]]
training = adData[inTrain,]
testing = adData[-inTrain,]

# Find all predictor variables that begin with IL
il_vars<-grep("^IL",names(training),value=TRUE)

# Perform principle components on these variables with the preProcess() function
# Calculate the number of principle components needed to capture 90% of the variance
model <- preProcess(training[,il_vars], method="pca", thresh=0.9)
# How many are there?
## 9



## Question 4
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData=data.frame(diagnosis,predictors[,grep("^IL",names(predictors))])
inTrain=createDataPartition(adData$diagnosis,p=3/4)[[1]]
training=adData[inTrain,]
testing=adData[-inTrain,]

# Create a training data set consisting of only the predictors with variable name beginning with IL
#il_vars<-grep("^IL|diagnosis",names(training))

# Build 2 predictivy models, one using predictors as they are and one using PCA with principle components explaining 80% of the variance in the predictors
# Use method= glm in the train function
#dfA <- training[,il_vars]
modelA = train(diagnosis~.,method="glm",data=training)
testA <- confusionMatrix(testing[,1], predict(modelA, testing[,-1]))

modelB = train(diagnosis~.,method="glm",data=training,preProcess="pca",Control=trainControl(preProcOptions = list(thresh=0.8)))
modelB1<-preProcess(training[,-1],method="pca",thresh=0.8)
trainingB1<-predict(modelB1,training[,-1])
testingB1<-predict(modelB1,testing[,-1])
modelB2 <- train(training$diagnosis~.,method="glm",data=trainingB1)
modelBResult<-confusionMatrix(testing[,1],predict(modelB2, testingB1))


# What is the accuracy of each method in test set? Which is more accurate?