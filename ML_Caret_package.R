library(caret); library(kernlab); data(spam)

inTrain <- createDataPartition(y=spam$type,
                               p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]


table(spam$type)/dim(spam)[1]
table(training$type)/dim(training)[1]
table(testing$type)/dim(testing)[1]


set.seed(32343)
modelFit <- train(type ~.,data=training, method="glm")
modelFit
modelFit$finalModel

predictions <- predict(modelFit,newdata=testing)
predictions

confusionMatrix(predictions,testing$type)
options(scipen = 999)

#k-folds

set.seed(32323)
folds <- createFolds(y=spam$type,k=10,
                     list=TRUE,returnTrain=TRUE) #returning train set
sapply(folds,length)
folds[[1]][1:10]
table(spam$type[folds[[1]]])/dim(spam)[1]

set.seed(32323)
folds <- createFolds(y=spam$type,k=10,
                     list=TRUE,returnTrain=FALSE) #returning test set
sapply(folds,length)
folds[[1]][450:460]


#Time slices

set.seed(32323)

tme <- 1:1000
folds <- createTimeSlices(y=tme,initialWindow=20,
                          horizon=10)
names(folds)
folds$train[[1]]
folds$test[[1]]

folds$train[[2]]
folds$test[[2]]






