#####
# 1 #
#####

library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)

training <- subset(segmentationOriginal, Case == "Train")

set.seed(125)
fit <- train(Class ~ ., data = training, method = "rpart")
fit$finalModel

library(rattle)
library(RColorBrewer)
library(rpart.plot)

options(scipen = 999)

fancyRpartPlot(fit$finalModel)

#####
# 3 #
#####

library(pgmm)
data(olive)
olive = olive[,-1]
fit <- train(Area ~ ., data = olive, method = "rpart")
fit$finalModel
fancyRpartPlot(fit$finalModel)
predict(fit, newdata = as.data.frame(t(colMeans(olive))))


#####
# 4 #
#####

library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)
fit <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl,
             data = trainSA, method = "glm", family = "binomial")

missClass = function(values,prediction){ 
    sum(((prediction > 0.5)*1) != values)/length(values)
    }

missClass(trainSA$chd, predict(fit, trainSA))
missClass(testSA$chd, predict(fit, testSA))

#####
# 5 #
#####

library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

vowel.train$y <- factor(vowel.train$y)
vowel.test$y <- factor(vowel.test$y)

set.seed(33833)
fit <- randomForest(y ~ ., data = vowel.train)

order(varImp(fit), decreasing = TRUE)


