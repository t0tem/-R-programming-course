#######
#  1  #
#######

library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

vowel.train$y <- factor(vowel.train$y)
vowel.test$y <- factor(vowel.test$y)
set.seed(33833)

library(caret)
modFit1 <- train(y ~ ., data = vowel.train, method = "rf")
modFit2 <- train(y ~ ., data = vowel.train, method = "gbm")

pred1 <- predict(modFit1, newdata = vowel.test)
pred2 <- predict(modFit2, newdata = vowel.test)

confusionMatrix(pred1, vowel.test$y)$overall[1]
confusionMatrix(pred2, vowel.test$y)$overall[1]

agrred_index <- pred1 == pred2
agreed <- pred1[pred1 == pred2]

confusionMatrix(agreed, vowel.test$y[agrred_index])$overall[1]

#######
#  2  #
#######

library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

set.seed(62433)
fit_rf <- train(diagnosis ~ ., data = training, method = "rf")
fit_gbm <- train(diagnosis ~ ., data = training, method = "gbm")
fit_lda <- train(diagnosis ~ ., data = training, method = "lda")

pred_rf <- predict(fit_rf, testing)
pred_gbm <- predict(fit_gbm, testing)
pred_lda <- predict(fit_lda, testing)

predDF <- data.frame(pred_rf, pred_gbm, pred_lda, diagnosis = testing$diagnosis)

fit_combo <- train(diagnosis ~ ., data = predDF, method = "rf")
pred_combo <- predict(fit_combo, predDF)

confusionMatrix(pred_rf, testing$diagnosis)$overall[1]
confusionMatrix(pred_gbm, testing$diagnosis)$overall[1]
confusionMatrix(pred_lda, testing$diagnosis)$overall[1]
confusionMatrix(pred_combo, testing$diagnosis)$overall[1]

#######
#  3  #
#######

set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(233)

fit_lasso <- train(CompressiveStrength ~ ., data = training, method = "lasso")
library(elasticnet)

fit_lasso$finalModel
plot.enet(fit_lasso$finalModel, xvar = "penalty", use.color = TRUE)


#######
#  4  #
#######
url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv"
download.file(url, "data/gaData.csv")

library(lubridate) # For year() function below

dat = read.csv("data/gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)

library(forecast)

fit <- bats(tstrain)
plot(forecast(fit))

pred <- forecast(fit, h = 235, level = 95)
total_ok <- sum(
    (pred$lower <= testing$visitsTumblr) & (testing$visitsTumblr <= pred$upper))
total_ok / length(testing$visitsTumblr)




#######
#  5  #
#######


set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(325)
library(e1071)
fit <- svm(CompressiveStrength ~ ., data = training)
pred <- predict(fit, testing)

sqrt(
    sum(
        (pred - testing$CompressiveStrength)^2
        )/256
    )

#or just
accuracy(pred, testing$CompressiveStrength)[2]



