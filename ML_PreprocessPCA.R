
library(caret); library(kernlab); data(spam)
inTrain <- createDataPartition(y=spam$type,
                               p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]

M <- abs(cor(training[,-58]))
diag(M) <- 0 #cor on diagonal is always 1, so make it 0
which(M > 0.8, arr.ind=TRUE)
names(spam)[c(34,32)]
plot(spam[,34],spam[,32])


##Basic PCA idea
# We might not need every predictor
# A weighted combination of predictors might be better
# We should pick this combination to capture the "most information" possible
# Benefits
#   Reduced number of predictors
#   Reduced noise (due to averaging)


X <- 0.71*training$num415 + 0.71*training$num857
Y <- 0.71*training$num415 - 0.71*training$num857
plot(X,Y)


## Preprocessing with PCA

preProc <- preProcess(log10(training[,-58]+1),method="pca",pcaComp=2) 
#<- log is useful against outliers and skewed data

trainPC <- predict(preProc,log10(training[,-58]+1))
modelFit <- train(training$type ~ .,method="glm",data=trainPC) #doesn't work
modelFit <- train(x = trainPC, y = training$type,method="glm") # works
modelFit

#predicting
testPC <- predict(preProc,log10(testing[,-58]+1)) #preProc is made from training (!)
confusionMatrix(testing$type,predict(modelFit,testPC))

# Alternative - pca is inside train func directly

modelFit <- train(type ~ ., method="glm", preProcess="pca", data=training)
modelFit
confusionMatrix(testing$type,predict(modelFit,testing))




