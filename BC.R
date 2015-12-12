# load the iris dataset
rm(list=ls())

# load the library
library(caret)
library(e1071)
require(rpart)

# LOAD THE RTextTools PACKAGE
set.seed(739) # SET THE SEED FOR REPLICABILITY


data(BreastCancer, package="mlbench")
BreastCancer <- na.omit(BreastCancer)
dat <- BreastCancer[-1]
summary(dat)

index <- 1:nrow(dat)
testindex <- sample(index, trunc(length(index)/3))

# Splitting the data into Train and Test sets 
xtest <- dat[testindex,]
xtrain <- dat[-testindex,]

# train the model 
svm.model <- svm(Class ~ ., data = xtrain, cost = 100, gamma = 1, cross = 5)
# make predictions
svm.pred  <- predict(svm.model, xtest[,-10], decision.values = TRUE)
# confusionMatrix(svm.pred, dat$Class)
table(pred = svm.pred, true = xtest[,10])

attributes(svm.model)

cat("Run cross-validated estimation for gamma and cost")

svm.tune=tune(svm, Class~., data = xtrain,
     ranges = list(gamma = 2^(-8:1), cost = 2^(0:4)),
     tunecontrol = tune.control(sampling = "cross", cross =10))
summary(svm.tune)
attributes(svm.tune)
# plot(svm.tune)

plot(svm.tune, transform.x = log10, xlab = expression(log[10](gamma)) , ylab = "Cost")

# Final trained model

bestGamma <- svm.tune$best.parameters[[1]]
bestC <- svm.tune$best.parameters[[2]]
bestM <- svm.tune$best.model[[8]]

final.svm <- svm(Class ~ ., data = xtrain, cost = bestC, gamma = bestGamma, cross = 10)
summary(final.svm)
final.svm

final.svm.pred  <- predict(final.svm, xtest[,-10], decision.values = TRUE)

trainLabels<-xtrain[,10]
testLabels<-xtest[,10]

svm.predtrain<-predict(final.svm,xtrain)
svm.predtest<-predict(final.svm,xtest)

plot(svm.predtrain)
plot(svm.predtest)

confTrain<-table(Predicted=svm.predtrain,Reference=trainLabels)
confTest<-table(Predicted=svm.predtest,Reference=testLabels)

confusionMatrix(confTest,positive='benign')

print(confTrain)
print(confTest)


cat("Confusion matrix for OPTIMAL, cross-validated values:")
table(true=xtest$Class, final.svm.pred)
(acc <- table(final.svm.pred, xtest$Class))
plot(final.svm.pred)

#Using Caret package and Radial Basis Function as kernel for SVM
ctr <- trainControl(method='cv',
                    number=10, 
                    classProbs=TRUE,
                    summaryFunction=twoClassSummary)

svm.c <- train(Class ~., xtrain,
               method='svmRadial',
               trControl=ctr,
               metric="ROC")
svm.c
#Plots the ROC Curve
plot(svm.c)

yhat.c <- predict(svm.c, xtest)

confusionMatrix(yhat.c, dat[testindex,'Class'])










