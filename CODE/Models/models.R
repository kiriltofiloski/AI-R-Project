# we remove the date('datum') attribute
dataKlas$datum <- NULL
dataReg$datum <- NULL

#first we make training and test sets

set.seed(0)
sel <- sample(1:nrow(dataKlas), as.integer(nrow(dataKlas) * 0.7), replace=F)
train <- dataKlas[sel,]
test <- dataKlas[-sel,]

#we get the class indicator to estimate the probability of predictions

library(nnet)
obsMat <- class.ind(test$norm_poraba)

#we also need the correct test classes for calculating classification accuracy later

observed <- test$norm_poraba


# classification accuracy function
CA <- function(observed, predicted)
{
	mean(observed == predicted)
}

# brier score function
brier.score <- function(observedMatrix, predictedMatrix)
{
	sum((observedMatrix - predictedMatrix) ^ 2) / nrow(predictedMatrix)
}


######################################
###MODELS WITHOUT FILTERED ATTRIBUTES##
#####################################

### 1st model - decision tree

library(rpart)
dt <- rpart(norm_poraba ~ ., data = train)

#we now use the model for prediction and test it

predictedDt <- predict(dt, test, type="class")
CA(observed, predictedDt) #CA is 0.6025716
predMatDt <- predict(dt, test, type = "prob")
brier.score(obsMat, predMatDt) #brier score is 0.5515094

#we can try pruning the tree for better results
#we first create a large tree, which while building the tree internally evaluates the atributes qualities

dt <- rpart(norm_poraba ~ ., data=train, cp=0)
tab <- printcp(dt)

#we select the cp parameter which corresponds with the minimal internal error (ova nez tocno preobjasni go pokasno, ne e ubo napisano)

row <- which.min(tab[,"xerror"])
th <- mean(c(tab[row, "CP"], tab[row-1, "CP"]))
th #3.062069e-05

#then we prune the tree with the selected setting

dt <- prune(dt, cp=th)

#we test it again

predictedDt <- predict(dt, test, type="class")
CA(observed, predictedDt) #CA is 0.8327489

predMatDt <- predict(dt, test, type = "prob")
brier.score(obsMat, predMatDt) #brier score is 0.2625211

### 2nd model - Naive Bayes

library(e1071)
nb <- naiveBayes(norm_poraba ~ ., data = train)

predictedNb <- predict(nb, test, type="class")
CA(observed, predictedNb) #CA is 0.3791805

predMatNb <- predict(nb, test, type = "raw")
brier.score(obsMat, predMatNb) #brier score is 0.8361006

### 3rd model - Random Forest

library(randomForest)

rf <- randomForest(norm_poraba ~ ., data = train)

predictedRf <- predict(rf, test, type="class")
CA(observed, predictedRf) #CA is 0.8672641

predMatRf <- predict(rf, test, type = "prob")
brier.score(obsMat, predMatRf) #brier score is 0.1995532

##### Regression models

set.seed(0)
sel <- sample(1:nrow(dataReg), as.integer(nrow(dataReg) * 0.7), replace=F)
trainReg <- dataReg[sel,]
testReg <- dataReg[-sel,]

observedReg <- testReg$poraba

#Trivial model that tells us the average value of our target variable(neznam dal bitno)

meanVal <- mean(trainReg$poraba)
meanVal

predTrivial <- rep(meanVal, nrow(testReg))

mae(observedReg, predTrivial)
mse(observedReg, predTrivial)

# srednja absolutna napaka
mae <- function(obs, pred)
{
	mean(abs(obs - pred))
}

# srednja kvadratna napaka
mse <- function(obs, pred)
{
	mean((obs - pred)^2)
}

# relativna srednja absolutna napaka
rmae <- function(obs, pred, mean.val) 
{  
	sum(abs(obs - pred)) / sum(abs(obs - mean.val))
}

# relativna srednja kvadratna napaka
rmse <- function(obs, pred, mean.val) 
{  
	sum((obs - pred)^2)/sum((obs - mean.val)^2)
}

### 1st Model - Linear Regression

linReg <- lm(poraba ~ ., trainReg)
predictedLinReg <- predict(linReg, testReg)

mae(observedReg, predictedLinReg) #30.45863
mse(observedReg, predictedLinReg) #4252.676
rmae(observedReg, predictedLinReg, mean(trainReg$poraba)) #0.201877
rmse(observedReg, predictedLinReg, mean(trainReg$poraba)) #0.06699659

### 2nd Model - Regression tree

library(rpart)

rt <- rpart(poraba ~ ., data=trainReg)
predictedRt <- predict(rt, testReg)

mae(observedReg,predictedRt) #42.22185
rmae(observedReg, predictedRt, mean(trainReg$poraba)) #0.2798425

#we can prune the tree the same as in classification models

rt <- rpart(poraba ~ ., data=trainReg,cp=0)
tab <- printcp(rt)
row <- which.min(tab[,"xerror"])
th <- mean(c(tab[row, "CP"], tab[row-1, "CP"]))

rt <- prune(rt, cp=th)
predictedRt <- predict(rt, testReg)

mae(observedReg,predictedRt) #18.07195
rmae(observedReg, predictedRt, mean(trainReg$poraba)) #0.1197792

### 3rd Model - SVM

library(e1071)

svmReg <- svm(poraba ~ ., trainReg)
predictedSvmReg <- predict(svmReg, testReg)

mae(observedReg,predictedSvmReg) #20.66351
rmae(observedReg, predictedSvmReg, mean(trainReg$poraba)) #0.1369558







