#########
######### Comparison of models taught with different regions

vzhodnaKlas = dataKlas[dataKlas$regija == "vzhodna",]
zahodnaKlas = dataKlas[dataKlas$regija == "zahodna",]

vzhodnaReg = dataReg[dataReg$regija == "vzhodna",]
zahodnaReg = dataReg[dataReg$regija == "zahodna",]

vzhodnaKlas$regija <- NULL
zahodnaKlas$regija <- NULL
vzhodnaReg$regija <- NULL
zahodnaReg$regija <- NULL

set.seed(0)
sel <- sample(1:nrow(vzhodnaKlas), as.integer(nrow(vzhodnaKlas) * 0.7), replace=F)
trainVzKlas <- vzhodnaKlas[sel,]
testVzKlas <- vzhodnaKlas[-sel,]

set.seed(0)
sel <- sample(1:nrow(zahodnaKlas), as.integer(nrow(zahodnaKlas) * 0.7), replace=F)
trainZaKlas <- zahodnaKlas[sel,]
testZaKlas <- zahodnaKlas[-sel,]

set.seed(0)
sel <- sample(1:nrow(vzhodnaReg), as.integer(nrow(vzhodnaReg) * 0.7), replace=F)
trainVzReg <- vzhodnaReg[sel,]
testVzReg <- vzhodnaReg[-sel,]

set.seed(0)
sel <- sample(1:nrow(zahodnaReg), as.integer(nrow(zahodnaReg) * 0.7), replace=F)
trainZaReg <- zahodnaReg[sel,]
testZaReg <- zahodnaReg[-sel,]

##### Classification Models

library(nnet)

obsMatVz <- class.ind(testVzKlas$norm_poraba)
obsMatZa <- class.ind(testZaKlas$norm_poraba)

observedVzKlas <- testVzKlas$norm_poraba
observedZaKlas <- testZaKlas$norm_poraba

### Decision tree
##vzhodna

library(rpart)
dtVz <- rpart(norm_poraba ~ ., data = trainVzKlas)

predictedDtVz <- predict(dtVz, testVzKlas, type="class")
CA(observedVzKlas, predictedDtVz) #CA is 0.5968799
predMatDtVz <- predict(dtVz, testVzKlas, type = "prob")
brier.score(obsMatVz, predMatDtVz) #brier score is 0.5499633

#pruned
dtVz <- rpart(norm_poraba ~ ., data = trainVzKlas,cp=0)
tab <- printcp(dtVz)

row <- which.min(tab[,"xerror"])
th <- mean(c(tab[row, "CP"], tab[row-1, "CP"]))

dtVz <- prune(dtVz, cp=th)

predictedDtVz <- predict(dtVz, testVzKlas, type="class")
CA(observedVzKlas, predictedDtVz) #CA is 0.8096427
predMatDtVz <- predict(dtVz, testVzKlas, type = "prob")
brier.score(obsMatVz, predMatDtVz) #brier score is 0.301578

##zahodna

library(rpart)
dtZa <- rpart(norm_poraba ~ ., data = trainZaKlas)

predictedDtZa <- predict(dtZa, testZaKlas, type="class")
CA(observedZaKlas, predictedDtZa) #CA is 0.6512703
predMatDtZa <- predict(dtZa, testZaKlas, type = "prob")
brier.score(obsMatZa, predMatDtZa) #brier score is 0.4988423

#pruned
dtZa <- rpart(norm_poraba ~ ., data = trainZaKlas,cp=0)
tab <- printcp(dtZa)

row <- which.min(tab[,"xerror"])
th <- mean(c(tab[row, "CP"], tab[row-1, "CP"]))

dtZa <- prune(dtZa, cp=th)

predictedDtZa <- predict(dtZa, testZaKlas, type="class")
CA(observedZaKlas, predictedDtZa) #CA is 0.8682622
predMatDtZa <- predict(dtZa, testZaKlas, type = "prob")
brier.score(obsMatZa, predMatDtZa) #brier score is 0.2092135

###Naive Bayes
##vzhodna

library(e1071)
nbVz <- naiveBayes(norm_poraba ~ ., data = trainVzKlas)

predictedNbVz <- predict(nbVz, testVzKlas, type="class")
CA(observedVzKlas, predictedNbVz) #CA is 0.3199768

predMatNbVz <- predict(nbVz, testVzKlas, type = "raw")
brier.score(obsMatVz, predMatNbVz) #brier score is 0.9380646

##zahodna

library(e1071)
nbZa <- naiveBayes(norm_poraba ~ ., data = trainZaKlas)

predictedNbZa <- predict(nbZa, testZaKlas, type="class")
CA(observedZaKlas, predictedNbZa) #CA is 0.4865907

predMatNbZa <- predict(nbZa, testZaKlas, type = "raw")
brier.score(obsMatZa, predMatNbZa) #brier score is 0.7098367

###Random Forest
##vzhodna

library(randomForest)

rfVz <- randomForest(norm_poraba ~ ., data = trainVzKlas)

predictedRfVz <- predict(rfVz, testVzKlas, type="class")
CA(observedVzKlas, predictedRfVz) #CA is 0.8463098

predMatRfVz <- predict(rfVz, testVzKlas, type = "prob")
brier.score(obsMatVz, predMatRfVz) #brier score is 0.2319066

##zahodna

library(randomForest)

rfZa <- randomForest(norm_poraba ~ ., data = trainZaKlas)

predictedRfZa <- predict(rfZa, testZaKlas, type="class")
CA(observedZaKlas, predictedRfZa) #CA is 0.8872207

predMatRfZa <- predict(rfZa, testZaKlas, type = "prob")
brier.score(obsMatZa, predMatRfZa) #brier score is 0.1721131

##### Regression models

observedVzReg <- testVzReg$poraba
observedZaReg <- testZaReg$poraba

### Linear regression
##vzhodna

linRegVz <- lm(poraba ~ ., trainVzReg)
predictedLinRegVz <- predict(linRegVz, testVzReg)

rmae(observedVzReg, predictedLinRegVz, mean(trainVzReg$poraba)) #0.2453173
rmse(observedVzReg, predictedLinRegVz, mean(trainVzReg$poraba)) #0.08104204

##zahodna

linRegZa <- lm(poraba ~ ., trainZaReg)
predictedLinRegZa <- predict(linRegZa, testZaReg)

rmae(observedZaReg, predictedLinRegZa, mean(trainZaReg$poraba)) #0.149892
rmse(observedZaReg, predictedLinRegZa, mean(trainZaReg$poraba)) #0.02491583

### Regression tree
##vzhodna

library(rpart)

rtVz <- rpart(poraba ~ ., data=trainVzReg)
predictedRtVz <- predict(rtVz, testVzReg)

rmae(observedVzReg, predictedRtVz, mean(trainVzReg$poraba)) #0.3118476
rmse(observedVzReg, predictedRtVz, mean(trainVzReg$poraba)) #0.1207086

#pruned

rtVz <- rpart(poraba ~ ., data=trainVzReg,cp=0)
tab <- printcp(rtVz)
row <- which.min(tab[,"xerror"])
th <- mean(c(tab[row, "CP"], tab[row-1, "CP"]))

rtVz <- prune(rtVz, cp=th)
predictedRtVz <- predict(rtVz, testVzReg)

rmae(observedVzReg, predictedRtVz, mean(trainVzReg$poraba)) #0.135979
rmse(observedVzReg, predictedRtVz, mean(trainVzReg$poraba)) #0.04262494

##zahodna

library(rpart)

rtZa <- rpart(poraba ~ ., data=trainZaReg)
predictedRtZa <- predict(rtZa, testZaReg)

rmae(observedZaReg, predictedRtZa, mean(trainZaReg$poraba)) #0.2169422
rmse(observedZaReg, predictedRtZa, mean(trainZaReg$poraba)) #0.04695856

#pruned

rtZa <- rpart(poraba ~ ., data=trainZaReg,cp=0)
tab <- printcp(rtZa)
row <- which.min(tab[,"xerror"])
th <- mean(c(tab[row, "CP"], tab[row-1, "CP"]))

rtZa <- prune(rtZa, cp=th)
predictedRtZa <- predict(rtZa, testZaReg)

rmae(observedZaReg, predictedRtZa, mean(trainZaReg$poraba)) #0.08804727
rmse(observedZaReg, predictedRtZa, mean(trainZaReg$poraba)) #0.01260737

### KNN
##vzhodna

library(kknn)

knnVz <- kknn(poraba ~ ., trainVzReg, testVzReg, k = 5)
predictedKnnVz <- fitted(knnVz)

rmae(observedVzReg, predictedKnnVz, mean(trainVzReg$poraba)) #0.2399431
rmse(observedVzReg, predictedKnnVz, mean(trainVzReg$poraba)) #0.06332326

##zahodna

library(kknn)

knnZa <- kknn(poraba ~ ., trainZaReg, testZaReg, k = 5)
predictedKnnZa <- fitted(knnZa)

rmae(observedZaReg, predictedKnnZa, mean(trainZaReg$poraba)) #0.1989326
rmse(observedZaReg, predictedKnnZa, mean(trainZaReg$poraba)) #0.03748677
