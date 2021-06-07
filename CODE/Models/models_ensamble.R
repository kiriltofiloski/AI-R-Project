#############################################
##VOTING, WEIGHTED VOTING, BAGGING, BOOSTING#
############################################

library(rpart.plot)
#Decision tree, naive bayes, knn
modelDT <- CoreModel(norm_poraba ~ ., train, model="tree")
modelNB <- CoreModel(norm_poraba ~ ., train, model="bayes")
modelKNN <- CoreModel(norm_poraba ~ ., train, model="knn", kInNN = 10)

predDT <- predict(modelDT, test, type = "class")
caDT <- CA(test$norm_poraba, predDT)
caDT
#caDT = 0.8274


predNB <- predict(modelNB, test, type="class")
caNB <- CA(test$norm_poraba, predNB)
caNB
#caNB = 0.44301

predKNN <- predict(modelKNN, test, type="class")
caKNN <- CA(test$norm_poraba, predKNN)
caKNN
#caKNN = 0.51062


#
#VECINSKI KLASIFIKATOR CA = 0.36983
#
table(test$norm_poraba)
sum(test$norm_poraba == "SREDNJA") / length(test$norm_poraba)

#
#
#
library(nnet)
observedMat <-class.ind(test$norm_poraba)

p0 <- table(train$norm_poraba)/nrow(train)
p0

p0Mat <- matrix(rep(p0, times=nrow(test)), nrow = nrow(test), byrow=T)
colnames(p0Mat) <- names(p0)

brier.score(observedMat, p0Mat)
##vecinski klasifikator brier= 0.7494

inf.score(train$norm_poraba, test$norm_poraba, p0Mat)
##vecinski klasifikator inf.score = 0, we didn't learn anything new


#
#VOTING
#
pred <- data.frame(predDT, predNB, predKNN)
voting <- function(predictions)
{
  res <- vector()
  
  for (i in 1 : nrow(predictions))  	
  {
    vec <- unlist(predictions[i,])
    res[i] <- names(which.max(table(vec)))
  }
  
  res
}

predClass <- voting(pred)
head(predClass)

predicted <- factor(predClass, levels=levels(train$norm_poraba))
head(predicted)

CA(test$norm_poraba, predicted)
#CA = 0.64 which is lower than the CA of the decision tree



#
# UTEZENO GLASOVANJE ######
#
predDT.prob <- predict(modelDT, test, type="prob")
predNB.prob <- predict(modelNB, test, type="prob")
predKNN.prob <- predict(modelKNN, test, type="prob")

predProb <- predDT.prob + predNB.prob + predKNN.prob
head(predProb)
head(max.col(predProb))

predClass <- colnames(predProb)[max.col(predProb)]
predicted <- factor(predClass, levels(data$norm_poraba))
head(predicted)

CA(test$norm_poraba, predicted)
#CA = 0.78 which is still lower than the decision tree
brier.score(observedMat, predProb)
#brier=1.78008, fails because the probabilites are higher than 1.




#
# BAGGING
#
library(ipred)

bag <- bagging(norm_poraba ~ ., train, nbagg=35)
predicted <- predict(bag, test, type="class")
predMat_Bag <- predict(bag, test, type = "prob")
CA(test$norm_poraba, predicted)
#CA = 0.85994 which is the best off all.
inf.score(train$norm_poraba, test$norm_poraba, predMat_Bag$prob)
#inf.score = 1.09409
brier.score(observedMat, predMat_Bag$prob)
#brier = 0.6865



#Regression
bagR <- bagging(poraba~., trainR, nbagg=35)
predictedR <- predict(bagR, testR)
observedR <- testR$poraba
rmse(observedR, predictedR, mean(trainR$poraba))
#rmse=0.1494



#
#BOOSTING
#
library(adabag)
bm <- boosting(norm_poraba ~ ., train, mfinal=100)
predictionsBoost <- predict(bm, test)
names(predictionsBoost)


predictedBoost <- predictionsBoost$class
CA(test$norm_poraba, predictedBoost)
#CA = 0.6369
library(nnet)

brier.score(observedMat, predictionsBoost$prob)
#BRIER. score is 0.5346 -> higher than bagging

inf.score(train$norm_poraba, test$norm_poraba, predictionsBoost$prob)
#inf.score = 1.15814 -> higher than bagging

