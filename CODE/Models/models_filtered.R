#######
####### Models with chosen highest impact attributes

##### Classification Models

# The highest impact classification attributes based on relieff are: namembnost, leto_izgradnje, povrsina and stavba.

### Decision tree

library(rpart)
dtReduced <- rpart(norm_poraba ~ namembnost + leto_izgradnje + povrsina + stavba, data = train)

predictedDtReduced <- predict(dtReduced, test, type="class")
CA(observed, predictedDtReduced) #CA is 0.4679038
predMatDtReduced <- predict(dtReduced, test, type = "prob")
brier.score(obsMat, predMatDtReduced) #brier score is 0.6710721

#pruned

dtReduced <- rpart(norm_poraba ~ namembnost + leto_izgradnje + povrsina + stavba, data=train, cp=0)
tab <- printcp(dtReduced)

row <- which.min(tab[,"xerror"])
th <- mean(c(tab[row, "CP"], tab[row-1, "CP"]))
th #4.597701e-05

dtReduced <- prune(dtReduced, cp=th)

predictedDtReduced <- predict(dtReduced, test, type="class")
CA(observed, predictedDtReduced) #CA is 0.6436782
predMatDtReduced <- predict(dtReduced, test, type = "prob")
brier.score(obsMat, predMatDtReduced) #brier score is 0.473547

### Naive Bayes

library(e1071)
nbReduced <- naiveBayes(norm_poraba ~ namembnost + leto_izgradnje + povrsina + stavba, data = train)

predictedNbReduced <- predict(nbReduced, test, type="class")
CA(observed, predictedNbReduced) #CA is 0.3779629

predMatNbReduced <- predict(nbReduced, test, type = "raw")
brier.score(obsMat, predMatNbReduced) #brier score is 0.7329174

### Random Forest

library(randomForest)

rfReduced <- randomForest(norm_poraba ~ namembnost + leto_izgradnje + povrsina + stavba, data = train)

predictedRfReduced <- predict(rfReduced, test, type="class")
CA(observed, predictedRfReduced) #CA is 0.6442301

predMatRfReduced <- predict(rfReduced, test, type = "prob")
brier.score(obsMat, predMatRfReduced) #brier score is 0.6580415

##### Regression Models

# The highest impact regression attributes based on relieff are: max_por_prejsni_dan , pov_por_prejsni_dan, min_por_prejsni_dan, povrsina
# we will use max_por_prejsni_dan + povrsina + ura + pritisk beacue one attribute related to previous day poraba is enough

### Linear Regression

linRegReduced <- lm(poraba ~ max_por_prejsni_dan + povrsina + ura + pritisk, trainReg)
predictedLinRegReduced <- predict(linRegReduced, testReg)

mae(observedReg, predictedLinRegReduced) #37.12275
mse(observedReg, predictedLinRegReduced) #6008.596
rmae(observedReg, predictedLinRegReduced, mean(trainReg$poraba)) #0.2460462
rmse(observedReg, predictedLinRegReduced, mean(trainReg$poraba)) #0.09465932

### Regression Tree

library(rpart)

rtReduced <- rpart(poraba ~ max_por_prejsni_dan + povrsina + ura + pritisk, data=trainReg)
predictedRtReduced <- predict(rtReduced, testReg)

mae(observedReg,predictedRtReduced) #46.2569
rmae(observedReg, predictedRtReduced, mean(trainReg$poraba)) #0.3065865

#pruned

rtReduced <- rpart(poraba ~ max_por_prejsni_dan + povrsina + ura + pritisk, data=trainReg, cp=0)
tab <- printcp(rtReduced)
row <- which.min(tab[,"xerror"])
th <- mean(c(tab[row, "CP"], tab[row-1, "CP"]))

rtReduced <- prune(rtReduced, cp=th)
predictedRtReduced <- predict(rt, testReg)

mae(observedReg,predictedRtReduced) #42.22185
rmae(observedReg, predictedRtReduced, mean(trainReg$poraba)) #0.0.2798425

### SVM

library(e1071)

svmRegReduced <- svm(poraba ~ max_por_prejsni_dan + povrsina + ura + pritisk, trainReg)
predictedSvmRegReduced <- predict(svmRegReduced, testReg)

mae(observedReg,predictedSvmRegReduced) #30.21697
rmae(observedReg, predictedSvmRegReduced, mean(trainReg$poraba)) #0.2002753
