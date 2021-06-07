####EVALVACIJA MODELOV#####

install.packages("lubridate")
library(lubridate)
dataFinal$mesec <- month(dataFinal$datum)
dataFinal$mesec <- as.factor(dataFinal$mesec)
dataKlas <- subset(dataFinal, select = -c(poraba, datum))

dataReg <- subset(dataFinal, select = -c(norm_poraba))

evaluation <- function(formula, model, data, evalFun, class = T, 
                       custPrune=F, predictFun, knn=F) {
  selLearn <- rep(F, times = nrow(dataFinal))
  cv.results <- vector()
  for(i in 1:11) {
    cat("ITERATION: ", i, "\n")
    flush.console()
    
    selLearn <- selLearn | dataFinal$mesec == i
    selTest <- dataFinal$mesec == i + 1
    if(knn){
      m <- kknn(formula, data[selLearn,], data[selTest,], k=5)
      
    }
    else{
    if(custPrune){
      m <- model(formula, data=data[selLearn,], cp=0)
      tab <- printcp(m)
      
      row <- which.min(tab[,"xerror"])
      th <- mean(c(tab[row, "CP"], tab[row-1, "CP"]))
      m <- prune(m, cp=th)
    }
    else{
      m <- model(formula, data[selLearn,])
    }
    }
    if(class){
      observed <- data$norm_poraba[selTest]
    }
    else{
      observed <- data$poraba[selTest]
    }
    if(knn){
      predicted <- predictFun(m)
    }
    else {
      predicted <- predictFun(m, data[selTest,])
    }
    if(!class)
      cv.results[i] <- evalFun(predicted, observed,mean(data$poraba[selLearn]))
    else
      cv.results[i] <- evalFun(predicted, observed)
  }
  
  cv.results
}

classPred <- function(m, data){
  predict(m, data, type="class")
}


mse <- function(obs, pred)
{
  mean((obs - pred)^2)
}

mae <- function(obs, pred)
{
  mean(abs(obs - pred))
}

rmae <- function(obs, pred, mean.val) 
{  
  sum(abs(obs - pred)) / sum(abs(obs - mean.val))
}

rmse <- function(obs, pred, mean.val) 
{  
  sum((obs - pred)^2)/sum((obs - mean.val)^2)
}



######DECISION TREES################
dataKlas$mesec <- NULL
dTree <- evaluation(formula=norm_poraba ~., model=rpart,data=dataKlas,
                    class=T,custPrune = T,predictFun = classPred, evalFun = CA)

dTree_unpruned <- evaluation(formula=norm_poraba ~ ., model=rpart,data=dataKlas,
                             class=T,custPrune = F,predictFun = classPred, evalFun = CA)
mean(dTree) 
sd(dTree)
plot(dTree, type="l", ylab="CA")

mean(dTree_unpruned)
sd(dTree_unpruned)
plot(dTree_unpruned, type="l",ylab="CA")




###NAIVE BAYES#########
library(e1071)

naiveB <- evaluation(formula= norm_poraba ~., model=naiveBayes,dataKlas ,
                     class=T, 
                     predictFun = classPred, evalFun = CA)

mean(naiveB)
sd(naiveB)

plot(naiveB, type="l",ylab="CA")



#### Random Forest
library(randomForest)

rndForest <- evaluation(formula, model=randomForest, dataKlas, class=T, 
                        predictFun = classPred, evalFun = CA)




########LINEAR REG########
dataReg$mesec <- NULL
dataReg$datum <- NULL
dataReg_lm <- subset(dataReg, select = -c(letni_cas))

linReg_rmse <- evaluation(formula=poraba~.,data=dataReg_lm,
                          model=lm, class=F,custPrune = F, 
                          predictFun=predict, evalFun=rmse)

mean(linReg_rmse)
sd(linReg_rmse)

plot(linReg_rmse, type="l", ylab="RMSE")



#### Regression Tree
regTreeUnpr_rmse <- evaluation(formula=poraba~.,data=dataReg,model=rpart, 
                               class=F, predictFun=predict, evalFun=rmse)

mean(regTreeUnpr_rmse)
sd(regTreeUnpr_rmse)
plot(regTreeUnpr_rmse, type="l", ylab="RMSE")

regTreePruned_rmse <- evaluation(formula=poraba~.,data=dataReg,model=rpart, 
                                 class=F,custPrune = T,predictFun=predict, 
                                 evalFun=rmse)

mean(regTreePruned_rmse)
plot(regTreePruned_rmse, type = "l", ylab="RMSE")
sd(regTreePruned_rmse)



####KNN-REG##########
library(kknn)
formula <- poraba ~ max_por_prejsni_dan + povrsina + ura + pritisk
knnReg <- evaluation(formula=formula,data=dataReg,model=kknn, 
                  class=F,predictFun=fitted, evalFun=rmse,knn=T)

mean(knnReg)
sd(knnReg)
plot(knnReg, type="l", ylab="RSME")

