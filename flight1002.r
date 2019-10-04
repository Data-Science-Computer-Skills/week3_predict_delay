
library(MASS)
install.packages("rpart")
library(rpart)
install.packages("randomForest")
library(randomForest)
install.packages("pROC")
library(pROC)
summary(flights0930)
flights09300=flights0930#用全部数据的时候删去这一行
flights0930 <- flights09300[1:10000,]#用全部数据的时候删去这一行
#若效果不好，可分为季节做
#flights0930$spring=flights0930$month_1+flights0930$month_2+flights0930$month_3
#flights0930$summer=flights0930$month_4+flights0930$month_5+flights0930$month_6
#flights0930$fall=flights0930$month_7+flights0930$month_8+flights0930$month_9
#flights0930$winter=flights0930$month_10+flights0930$month_11+flights0930$month_12
#step1&2
flights0930_2 <- flights0930[,-c(1:9,35:65)]
flights0930_2$DEPARTURE_DELAY=log(flights0930$DEPARTURE_DELAY-min(flights0930$DEPARTURE_DELAY)+1)
flights0930_2$TAXI_OUT=log(flights0930$TAXI_OUT)
flights0930_2$SCHEDULED_TIME=log(flights0930$SCHEDULED_TIME)
flights0930_2$ELAPSED_TIME=log(flights0930$ELAPSED_TIME)
flights0930_2$AIR_TIME=log(flights0930$AIR_TIME)
flights0930_2$DISTANCE=log(flights0930$DISTANCE)
flights0930_2$TAXI_IN=log(flights0930$TAXI_IN)
flights0930_2$ARRIVAL_DELAY=log(flights0930$ARRIVAL_DELAY-min(flights0930$ARRIVAL_DELAY)+1)

train = sample(c(T,F),nrow(flights0930_2),rep=T)
test = !train
##step3
fit1 <- lm(ARRIVAL_DELAY~.,data = flights0930_2[train,])
fit_step <- stepAIC(fit1,direction="both")

#step4
#回归树
fit_tree <- rpart(ARRIVAL_DELAY~.,data = flights0930_2[train,])
tree <- plot(fit_tree)
#随机森林
fit_forest <- randomForest(ARRIVAL_DELAY~., data=flights0930_2[train,],ntree=100,mtry=5,importance=TRUE, proximity=TRUE)
fit_forest$importance

#用画图来看变量的重要性
varImpPlot(fit_forest, main = "variable importance")

#step5验证
pred1=predict(fit_step,newdata=flights0930_2[test,])
pred2=predict(fit_tree,newdata=flights0930_2[test,])
pred3=predict(fit_forest,newdata=flights0930_2[test,])
result=cbind(pred1,pred2,pred3)
#以均方误差作为评估标准
dif=result-flights0930_2$ARRIVAL_DELAY[test]
mse=apply(dif^2,2,mean)
##逐步回归的结果明显优于两种树方法，可能是因为树方法做得太粗糙

#step6
flights0930_2l=flights0930_2
flights0930_2l$ARRIVAL_DELAY[flights0930$ARRIVAL_DELAY<=0] <- 0
flights0930_2l$ARRIVAL_DELAY[flights0930$ARRIVAL_DELAY>0] <- 1

#step7
fit_full <- glm(ARRIVAL_DELAY~.,data = flights0930_2l)
fit_step2 <- stepAIC(fit_full,direction="both")
