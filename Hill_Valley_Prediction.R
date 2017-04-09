library(e1071)

x <- subset(train, select=-train$class)
y <- train$class

svm_model <- svm(train$class ~ ., data=train)
summary(svm_model)

svm_model1 <- svm(x,y)
summary(svm_model1)

pred <- predict(svm_model1,x)
system.time(pred <- predict(svm_model1,x))

table(pred,y)

svm_tune <- tune(svm, train.x=x, train.y=y, 
                 kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))

print(svm_tune)

svm_model_after_tune <- svm(train$class ~ ., data=train, kernel="radial", cost=10, gamma=0.5)
summary(svm_model_after_tune)

pred <- predict(svm_model_after_tune,x)
system.time(predict(svm_model_after_tune,x))

Table<-table(pred, y)

library(ROCR)
data(ROCR.simple)
pred <- prediction( pred, train$class )
perf <- performance( pred, "tpr", "fpr" )
plot(perf, avg="vertical", lwd=3, col="red",
    spread.estimate="stderror",plotCI.lwd=2,add=TRUE)