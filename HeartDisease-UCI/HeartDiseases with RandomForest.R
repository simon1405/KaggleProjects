##Simon
##2/11/2019
heart <- read.csv("Rdocuments/KaggleProjects/HeartDisease-UCI/heart.csv", header = T)
heart.x<-heart[,-14]
heart.y<-heart[,14]

install.packages("e1071")
library(e1071)
library(caret)
worrf_50<- train(x = as.matrix(heart.x),
                 y = as.factor((heart.y)),
                 method = "rf",
                 ntree = 50,
                 trControl = trainControl(method = "oob"))

worrf_50
pred.rf<-predict(worrf_50$finalModel,as.matrix(heart.x))
worrf_50$finalModel$confusion
##Confusion Table is
##    0   1 class.error
##0 108  30   0.2173913
##1  21 144   0.1272727
max(1-worrf_50$finalModel$err.rate[,1])
##Accuracy is 0.8415842