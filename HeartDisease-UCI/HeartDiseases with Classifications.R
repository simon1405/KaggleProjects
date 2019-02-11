##Simon
##2/11/2019
heart <- read.csv("Rdocuments/KaggleProjects/HeartDisease-UCI/heart.csv", header = T)
heart.x<-as.matrix(heart[,-14])
heart.y<-heart[,14]
## Plots for distributions of variables are already shown on websites 

#Linear Discriminant Analysis
k<-2
library(MASS)
heart.lda <- lda(heart.y~heart.x, prior=rep(1,k)/k)
pred.lda <- predict(heart.lda)$class #Predicting each state
pre<-data.frame(heart.y,pred.lda)#Comparing our predictions and R predictions
table(pre)
##Confusion Table is
##       pred.lda
##heart.y   0   1
##      0 104  34
##      1  13 152
sum(diag(table(pre)))/sum(table(pre))
##accuracy is 0.8448845

#K nearest neighbor
library(class)
###K = 2
heart.knn <- knn(train=heart[1:13], test=heart[1:13], cl = heart$target, k=2)
(tab.knn <- table(Target = heart$target, Predicted = heart.knn))
##Confustion Table is
##      Predicted
##Target   0   1
##     0 105  33
##     1  24 141
sum(diag(tab.knn))/sum(tab.knn)
##accuracy is 0.8118812

#Classification tree
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
mct <- rpart(heart.y~heart.x, method="class")
rpart.plot(mct, main= "target", type=0,extra=101)
pct <- predict(mct, as.data.frame(data.x), type="class")
(tab.ct <- table(Target = heart$target, Predicted = pct))
##Confusion Table is 
##      Predicted
##Target   0   1
##     0 109  29
##     1  12 153
sum(diag(tab.ct))/sum(tab.ct)
##Accuracy is 0.8646865