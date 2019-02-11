##Simon
##2/11/2019
heart <- read.csv("Rdocuments/KaggleProjects/HeartDisease-UCI/heart.csv", header = T)
heart.x<-heart[,-14]
heart.y<-heart[,14]
## Plots for distributions of variables are already shown on websites 

#PCA and FA
fit <- princomp(as.matrix(heart.x), cor=TRUE)
summary(fit)
plot(fit,type="lines", main="Scree Plot")
biplot(fit)

eigenvalue<-eigen(cor(as.matrix(heart.x)))$values
eigenvector<-eigen(cor(as.matrix(heart.x)))$vectors
varexplained <-c()
for (i in 1:13){
  (varexplained[i]<-sum(eigenvalue[1:i])/sum(eigenvalue))
}
##Using FA might not be effective since 10 of 13 variables explains almost 90% whole vairances

#Logistic Regression
cp.1<-matrix(rep(0,303),ncol=1)
cp.2<-matrix(rep(0,303),ncol=1)
cp.3<-matrix(rep(0,303),ncol=1)
cp.1[which(heart.x$cp==1)]=1
cp.2[which(heart.x$cp==2)]=1
cp.3[which(heart.x$cp==3)]=1
data.x<-cbind(as.matrix(heart.x[,-3]),cp.1,cp.2,cp.3)

hd.logit <- glm(heart.y ~ data.x, family = "binomial")
summary(hd.logit)
##Age, Trestbps,Chol,Fbs,Restecg,Slope are not statistically significant based on P-value >0.05

##Data restructuring
data.x<-cbind(as.matrix(heart.x[,c(-1,-3,-5,-6,-7,-11)]),cp.1,cp.2,cp.3)
colnames(data.x)[8]<-'cp.1'
colnames(data.x)[9]<-'cp.2'
colnames(data.x)[10]<-'cp.3'

hd1.logit <- glm(heart.y ~ data.x, family = "binomial")
summary(hd1.logit)

confint.default(hd1.logit)
exp(coef(hd1.logit))
##Odd ratios are shown above
pred<-predict(hd1.logit, newdata =as.data.frame(data.x),type="response")
pred.hd <- matrix(rep(0,303),ncol=1)
pred.hd[which(pred>=0.5)]=1
table(pred.hd,heart.y)
## Confusion Table is 
##       heart.y
##pred.hd   0   1
##0         106  16
##1         32 149
mean(pred.hd==heart.y)
##Accuracy is 0.8415842
install.packages("pROC")
library(pROC)
prob<-predict(hd1.logit,type=c("response"))
g <- roc(heart.y~prob)
plot(g)

##Although Age is not significant, we still take a look at age
hist(heart$ï..age)
max(heart$ï..age)-min(heart$ï..age)
age<-heart$ï..age
##Divide patient into 4 groups with age (29,41),(41,53),(53,65),(65,77) 
age.1 <- matrix(rep(0,303),ncol=1)
age.2 <- matrix(rep(0,303),ncol=1)
age.3 <- matrix(rep(0,303),ncol=1)
age.4 <- matrix(rep(0,303),ncol=1)
 for (i in 1:303){
    if (age[i]<=41) {age.1[i]=1}
    if ((age[i]<=53)&(age[i]> 41)) {age.2[i]=1}
    if ((age[i]<=65)&(age[i]> 53)) {age.3[i]=1}
    if ((age[i]<=77)&(age[i]> 65)) {age.4[i]=1}
 }
barplot(c(sum(age.1),sum(age.2),sum(age.3),sum(age.4)),main = "4 Divisions of Age")

age.logit <- glm(heart.y ~ age.1 +age.2+age.3, family = "binomial")
summary(age.logit)
##Based on the p-value of groups, neither are significant. So we can conclude that age 
##may not be the factor associated with Heart Diseases.

