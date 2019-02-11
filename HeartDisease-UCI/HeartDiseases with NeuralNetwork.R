##Simon
##2/11/2019
install.packages("nnet")
install.packages("neuralnet")
library(nnet)
library(neuralnet)

heart <- read.csv("~/Rdocuments/KaggleProjects/HeartDisease-UCI/heart.csv", header = T)
heart.x<-as.matrix(heart[,-14])
heart.y<-heart[,14]

normli<-function(x){
  (x-mean(x))/sd(x)
}
heart$ï..age<-normli(heart$ï..age)
heart$trestbps<-normli(heart$trestbps)
heart$chol<-normli(heart$chol)
heart$thalach<-normli(heart$thalach)

mydata[,1:13]<-normli(as.matrix(heart[,1:13]))
mydata[,14]<-heart[,14]

mydata<-heart
colnames(mydata)[1]<-"age"
##train/test: sample are small, here is the optional choice
trainsize<- floor(0.85*nrow(mydata))
train <- sample(seq_len(nrow(mydata)), size = trainsize)
mydata.train <- mydata[train, ]
mydata.test  <- mydata[-train,]

##Using NN
nn<-
  neuralnet(target~.,data=mydata,
          hidden=c(5,3),rep=1,
          threshold = 0.01,
          stepmax=1000000,
          learningrate.limit = NULL,
          learningrate = 0.05,
          act.fct ="logistic", 
          linear.output=FALSE)

plot(nn)
pr.nn <- compute(nn,as.data.frame(heart[,1:13]))
table(mydata$target, round(pr.nn$net.result))
##Confusion Matrix is
##     0   1
## 0 130   8
## 1   4 161

##The accuracy is 96.04%
##The highest so far

##Let's use sigmoid as activate function
sigmoid = function(x) {
  1 / (1 + exp(-x))
}
nn.1<-
  neuralnet(target~.,data=mydata,
            hidden=c(5,3),rep=1,
            threshold = 0.01,
            stepmax=1000000,
            learningrate.limit = NULL,
            learningrate = 0.05,
            act.fct =sigmoid, 
            linear.output=FALSE)

pr.nn1 <- compute(nn.1,heart[,1:13])
table(mydata$target, round(pr.nn1$net.result))
##Confusion Matrix is
##      0   1
##  0 129   9
##  1   3 162

##The accuracy is 96.04%