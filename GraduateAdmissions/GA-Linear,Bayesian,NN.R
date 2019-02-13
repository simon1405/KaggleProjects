#Simon 
##2/13/2019

graadm<-read.csv(("Rdocuments/KaggleProjects/KaggleProjects/GraduateAdmissions/Admission_Predict_Ver1.1.csv"),header=T)

colnames(graadm)
par( mfrow=c(3,3))
for (i in 1:9){
  print(colnames(graadm)[i])
  print("has a p-value of Shapiro.test")
  print(shapiro.test(graadm[,i])$p.value)
  qqnorm(graadm[,i], pch = 1, frame = FALSE,main=colnames(graadm)[i])
  qqline(graadm[,i], col = "steelblue", lwd = 2)
}
mydata<-graadm[,-1]
lm.try<-lm(Chance.of.Admit~.,data=mydata)
summary(lm.try)
lm.fit<-lm(Chance.of.Admit~GRE.Score+TOEFL.Score +LOR+
              CGPA+Research,data=mydata)
summary(lm.fit)
anova(lm.fit)
par( mfrow=c(2,2))
plot(lm.fit)
##University.Rating,SOP don't meet with the significance larger than 0.05

##Using Bayesian Regression
install.packages("MCMCpack")
library(MCMCpack)
mcmc.fit<-MCMCregress(Chance.of.Admit~GRE.Score+TOEFL.Score+ 
                      LOR+ CGPA+Research,data=mydata)
summary(mcmc.fit)
pr.lm<-predict(lm.fit,mydata[,c(-8,-4,-3)])

SSE <- sum((mydata[,8] - pr.lm) ^ 2)
par( mfrow=c(1,1))
plot(x=pr.lm,y=mydata[,8])
##R^2 is 0.82
mean(abs(pr.lm-mydata[,8]))
##Using NN
library(nnet)
library(neuralnet)

normli<-function(x){
  (x-mean(x))/sd(x)
}
data.x<-mydata[,1:7]
data.x<-normli(as.matrix(data.x))
data.all<-cbind(data.x,mydata[,8])
nn.lm<-
  neuralnet(V8~.,data=data.all,
            hidden=c(4,2),rep=1,
            threshold = 0.01,
            stepmax=1000000,
            learningrate.limit = NULL,
            learningrate = 0.05,
            act.fct ="logistic", 
            linear.output=TRUE)

plot(nn.lm)
summary(nn.lm)
pr.nnlm <- compute(nn.lm,as.data.frame(data.all[,1:7]))
plot(x=pr.nnlm$net.result,y=data.all[,8],main="prediction comparing to admission")
mean(abs(pr.nnlm$net.result-mydata[,8]))
SSE <- sum((data.all[,8] - pr.nnlm$net.result) ^ 2)
##Prediction is similar to linearRegression