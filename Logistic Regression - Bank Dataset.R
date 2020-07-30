
################ 

bank.full <- read.csv('D:\\Data Science\\Excelr\\Assignments\\Assignment\\Logistic Regression\\bank-full_r.csv')
# GLM function use sigmoid curve to produce desirable results   
# The output of sigmoid function lies in between 0-1 
model <- glm(y~.,data=bank.full,family = "binomial") 
summary(model) # Confusion matrix table  
prob <- predict(model,bank.full,type="response") 
prob 
# Confusion matrix and considering the threshold value as 0.5  
confusion<-table(prob>0.5,bank.full$y) 
confusion 
# Model Accuracy  
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy 
##ROC
library(ROCR)
rocrpred<-prediction(prob,bank.full$y)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
