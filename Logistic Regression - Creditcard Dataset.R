
################ 

creditcard <- read.csv('D:\\Data Science\\Excelr\\Assignments\\Assignment\\Logistic Regression\\creditcard.csv')
creditcard <- creditcard[,-1] # Removing the first column which is is an Index 

# GLM function use sigmoid curve to produce desirable results   
# The output of sigmoid function lies in between 0-1 
model <- glm(card~.,data=creditcard,family = "binomial") 
summary(model) # Confusion matrix table  
prob <- predict(model,creditcard,type="response") 
prob 
# Confusion matrix and considering the threshold value as 0.5  
confusion<-table(prob>0.5,creditcard$card) 
confusion 
# Model Accuracy  
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy 
##ROC
library(ROCR)
rocrpred<-prediction(prob,creditcard$card)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
