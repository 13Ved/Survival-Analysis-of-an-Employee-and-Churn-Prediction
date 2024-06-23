#Reading the data-set
Data_HR=read.csv("D:\\MSc(ASA)\\Sem 2\\DDA\\HR-Employee-Attrition.csv")

#performing one-hot encoding on BusinessTravel and Marital Status
Data_one_hot=model.matrix(~BusinessTravel+ MaritalStatus+EducationField+JobRole,
                          data=Data_HR)

#combining the data and removing the unnecessary columns and changing 
#few columns to numeric
Data_HR=cbind(Data_HR,Data_one_hot)
View(Data_HR)
Data_HR=Data_HR[,-c(3,5,8,14,16,18,32)]
Data_HR$Attrition=ifelse(Data_HR$Attrition=="Yes",1,0)
Data_HR$Gender=ifelse(Data_HR$Gender=="Male",1,0)
Data_HR$OverTime=ifelse(Data_HR$OverTime=="Yes",1,0)
colnames(Data_HR)=make.names(colnames(Data_HR))

#Scaled data and split
scaled=scale(Data_HR[,c(3,8,4,12,13)])
scaled_data=cbind(Data_HR[,-c(3,4,8,12,13)],scaled)
names(scaled_data)[which(names(scaled_data) == "")[1]] <- ".1"
set.seed(71)
y=sort(sample(nrow(scaled_data),nrow(scaled_data)*0.8))
Train_scaled=scaled_data[y,]
Test_scaled=scaled_data[-y,]
Train_scaled=oversample(Train_scaled,ratio=0.40,method="SMOTE",classAttr = "Attrition")
imbalanceRatio(Train_scaled, classAttr = "Attrition")
fun=function(x){
  ifelse(x<0.5,0,1)
}

Train_scaled[,c(5,9,21:37)]=lapply(Train_scaled[,c(5,9,21:37)],fun)
Train_scaled[,c(5,9,21:37)]=lapply(Train_scaled[,c(5,9,21:37)],as.factor)
Test_scaled[,c(5,9,21:37)]=lapply(Test_scaled[,c(5,9,21:37)],as.factor)
str(Test_scaled)
str(Train_scaled)

#Splitting the data
set.seed(25)
x=sort(sample(nrow(Data_HR),nrow(Data_HR)*0.8))
Train_Data=Data_HR[x,]
Test_Data=Data_HR[-x,]

#oversampling the Train Data 
library(imbalance)
imbalanceRatio(Data_HR,classAttr ="Attrition")
Train_Data=oversample(Train_Data,ratio=0.4,method="SMOTE",classAttr = "Attrition")
imbalanceRatio(Train_Data,classAttr = "Attrition")
View(Train_Data)

#Converting the target variable to factor
Data_HR$Attrition=as.factor(Data_HR$Attrition)
Train_Data$Attrition=as.factor(Train_Data$Attrition)
Test_Data$Attrition=as.factor(Test_Data$Attrition)
table(Train_Data$Attrition)


#Logistic Regression
library(caret)
LR_model=glm(as.factor(Attrition)~. ,data = Train_scaled ,family = binomial)
table(Train_scaled$Attrition)
summary(LR_model)
length(which(LR_model$fitted.values<0.5))
predictions_LR= predict(LR_model, newdata=Test_scaled, type = "response")
predictions_LR=ifelse(predictions_LR<0.5,0,1)
confusionMatrix(as.factor(predictions_LR),as.factor(Test_scaled$Attrition))
length(which(predictions_LR<0.5))
library(pROC) 
roc_val=roc(Test_scaled$Attrition,predictions_LR);roc_val
plot(roc_val,main = "ROC Curve", col = "blue")

#Step in logistic
install.packages("ols")
stepwise_model=step(LR_model, direction = "both")


#Condition number data
eg=eigen(cor(mat))
mat=as.matrix(scaled_data[,c(1:20,38:42)])
cor(as.numeric(mat))
View(scaled_data)
mat=apply((scaled_data[,c(1,3:20,38:42)]), 2,as.numeric)
mat=as.matrix(mat)
eg=eigen(cor(mat))
library(car)
vif(LR_model)
condition_number=max(eg$values)/min(eg$values)


