#1) A cloth manufacturing company is intrested to know about the segment of attributes causes high sale.
# Approach - A Random forest can be built with target variable Sales (we will first convert it in categorical variable)& all other 
#            variable will be independed in the analysis.

install.packages("randomForest")
library(randomForest)
company_data <- Company_Data_R
View(company_data)
summary(company_data)
names(company_data)

# Creating bins
sales_bin <- cut(company_data$Sales,3, labels = c("Low", "Average", "High"))
sales_bin
company_data$salesStatus<-sales_bin
View(company_data)

# Creating training  and test datasets
?createDataPartition
 company_train_test <- createDataPartition(company_data$salesStatus, p=0.7, list=FALSE, times=1)
library(caret)
company_train_test <- createDataPartition(company_data$salesStatus, p=0.7, list=FALSE, times=1)
length(company_train_test) 
company_train<- company_data[company_train_test,]
company_test <- company_data[-company_train_test,]
View(company_test)
min(company_data$Sales)
max(company_data$Sales)
str(company_data)
company_data<- company_data[-1]
View(company_data)
attach(company_data)

# Building a random forest model on the training data
company_rf <- randomForest(salesStatus~., data = company_train,na.action = na.roughfix,importance=TRUE)
is.numeric(company_train)
 
#Prediction of train data
pred <- predict(company_rf,company_train)

# Training Accuracy
mean(pred== company_train$salesStatus)
# Prediction of test data
pred_test<- predict(company_rf,newdata=company_test)
library(caret)
library(gmodels)

#Accuracy
Accuracy= mean(pred_test==company_test$salesStatus) # Accuracy =100 % 
CrossTable(pred_test,company_test$salesStatus)

# Confusion Matrix 
confusionMatrix(pred_test,company_test$salesStatus)

# Visualization 
plot(company_rf,lwd=2)
legend("topright", colnames(company_rf$err.rate),col=1:4,cex=0.8,fill=1:4)
x=data.frame(pred_test)
x = cbind(x,company_test$salesStatus)
View(x)

#2 Use Random Forest to prepare a model on fraud data 
#treating those who have taxable_income <= 30000 as "Risky" and others are "Good"

fraud_data<- read.csv("C:/EXCELR/ASSIGNMENTS/RandomForests/Fraud_check.csv")
View(fraud_data)
summary(fraud_data)
names(fraud_data)
status = ifelse(fraud_data$Taxable.Income<=30000, "Risky", "Good")
fraud_data = data.frame(fraud_data, status)
attach(fraud_data)
View(fraud_data)

# Creating training and test datasets
?createDataPartition
fraud_train_test <- createDataPartition(fraud_data$status, p=0.7, list=FALSE, times=1)
length(fraud_train_test)
fraud_train <- fraud_data[fraud_train_test,]
fraud_test <- fraud_data[-fraud_train_test,]
View(fraud_test)
names(fraud_data)

# Building a random forest model on training data 

fraud_rf <- randomForest(status~Undergrad+Marital.Status+City.Population+Work.Experience+Urban,data=fraud_train, na.action=na.roughfix,importance=TRUE)
fraud_rf

# Training accuracy 
pred <- predict(fraud_rf,fraud_train)  
mean(pred==fraud_train$status) # 90.49881
confusionMatrix(fraud_train$status, pred)

CrossTable(fraud_train$status, pred)


# Prediction of test data
pred_test<- predict(fraud_rf,fraud_test)
Accuracy =mean(pred_test==fraud_test$status) #77.65%

library(caret)
library(gmodels)

# Confusion Matrix
confusionMatrix(fraud_test$status, pred_test)
CrossTable(fraud_test$status, pred_test)

# Visualization 
plot(fraud_rf,lwd=2)
legend("topright", colnames(fraud_rf$err.rate),col=1:4,cex=0.8,fill=1:4)
plot(fraud_rf, log="y")
legend("topright", colnames(company_rf$err.rate),col=1:4,cex=0.8,fill=1:4)
x=data.frame(pred_test)
x= cbind(x,fraud_test$status)
View(x)

# get tree 
# extract a  single tree from random forest
getTree(fraud_rf, k=1, labelVar=FALSE)
varImpPlot(fraud_rf)

