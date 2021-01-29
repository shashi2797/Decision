library(readr)
Fraud<-read.csv(file.choose())
View(Fraud)
library(C50) ##use for decision tree#
library(gmodels) ##use for cross table#
library(party)##also use for decision tree#
library(caret) #use for confusion matrix#

#.........EDA.............#
sum(is.na(Fraud)) ##checking null values#

#we convert numerical values into categories#
tex_cat<-ifelse(Fraud$Taxable.Income <=30000, "Risky","Good")
Fraud<-data.frame(Fraud,tex_cat)
str(Fraud)
Fraud<-Fraud[-3]


#........spiliting..........#

Fraud_train<-Fraud[1:450,]
Fraud_test<-Fraud[451:600,]

prop.table(table(Fraud_train$tex_cat))
prop.table(table(Fraud_test$tex_cat))

Fraud[sapply(Fraud,is.character)] <- lapply(Fraud[sapply(Fraud,is.character)],as.factor)
str(Fraud)

str(Fraud_train$default)
Fraud_train$tex_cat<- as.factor(Fraud_train$tex_cat)
#Building Model on Training data set#
tex_train<-C5.0(Fraud_train[,-6],Fraud_train$tex_cat)
plot(tex_train)

op_tree = ctree(tex_cat ~ Undergrad +Marital.Status  + City.Population + Work.Experience+Urban , data = Fraud_train)


plot(op_tree)  

##training Accuracy#
pred_train<-predict(tex_train,Fraud_train)
#confusion matrix 
confusionMatrix(pred_train,Fraud_train$tex_cat)

mean(pred_train==Fraud_train$tex_cat) #0.77#

##predicting on test data#
pred_test<-predict(tex_train,newdata = Fraud_test)
table(pred_test,Fraud_test$tex_cat)
#cross table for comparison#
CrossTable(Fraud_test$tex_cat,pred_test)
mean(pred_test==Fraud_test$tex_cat) ##accuracy=0.775#

######company data########
company<-read.csv(file.choose())
View(company)


#.........EDA.............#
sum(is.na(company)) ##checking null values#
str(company)
summary(company)
company[sapply(company,is.character)] <- lapply(company[sapply(company,is.character)],as.factor)
str(company)


sales <- ifelse(company$Sales<9, "NO","YES")
company<-data.frame(company,sales)
company<-company[-1]
str(company)

com_train <- company[1:200,]
com_test <- company[201:400,]

prop.table(table(com_train$sales))
prop.table(table(com_test$sales))
com_train$sales<- as.factor(com_train$sales)
sales_train <-C5.0(com_train[,-11],com_train$sales)
plot(sales_train)

op_tree = ctree(sales ~ CompPrice + Income + Advertising + Population + Price + ShelveLoc
                 + Age + Education + Urban + US, data = com_train)

plot(op_tree) ## ctree gives me less rule but it gives me less accuracy so i go with c5.0# 

pred_train<-predict(sales_train,newdata = com_test)
table(com_test$sales)
pred_test <- predict(sales_train,newdata = com_test)
table(pred_test,com_test$sales)

CrossTable(com_test$sales,pred_test)
mean(pred_test==com_test$sales)

