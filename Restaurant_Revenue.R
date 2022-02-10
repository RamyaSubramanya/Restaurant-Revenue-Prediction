setwd(dir = "D:\\IIM - K\\SUPERVISED MACHINE LEARNING\\REGRESSION\\Restaurant Revenue Prediction(Kaggle)")

data<-read.csv("train.csv")
data1<-read.csv("test.csv")

install.packages("dplyr",dependencies = TRUE)
library(dplyr)

main<-bind_rows(data, data1)

colSums(is.na(data))

table(data$City.Group)
table(data$Type)

main<-main[,-c(1,2,3)]

str(main)

table(main$City.Group)
table(main$Type)

main$City.Group<-factor(main$City.Group, levels = c("Big Cities", "Other"), labels = c(1,2))

install.packages("fastDummies",dependencies = TRUE)
library(fastDummies)

?dummy_cols
main<-dummy_cols(main,select_columns = "Type",remove_first_dummy = TRUE)
main<-main[,-c(2)]

train<-main[1:137,]
test<-main[138:100137,]


----------------------------------------------------------------------------------

#split the data into Train and Test

#install.packages("caTools",dependencies = TRUE) 
#library(caTools)

#?sample.split
#split<-sample.split(main,SplitRatio = 0.70)
#split  
  
#train<-subset(main,split==TRUE)

#test<-subset(main,split==FALSE)

--------------------------------------------------------------------------------
#Run Regression models to check which model performs better with less RMSE
  
install.packages("caret",dependencies = TRUE)
library(caret)

install.packages("glmnet", dependencies = TRUE)
library(glmnet)

install.packages("psych",dependencies = TRUE)
library(psych)

?trainControl

custom<-trainControl(method = "repeatedcv", number = 10, repeats = 5)

--------------------------------------------------------------------------------

#Linear Regression model
set.seed(123)
?train
linear<-train(revenue~.,train,method="lm",trControl=custom)

linear$results
linear
summary(linear)

plot(varImp(linear,scale = TRUE))

--------------------------------------------------------------------------------
  
#Ridge Regression model

set.seed(123)
?train
ridge<-train(revenue~.,train,method="glmnet",
            tuneGrid=expand.grid(alpha=0,lambda=seq(0.0001,1,length=5)),
              trControl=custom)

ridge$results
ridge
summary(ridge)

plot(varImp(ridge,scale = TRUE))
plot(ridge$finalModel, xvar = "lambda", label = TRUE)

--------------------------------------------------------------------------------

#Lasso Regression model

set.seed(123)
?train
lasso<-train(revenue~.,train,method="glmnet",
             tuneGrid=expand.grid(alpha=1,lambda=seq(0.0001,1,length=5)),
             trControl=custom)

lasso$results
lasso
summary(lasso)

plot(varImp(lasso,scale = TRUE))

--------------------------------------------------------------------------------
  
#Elastic net
  
set.seed(123)
?train
elastic<-train(revenue~.,train,method="glmnet",
             tuneGrid=expand.grid(alpha=seq(0.1,length=10),lambda=seq(0.001,1,length=5)),
             trControl=custom)

elastic$results
elastic
summary(elastic)

plot(varImp(elastic,scale = TRUE))


--------------------------------------------------------------------------------
  
#compare models
  
modellist<-list(linear=linear, ridge=ridge, lasso=lasso, elastic=elastic)
?resamples
compare<-resamples(modellist)
summary(compare) 

#From comparing models we found that Ridge has less RMSE Median

ridge$bestTune


--------------------------------------------------------------------------------
#Check the model on test data
  
Predicted<-predict(ridge,test)

install.packages("Metrics",dependencies = TRUE)
library(Metrics)

View(Predicted)

Final<-data.frame(cbind(Actuals=test$revenue,Predicted=Predicted))

write.csv(Final$Predicted,"submission.csv")

#RMSE<-rmse(Final$Actuals,Final$Predicted)


#So RMSE is 32% which means Ridge model that we selected is able to predict the revenue for the restaurant well. 
