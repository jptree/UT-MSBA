#######Multiple Var Regression

rm(list = ls())
library(ggplot2)
library("tidyverse")
library(InformationValue)
library(RColorBrewer)
library(glmnet)
library(reshape2)
library(leaps)
set.seed(2020)
#models are based on scaled redwine file "red_wine PER.csv"
#to run models on white wine database, swap "red_wine PER.csv" with "wine_white.csv"

red_wine = read.csv('C:\\Users\\DENG\\Desktop\\UTA Doc\\Summer\\predictive modeling\\group project\\red_wine PER.csv',header=TRUE)
df<-data.frame(red_wine)
#sample train/text
bound<-floor((nrow(df)/10)*7)
df<-df[sample(nrow(df)),]
df.train<-df[1:bound,]
df.test<-df[(bound+1):nrow(df),]


#plot data
plot(x=df.train$fixed.acidity,y=df.train$quality)
plot(x=df.train$volatile.acidity,y=df.train$quality)
plot(x=df.train$citric.acid,y=df.train$quality)
plot(x=df.train$residual.sugar,y=df.train$quality)
plot(x=df.train$chlorides,y=df.train$quality)
plot(x=df.train$free.sulfur.dioxide,y=df.train$quality)
plot(x=df.train$total.sulfur.dioxide,y=df.train$quality)
plot(x=df.train$density,y=df.train$quality)
plot(x=df.train$pH,y=df.train$quality)
plot(x=df.train$sulphates,y=df.train$quality)
plot(x=df.train$alcohol,y=df.train$quality)


#optimize subset
regfit_full=regsubsets(df.train$quality~.,data=df.train,nvmax=11)
summary(regfit_full)


#multi liner regression(F-STAT 52.34)
model_red<-lm(quality~fixed.acidity+volatile.acidity+
            citric.acid+residual.sugar+chlorides+
            free.sulfur.dioxide+total.sulfur.dioxide+
            density+pH+sulphates+alcohol,data=df.train)
summary(model_red)

#test model for red(RMSE 1.337,R2 0.342, bad predict)
pred_red<-predict(model_red,df.test)
rmse<-sqrt(sum(exp(pred_red)-df.test$quality)^2)/
  length(df.test$quality)
c(RMSE=rmse,R2=summary(model_red)$r.squared)

#plot predict
par(mfrow=c(1,1))
plot(df.test$quality,exp(pred_red))

#need to take out less relevant variables(fixed.acid/citric/sugar/free/)
#model_red2 F-STAT=80.84
model_red2<-lm(quality~volatile.acidity+chlorides+total.sulfur.dioxide+
                density+pH+sulphates+alcohol,data=df.train)
summary(model_red2)

#test model_red2 for red2(RMSE 1.337,R2 0.337, bad predict)
pred_red2<-predict(model_red2,df.test)
rmse<-sqrt(sum(exp(pred_red2)-df.test$quality)^2)/
  length(df.test$quality)
c(RMSE=rmse,R2=summary(model_red2)$r.squared)

#plot predit vs actual
plot(pred_red2,df.test$quality,
     xlab = 'Prediction',ylab='Actual')


#log model red(RMSE1.337 not good)
logmodel_red<-glm(quality~fixed.acidity+volatile.acidity+
                citric.acid+residual.sugar+chlorides+
                free.sulfur.dioxide+total.sulfur.dioxide+
                density+pH+sulphates+alcohol,data=df.train)
summary(logmodel_red)

#log model red2(only keep vol/chlo/total/ph/sulphate/alcohol)
logmodel_red2<-glm(quality~volatile.acidity+chlorides+
                    total.sulfur.dioxide+pH+sulphates+
                     alcohol,data=df.train)
summary(logmodel_red2)

#test logmodel_red2 for red2(RMSE 1.336,bad predict)
pred_logred2<-predict(logmodel_red2,df.test)
rmse<-sqrt(sum(exp(pred_logred2)-df.test$quality)^2)/
  length(df.test$quality)
c(RMSE=rmse,R2=summary(logmodel_red2)$r.squared)
misClassError(df.test$quality,pred_logred2)

#Ridge regression 
y<-df.train$quality
x<-df.train%>%select(volatile.acidity,chlorides,total.sulfur.dioxide,
                     pH,sulphates,alcohol)%>%data.matrix()
lambdas <- 10^seq(3, -2, by = -.1)
ridge_red<-glmnet(x,y,alpha=0,lambda=lambdas)
summary(ridge_red)
cv_ridge_red<-cv.glmnet(x,y,altha=0,lambda=lambdas)
plot(cv_ridge_red)
opt_lambda<-cv_ridge_red$lambda.min

#ridge test
y_pred<-predict(ridge_red,s=opt_lambda,newx=x)
sst<-sum((y-mean(y))^2)
sse<-sum((y_pred-y)^2)
rsq<-1-sse/sst
rsq

#Lasso regression
#Ridge regression 
y<-df.train$quality
x<-df.train%>%select(volatile.acidity,chlorides,total.sulfur.dioxide,
                     pH,sulphates,alcohol)%>%data.matrix()
lambdas <- 10^seq(3, -2, by = -.1)