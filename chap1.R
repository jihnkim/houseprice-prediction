install.packages("dplyr")
install.packages("reshape2")
install.packages("forecast")
install.packages("tseries")
install.packages("ggplot2")
install.packages("chart.Correlation")
install.packages("car")
install.packages("caret")
install.packages("mixlm")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("Metrics")
install.packages("MASS")

library(mixlm)
library(dplyr)
library(reshape2)
library(ggplot2)
library(forecast)
library(tseries)
library(car)
library(caret)
library(rpart)
library(rpart.plot)
library(Metrics)
library(MASS)


## 데이터 불러오기
train<-read.csv("C:/Users/jih11/Desktop//housing_train.csv",header=TRUE)
test<-read.csv("C:/Users/jih11/Desktop//housing_test.csv",header = TRUE)

sum(is.na(train)) #총결측치개수 6965

## 수치형 데이터 따로 뽑은 후 상관관계 분석으로 R값 확인 
train_r<-as.data.frame(train[,c("SalePrice", "YearBuilt", "TotalBsmtSF", "X1stFlrSF" ,"GrLivArea", "FullBath", "TotRmsAbvGrd", "GarageCars",  "GarageArea")])
sum(is.na(train_r))
train_data<-na.omit(train_r)
train_data_cor<-cor(train_data)

summary(train_data)

#산점도 차고 연도 빼구 
plot(train_data$YearBuilt,train_data$SalePrice,main = "SalesPrice~YearBuilt",ylab = "Price",ylim = c(0,800000),xlim=c(1850,2015),col='blue')
plot(train_data$TotalBsmtSF,train_data$SalePrice,main = "SalesPrice~TotalBsmtSF",ylab = "Price",ylim = c(0,800000),xlim=c(0,6200),col='green4')
plot(train_data$X1stFlrSF,train_data$SalePrice,main = "SalesPrice~X1stFlrSF",ylab = "Price",ylim = c(0,800000),xlim=c(0,5000),col='orange')
plot(train_data$GrLivArea,train_data$SalePrice,main = "SalesPrice~GrLivArea",ylab = "Price",ylim = c(0,800000),xlim=c(0,6000),col='yellowgreen')
plot(train_data$FullBath,train_data$SalePrice,main = "SalesPrice~FullBath",ylab = "Price",ylim = c(0,800000),xlim=c(0,3),col='steelblue3')
plot(train_data$TotRmsAbvGrd,train_data$SalePrice,main = "SalesPrice~TotRmsAbvGrd",ylab = "Price",ylim = c(0,800000),xlim=c(0,12),col='tan2')
plot(train_data$GarageCars,train_data$SalePrice,main = "SalesPrice~GarageCars",ylab = "Price",ylim = c(0,800000),xlim=c(0,4),col='violetred')
plot(train_data$GarageArea,train_data$SalePrice,main = "SalesPrice~GarageArea",ylab = "Price",ylim = c(0,800000),xlim=c(0,1500),col='red')

# boxplot 통계치 확인후 이상치 제거 ***이상치 124개 

boxplot(train_data$YearBuilt)$stats
boxplot(train_data$TotalBsmtSF)$stats
boxplot(train_data$X1stFlrSF)$stats
boxplot(train_data$GrLivArea)$stats
boxplot(train_data$FullBath)$stats
boxplot(train_data$TotRmsAbvGrd)$stats
boxplot(train_data$GarageCars)$stats
boxplot(train_data$GarageArea)$stats

train_data$YearBuilt<-ifelse(train_data$YearBuilt<1885|train_data$YearBuilt>2010, NA, train_data$YearBuilt)
train_data$TotalBsmtSF<-ifelse(train_data$TotalBsmtSF<105.0|train_data$TotalBsmtSF>2046.0, NA, train_data$TotalBsmtSF)
train_data$X1stFlrSF<-ifelse(train_data$X1stFlrSF<334.0|train_data$X1stFlrSF>2136, NA, train_data$X1stFlrSF)
train_data$GrLivArea<-ifelse(train_data$GrLivArea<334|train_data$GrLivArea>2730, NA, train_data$GrLivArea)
train_data$FullBath<-ifelse(train_data$FullBath<0|train_data$FullBath>3, NA, train_data$FullBath)
train_data$TotRmsAbvGrd<-ifelse(train_data$TotRmsAbvGrd<2|train_data$TotRmsAbvGrd>10, NA, train_data$TotRmsAbvGrd)
train_data$GarageCars<-ifelse(train_data$GarageCars<0|train_data$GarageCars>3, NA, train_data$GarageCars)
train_data$GarageArea<-ifelse(train_data$GarageArea<0|train_data$GarageArea>936, NA, train_data$GarageArea)

train_data<-na.omit(train_data)
summary(train_data)



#테스트로 나누기
#train_data_train<-train_data[1:844,]
#train_data_test<-train_data[844:1336,]

set.seed(1)
ind<-sample(2,nrow(train_data), replace = TRUE, prob = c(0.7,0.3))
Train<-train_data[ind==1, ]
Test<-train_data[ind==2, ]


#의사결정나무,10fold방식으로 a는 0이 될 때까지 나무구조 저장, 노드를 나누는 최소 자료수 전체 데이터 수의 5%
mycontrol<-rpart.control(xval=10,cp=0,minsplit=nrow(Train)*0.05)
tree_model<-rpart(SalePrice~.,method="anova",control = mycontrol,data = Train)
rpart.plot(tree_model)

#최적 complexity parameter 찾기,xerror가 가장 낮은거 찾음
printcp(tree_model)
plotcp(tree_model)
which.min(tree_model$cptable[,'xerror'])


#최적 cp로 가지치기 수행
pruned_model<-prune.rpart(tree_model,cp=0.00312)
#pruned_model<-prune.rpart(tree_model,cp=tree_model$cptable[which.min(tree_model$cptable[,'xerror']),"CP"])
pruned_model

#중요한 변수를 수치적으로 나타냄
pruned_model$variable.importance
rpart.plot(pruned_model,cex=0.8,type=5)
summary(pruned_model)
pruned_model

test_pred<-predict(pruned_model,newdata = Test)
#예측 confusionMatrix(test_pred,train_data_test$SalePrice)


summary(pruned_model)
var.test(test_pred,Test$SalePrice)

rmse(Test$SalePrice,test_pred)


cor(train_data)

# 다중회귀분석 
model<-lm(SalePrice~.,data = train_data)
summary(model)
coef(model)

# stepwise를 통한 독립변수 선택
stepwise<-step(model, direction = "both")
summary(stepwise)
anova(stepwise)

# 다중공선성 
vif(model)

vif(stepwise)

# 다중회귀모델 predict
lmmodel<-lm(SalePrice~.,data = Train)
lmmodel_pred<-predict(model,newdata = Test)
mse_lmmodel<-mean((Test$SalePrice-lmmodel_pred)^2)
rmse_lmmodel<-sqrt(mse_lmmodel)

mse_lmmodel
rmse_lmmodel

# Stepwise model predict
stepmodel_pred<-predict(stepwise,newdata = Test)
mse_stepmodel<-mean((Test$SalePrice-stepmodel_pred)^2)
rmse_stepmodel<-sqrt(mse_stepmodel)

mse_stepmodel
rmse_stepmodel

############################################# 인공신경망

normalize <- function(x){
  (x-min(x))/(max(x)-min(x))
}

scaled_train<-normalize(train_data)


set.seed(2)
ind<-sample(2,nrow(scaled_train), replace = TRUE, prob = c(0.7,0.3))

nn_train<-scaled_train[ind==1, ]
nn_test<-scaled_train[ind==2, ]
View(nn_test)
model_train<-train_data[ind==1, ]
model_test<-train_data[ind==2, ]
View(model_test)


myformula<-SalePrice~.

#test 자료를 이용하여 Hidden node의 크기를 결정
test_err <- function(h_size, maxit0){
  nnmodel1<-nnet(myformula, data=scaled_train, size=h_size, decay = 5e-4, trace = F,maxit = maxit0)
  y <- nn_test$SalePrice
  p <- predict(nnmodel1, nn_test)
  err <- rmse(y,p)
  c(h_size,err)
}

out <- t(sapply(1:10,FUN=test_err, maxit0=200))
plot(out,type="b", xlab = "The number of Hidden units", ylab = "Test Error")


model_h3<- nnet(myformula, size=3, decay = 5e-4, range = 0.1, maxit = 200, data= model_train)
model_h4<- nnet(myformula, size=4, decay = 5e-4, range = 0.1, maxit = 200, data=model_train)
model_h5<- nnet(myformula, size=5, decay = 5e-4, range = 0.1, maxit = 200, data=model_train)
model_h6<- nnet(myformula, size=6, decay = 5e-4, range = 0.1, maxit = 200, data=model_train)
model_h7<- nnet(myformula, size=7, decay = 5e-4, range = 0.1, maxit = 200, data=model_train)

price_pred3<-predict(model_h3,new=model_test)
price_pred4<-predict(model_h4,new=model_test)
price_pred5<-predict(model_h5,new=model_test)
price_pred6<-predict(model_h6,new=model_test)
price_pred7<-predict(model_h7,new=model_test)

View(price_pred3)

rmse(model_test$SalePrice,price_pred3)
rmse(model_test$SalePrice,price_pred4)
rmse(model_test$SalePrice,price_pred5)
rmse(model_test$SalePrice,price_pred6)
rmse(model_test$SalePrice,price_pred7)
