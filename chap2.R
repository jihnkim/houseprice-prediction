install.packages("dplyr")
install.packages("reshape2")
install.packages("forecast")
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
library(car)
library(caret)
library(rpart)
library(rpart.plot)
library(Metrics)
library(MASS)


## 데이터 불러오기
data<-read.csv("C:/Users/jih11/Desktop//housing.csv",header=TRUE)

sum(is.na(data)) #총결측치개수 6965

## 수치형 데이터 따로 뽑은 후 상관관계 분석으로 R값 확인 
data_r<-as.data.frame(data[,c("SalePrice", "YearBuilt", "TotalBsmtSF", "X1stFlrSF" ,"GrLivArea", "FullBath", "TotRmsAbvGrd", "GarageCars",  "GarageArea")])
sum(is.na(data_r))
data<-na.omit(data_r)
data_cor<-cor(data)

summary(data)

#산점도 차고 연도 빼구 
plot(data$YearBuilt,data$SalePrice,main = "SalesPrice~YearBuilt",ylab = "Price",ylim = c(0,800000),xlim=c(1850,2015),col='blue')
plot(data$TotalBsmtSF,data$SalePrice,main = "SalesPrice~TotalBsmtSF",ylab = "Price",ylim = c(0,800000),xlim=c(0,6200),col='green4')
plot(data$X1stFlrSF,data$SalePrice,main = "SalesPrice~X1stFlrSF",ylab = "Price",ylim = c(0,800000),xlim=c(0,5000),col='orange')
plot(data$GrLivArea,data$SalePrice,main = "SalesPrice~GrLivArea",ylab = "Price",ylim = c(0,800000),xlim=c(0,6000),col='yellowgreen')
plot(data$FullBath,data$SalePrice,main = "SalesPrice~FullBath",ylab = "Price",ylim = c(0,800000),xlim=c(0,3),col='steelblue3')
plot(data$TotRmsAbvGrd,data$SalePrice,main = "SalesPrice~TotRmsAbvGrd",ylab = "Price",ylim = c(0,800000),xlim=c(0,12),col='tan2')
plot(data$GarageCars,data$SalePrice,main = "SalesPrice~GarageCars",ylab = "Price",ylim = c(0,800000),xlim=c(0,4),col='violetred')
plot(data$GarageArea,data$SalePrice,main = "SalesPrice~GarageArea",ylab = "Price",ylim = c(0,800000),xlim=c(0,1500),col='red')

# boxplot 통계치 확인후 이상치 제거 ***이상치 124개 

boxplot(data$YearBuilt)$stats
boxplot(data$TotalBsmtSF)$stats
boxplot(data$X1stFlrSF)$stats
boxplot(data$GrLivArea)$stats
boxplot(data$FullBath)$stats
boxplot(data$TotRmsAbvGrd)$stats
boxplot(data$GarageCars)$stats
boxplot(data$GarageArea)$stats

data$YearBuilt<-ifelse(data$YearBuilt<1885|data$YearBuilt>2010, NA, data$YearBuilt)
data$TotalBsmtSF<-ifelse(data$TotalBsmtSF<105.0|data$TotalBsmtSF>2046.0, NA, data$TotalBsmtSF)
data$X1stFlrSF<-ifelse(data$X1stFlrSF<334.0|data$X1stFlrSF>2136, NA, data$X1stFlrSF)
data$GrLivArea<-ifelse(data$GrLivArea<334|data$GrLivArea>2730, NA, data$GrLivArea)
data$FullBath<-ifelse(data$FullBath<0|data$FullBath>3, NA, data$FullBath)
data$TotRmsAbvGrd<-ifelse(data$TotRmsAbvGrd<2|data$TotRmsAbvGrd>10, NA, data$TotRmsAbvGrd)
data$GarageCars<-ifelse(data$GarageCars<0|data$GarageCars>3, NA, data$GarageCars)
data$GarageArea<-ifelse(data$GarageArea<0|data$GarageArea>936, NA, data$GarageArea)

data<-na.omit(data)
summary(data)



#테스트로 나누기

set.seed(1234)
ind<-sample(2,nrow(data), replace = TRUE, prob = c(0.7,0.3))
Train<-data[ind==1, ]
Test<-data[ind==2, ]


#의사결정나무,10fold방식으로 a는 0이 될 때까지 나무구조 저장, 노드를 나누는 최소 자료수 전체 데이터 수의 5%
mycontrol<-rpart.control(xval=10,cp=0,minsplit=nrow(Train)*0.05)
tree_model<-rpart(SalePrice~.,method="anova",control = mycontrol,data = Train)
rpart.plot(tree_model)

#최적 complexity parameter 찾기,xerror가 가장 낮은거 찾음
printcp(tree_model)
plotcp(tree_model)
which.min(tree_model$cptable[,'xerror'])


#최적 cp로 가지치기 수행
pruned_model<-prune.rpart(tree_model,cp=0.0048)
#pruned_model<-prune.rpart(tree_model,cp=tree_model$cptable[which.min(tree_model$cptable[,'xerror']),"CP"])
pruned_model

#중요한 변수를 수치적으로 나타냄
pruned_model$variable.importance
rpart.plot(pruned_model,cex=0.8,type=5)
summary(pruned_model)

test_pred<-predict(pruned_model,newdata = Test)
#예측 confusionMatrix(test_pred,train_data_test$SalePrice)


summary(pruned_model)
var.test(test_pred,Test$SalePrice)

rmse(Test$SalePrice,test_pred)


cor(data)

# 다중회귀분석 
model<-lm(SalePrice~.,data = data)
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
