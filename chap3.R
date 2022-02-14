install.packages("forecast")
install.packages("ggplot2")
install.packages("reshape2")
install.packages("corrplot")

library(reshape2)
library(ggplot2)
library(forecast)
library(dplyr)
library(corrplot)

# 1. CSV 불러오기
train<-read.csv("C:/Users/jih11/Desktop//housing_train.csv",header=TRUE)
test<-read.csv("C:/Users/jih11/Desktop//housing_test.csv",header = TRUE)

# 2. CSV 에서 유의미한 독립변수 추출
train_data<-select(train,LotFrontage,LotArea,TotalBsmtSF,GrLivArea,FullBath,TotRmsAbvGrd,GarageArea,YearBuilt,SalePrice)
test_data<-select(train,LotFrontage,LotArea,TotalBsmtSF,GrLivArea,FullBath,TotRmsAbvGrd,GarageArea,YearBuilt,SalePrice)

# 3. 설립년도 변수 형태 변경
train_data$YearBuilt<-abs(train_data$YearBuilt-2020)
test_data$YearBuilt<-abs(test_data$YearBuilt-2020)

# 4. 결측치 제거
train_data<-na.omit(train_data)

# 5. 결측치 제거 후 상관관계 분석
cor(train_data)
plot(train_data)

train_data.cor<-cor(train_data)
train_data

corrplot(train_data.cor, method = "number")

# 6. 이상치 제거를 위한 상자그림과 통계치 표현
# par(mfrow=c(3,3))
boxplot(train_data$LotFrontage)$stats
boxplot(train_data$LotArea)$stats
boxplot(train_data$TotalBsmtSF)$stats
boxplot(train_data$GrLivArea)$stats
boxplot(train_data$FullBath)$stats
boxplot(train_data$TotRmsAbvGrd)$stats
boxplot(train_data$GarageArea)$stats
boxplot(train_data$YearBuilt)$stats

# 7. 이상치를 결측치(NA)로 치환
train_data$LotFrontage<-ifelse(train_data$LotFrontage<30|train_data$LotFrontage>111, NA, train_data$LotFrontage)
train_data$LotArea<-ifelse(train_data$LotArea<1680|train_data$LotArea>16905, NA, train_data$LotArea)
train_data$TotalBsmtSF<-ifelse(train_data$TotalBsmtSF<0|train_data$TotalBsmtSF>2078, NA, train_data$TotalBsmtSF)
train_data$GrLivArea<-ifelse(train_data$GrLivArea<334|train_data$GrLivArea>2713, NA, train_data$GrLivArea)
train_data$FullBath<-ifelse(train_data$FullBath<0|train_data$FullBath>3, NA, train_data$FullBath)
train_data$TotRmsAbvGrd<-ifelse(train_data$TotRmsAbvGrd<2|train_data$TotRmsAbvGrd>10, NA, train_data$TotRmsAbvGrd)
train_data$GarageArea<-ifelse(train_data$GarageArea<0|train_data$GarageArea>968, NA, train_data$GarageArea)
train_data$YearBuilt<-ifelse(train_data$YearBuilt<10|train_data$YearBuilt>148, NA, train_data$YearBuilt)

# 8. 결측치로 치환한 이상치 제거
train_data<-na.omit(train_data)

# 9. 이상치 결측치 제거한 산점도 시각화
ggplot(train_data, aes(x=LotFrontage,y=SalePrice))+geom_point(shape=19, size=2, colour='red')+ggtitle("X: 가까운 도로까지 거리 Y : 판매가격")+theme(plot.title = element_text(size=15))+stat_smooth(method = 'lm')+theme_bw()
ggplot(train_data, aes(x=LotArea,y=SalePrice))+geom_point(shape=19, size=2, colour='blue')+ggtitle("X: 대지면적 Y : 판매가격")+theme(plot.title = element_text(size=15))+stat_smooth(method = 'lm')+theme_bw()
ggplot(train_data, aes(x=TotalBsmtSF,y=SalePrice))+geom_point(shape=19, size=2, colour='yellow')+ggtitle("X: 지하면적 Y : 판매가격")+theme(plot.title = element_text(size=15))+stat_smooth(method = 'lm')+theme_bw()
ggplot(train_data, aes(x=GrLivArea,y=SalePrice))+geom_point(shape=19, size=2, colour='tan')+ggtitle("X: 지상면적 Y : 판매가격")+theme(plot.title = element_text(size=15))+stat_smooth(method = 'lm')+theme_bw()
ggplot(train_data, aes(x=FullBath,y=SalePrice))+geom_point(shape=19, size=2, colour='yellowgreen')+ggtitle("X: 화장실개수 Y : 판매가격")+theme(plot.title = element_text(size=15))+stat_smooth(method = 'lm')+theme_bw()
ggplot(train_data, aes(x=TotRmsAbvGrd,y=SalePrice))+geom_point(shape=19, size=2, colour='steelblue3')+ggtitle("X: 모든 방의 개수 Y : 판매가격")+theme(plot.title = element_text(size=15))+stat_smooth(method = 'lm')+theme_bw()
ggplot(train_data, aes(x=GarageArea,y=SalePrice))+geom_point(shape=19, size=2, colour='tan2')+ggtitle("X: 주차장 면적 Y : 판매가격")+theme(plot.title = element_text(size=15))+stat_smooth(method = 'lm')+theme_bw()
ggplot(train_data, aes(x=YearBuilt,y=SalePrice))+geom_point(shape=19, size=2, colour='violetred')+ggtitle("X: 설립년도 Y : 판매가격")+theme(plot.title = element_text(size=15))+stat_smooth(method = 'lm')+theme_bw()

# 10. 이상치 결측치 제거 후 상관관계 분석

cor(train_data)
plot(train_data)

train_data.cor<-cor(train_data)
train_data

corrplot(train_data.cor, method = "number")

#
Price_LotF<-lm(train_data$SalePrice~train_data$LotFrontage,data=train_data)
Price_LotA<-lm(train_data$SalePrice~train_data$LotArea,data=train_data)
Price_TotalB<-lm(train_data$SalePrice~train_data$TotalBsmtSF,data=train_data)
Price_GrLiv<-lm(train_data$SalePrice~train_data$GrLivArea,data=train_data)
Price_FullB<-lm(train_data$SalePrice~train_data$FullBath,data=train_data)
Price_TotR<-lm(train_data$SalePrice~train_data$TotRmsAbvGrd,data=train_data)
Price_Garage<-lm(train_data$SalePrice~train_data$GarageArea,data=train_data)
Price_YearB<-lm(train_data$SalePrice~train_data$YearBuilt,data=train_data)
