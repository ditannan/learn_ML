setwd("F:/数据及程序/chapter5/示例程序")
library(forecast)
library(fUnitRoots)
Data <- read.csv("./data/arima_data.csv", header = TRUE)[, 2]
sales <- ts(Data)
plot.ts(sales, xlab = "时间", ylab = "销量 / 元")
# 单位根检验
unitrootTest(sales)
# 自相关图
acf(sales)
# 一阶差分
difsales <- diff(sales)
plot.ts(difsales, xlab = "时间", ylab = "销量残差 / 元")
# 自相关图
acf(difsales)
# 单位根检验
unitrootTest(difsales)
# 白噪声检验
Box.test(difsales, type="Ljung-Box") 
# 偏自相关图
pacf(difsales)
# ARIMA(1,1,0)模型
arima <- arima(sales, order = c(1, 1, 0))
arima
forecast <- forecast.Arima(arima, h = 5, level = c(99.5))
forecast