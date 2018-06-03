library(TSA)
setwd("F:/数据及程序/chapter5/示例程序")
Data <- read.csv("./data/arima_data.csv", header = TRUE)[, 2]
sales <- ts(Data)
plot.ts(sales, xlab = "时间", ylab = "销量 / 元")
# 一阶差分
difsales <- diff(sales)
# BIC图
res <- armasubsets(y = difsales, nar = 5, nma = 5, y.name = 'test',
                   ar.method = 'ols')
plot(res)