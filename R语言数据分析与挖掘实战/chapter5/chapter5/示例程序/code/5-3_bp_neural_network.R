# 设置工作空间
# 把“数据及程序”文件夹拷贝到F盘下，再用setwd设置工作空间
setwd("F:/数据及程序/chapter5/示例程序")
# 读入数据
Data <- read.csv("./data/sales_data.csv")[, 2:5]
# 数据命名
library(nnet)
colnames(Data) <- c("x1", "x2", "x3", "y")
# 最终模型
model1 <- nnet(y ~ ., data = Data, size = 6, decay = 5e-4, maxit = 1000)  

pred <- predict(model1, Data[, 1:3], type = "class")	
(P <- sum(as.numeric(pred == Data$y)) / nrow(Data))
table(Data$y, pred)
prop.table(table(Data$y, pred), 1)