
# 设置工作空间
# 把“数据及程序”文件夹拷贝到F盘下，再用setwd设置工作空间
setwd("F:/数据及程序/chapter5/示例程序")
# 读入数据
Data <- read.csv("./data/consumption_data.csv", header = TRUE)[, 2:4]
km <- kmeans(Data, center = 3)
print(km)
km$size / sum(km$size)
# 数据分组
aaa <- data.frame(Data, km$cluster)
Data1 <- Data[which(aaa$km.cluster == 1), ]
Data2 <- Data[which(aaa$km.cluster == 2), ]
Data3 <- Data[which(aaa$km.cluster == 3), ]
# 客户分群“1”的概率密度函数图
par(mfrow = c(1,3))
plot(density(Data1[, 1]), col = "red", main = "R")
plot(density(Data1[, 2]), col = "red", main = "F")
plot(density(Data1[, 3]), col = "red", main = "M")
# 客户分群“2”的概率密度函数图
par(mfrow = c(1, 3))
plot(density(Data2[, 1]), col="red", main = "R")
plot(density(Data2[, 2]), col="red", main = "F")
plot(density(Data2[, 3]), col="red", main = "M")
# 客户分群“3”的概率密度函数图
par(mfrow = c(1, 3))
plot(density(Data3[, 1]), col="red", main = "R")
plot(density(Data3[, 2]), col="red", main = "F")
plot(density(Data3[, 3]), col="red", main = "M")
