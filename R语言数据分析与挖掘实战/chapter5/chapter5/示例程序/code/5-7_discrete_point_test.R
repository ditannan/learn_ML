# 设置工作空间
# 把“数据及程序”文件夹拷贝到F盘下，再用setwd设置工作空间
setwd("F:/数据及程序/chapter5/示例程序")
# 读入数据
Data <- read.csv("./data/consumption_data.csv", header = TRUE)[, 2:4]
Data <- scale(Data)
set.seed(12)
km <- kmeans(Data, center = 3)
print(km)
km$centers
# 各样本欧氏距离
x1 <- matrix(km$centers[1, ], nrow = 940, ncol =3 , byrow = T)
juli1 <- sqrt(rowSums((Data - x1) ^ 2))
x2 <- matrix(km$centers[2, ], nrow = 940, ncol =3 , byrow = T)
juli2 <- sqrt(rowSums((Data - x2) ^ 2))
x3 <- matrix(km$centers[3, ], nrow = 940, ncol =3 , byrow = T)
juli3 <- sqrt(rowSums((Data - x3) ^ 2))
dist <- data.frame(juli1, juli2, juli3)
# 欧氏距离最小值
y <- apply(dist, 1, min)
plot(1:940, y, xlim = c(0, 940), xlab = "样本点", ylab = "欧氏距离")
points(which(y > 2.5), y[which(y > 2.5)], pch = 19, col = "red")
