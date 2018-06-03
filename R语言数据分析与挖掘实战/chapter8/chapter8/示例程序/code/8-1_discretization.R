# 设置工作空间
# 把“数据及程序”文件夹拷贝到F盘下，再用setwd设置工作空间
setwd("F:/数据及程序/chapter8/示例程序")
# 读取数据  
datafile <- read.csv("./data/data.csv",header=T) 
# 参数初始化
type <- 4  # 数据离散化的分组个数
index <- 8  # TNM分期数据所在列
typelabel <- c("A", "B", "C", "D", "E", "F")  # 数据离散化后的标识前缀
set.seed(1234)  # 固定随机化种子
cols <- ncol(datafile[, 1:6])  # 取六种证型列数
rows <- nrow(datafile[, 1:6])  # 行数
disdata <- matrix(NA, rows, cols + 1)  # 初始化

# 聚类离散化
for (i in 1:cols) {
  cl <- kmeans(datafile[, i], type, nstart = 20)  # 对单个属性列进行聚类
  disdata[, i] <- paste(typelabel[i], cl$cluster) 
}

disdata[, cols + 1] <- datafile[,index]
disdata[, cols + 1] <- paste("H", disdata[, cols + 1], seq = "")

# 导出数据
colnames(disdata) <- c("肝气郁结证型系数", "热毒蕴结证型系数", 
                       "冲任失调证型系数", "气血两虚证型系数", 
                       "脾胃虚弱证型系数", "肝肾阴虚证型系数", "TNM分期")
write.csv(disdata, file = "./tmp/processedfile.csv", quote = F, row.names = F)
