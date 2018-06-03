# 设置工作空间
# 把“数据及程序”文件夹拷贝到F盘下，再用setwd设置工作空间
setwd("F:/数据及程序/chapter8/示例程序")
library(arules)
# 读入数据
a <- read.csv("./data/processedfile.csv", header = TRUE) # 读入数据
trans <- as(a, "transactions")  # 将数据转换为transactions格式
inspect(trans[1:5])  # 观测trans数据集中前5行数据items
# 调用Apriori算法
rule <- apriori(trans, parameter = list(support = 0.06, confidence = 0.75)) 
# 生成关联规则rules
rules  # 显示rules中关联规则条数
inspect(rules)  # 观测rules中关联规则
