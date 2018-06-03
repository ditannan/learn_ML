# 设置工作空间
# 把“数据及程序”文件夹拷贝到F盘下，再用setwd设置工作空间
setwd("F:/数据及程序/chapter5/示例程序")
# 读入数据
Data <- read.csv("./data/bankloan.csv")[2:701, ]
# 数据命名
colnames(Data) <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "y")
# logistic回归模型
glm <- glm(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8,
           family = binomial(link = logit), data = Data)
summary(glm)
# 逐步寻优法
logit.step <- step(glm, direction = "both")
summary(logit.step)
# 前向选择法
logit.step <- step(glm, direction = "forward")
summary(logit.step)
# 后向选择法
logit.step <- step(glm, direction = "backward")
summary(logit.step)