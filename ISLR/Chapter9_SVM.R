
# loading packages --------------------------------------------------------

library(ISLR)
library(e1071)

# 生成数据集
set.seed(1)
x <- matrix(rnorm(20 * 2), ncol = 2)
y <- c(rep(-1, 10), rep(1, 10))
x[y == 1,] <- x[y==1, ] + 1
# 绘制散点图
plot(x, col = (3 -y))

# SVM线性分类-----------------------------------------------------------------

# svm分类
data <- data.frame(x = x, y = as.factor(y))
svmfit <- svm(y ~., data = data, kernel = 'linear', cost = 10, scale = FALSE)
plot(svmfit, data = data)
# 支持向量：
svmfit$index
# 拟合基本信息
summary(svmfit)

# 使用较小的cost
svmfit <- svm(y ~., data = data, kernel = 'linear', cost = 0.1, scale = FALSE)
plot(svmfit, data = data) ## 得到更多的支持向量，间隔更宽
svmfit$index

## 自画
make.grid <- function(x, n = 75) {
  grange <- apply(x, 2, range)
  x1 <- seq(from = grange[1, 1], to = grange[2, 1], length = n)
  x2 <- seq(from = grange[1, 2], to = grange[2, 2], length = n)
  expand.grid(x.1 = x1, x.2 = x2)
}
xgrid <- make.grid(x)
ygrid <- predict(svmfit, xgrid)
plot(xgrid, col = c('red', 'blue')[as.numeric(ygrid)], pch = 20, cex = .2)
# 画出各点
points(x, col = y + 3, pch = 19)
# 画出支持向量点
points(x[svmfit$index,], pch = 5, cex = 2)

# 画出分界线
beta <- drop(t(svmfit$coefs) %*% x[svmfit$index, ])
beta0 <- svmfit$rho
plot(xgrid, col = c('red', 'blue')[as.numeric(ygrid)], pch = 20, cex = .2)
points(x, col = y + 3, pch = 19)
points(x[svmfit$index,], pch = 5, cex = 2)
abline(beta0 / beta[2], -beta[1] / beta[2])
abline((beta0 - 1) / beta[2], -beta[1] / beta[2], lty = 2)
abline((beta0 + 1) / beta[2], -beta[1] / beta[2], lty = 2)

# 使用内置函数进行交叉验证
set.seed(1)
tune.out <- tune(svm, y ~ ., data = data, kernel = 'linear', 
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)
# 获得最好模型
bestmod <- tune.out$best.model
summary(bestmod)
# 生成测试数据集
xtest <- matrix(rnorm(20 * 2), ncol = 2)
ytest <- sample(c(-1, 1), 20, replace = TRUE)
xtest[ytest == 1, ] <- xtest[ytest == 1, ] + 1
testdata <- data.frame(x = xtest, y = as.factor(ytest))
# 进行预测
ypred <- predict(bestmod, testdata)
table(predict = ypred, truth = testdata$y) ## 有一个被误分

# 看看cost=0.01会怎样
svmfit <- svm(y ~ ., data = data, kernel = 'linear', cost = 0.01, scale = FALSE)
ypred <- predict(svmfit, testdata)
table(predict = ypred, truth = testdata$y) ## 有两个被误分


# 线性可分情况
x[y == 1,] <- x[y == 1,] + 0.5
plot(x, col = (y + 5) / 2, pch = 19)

data <- data.frame(
  x = x,
  y = as.factor(y)
)
svmfit <- svm(y ~ ., data = data, kernel = 'linear', cost = 1e5)
summary(svmfit)
plot(svmfit, data = data) # 全部分类正确，但间隔很窄

# 使用更小的cost，间隔会变宽
svmfit <- svm(y ~ ., data = data, kernel = 'linear', cost = 1)
summary(svmfit)
plot(svmfit, data = data) # 一个被误分，但间隔更宽，在测试集上表现会更好



# SVM非线性分类 ----------------------------------------------------------------

# 使用kernel='polynomial'拟合多项式核函数的SVM，需要设定degree值指定阶数
# 使用kernel='radial'拟合径向基核函数的SVM，需要设定gamma参数
set.seed(1)
x <- matrix(rnorm(200 * 2), ncol = 2)
x[1 : 100,] <- x[1 : 100,] + 2 ## 前100个点往右上走
x[101 : 150, ] <- x[101 : 150, ] - 2  ## 中间50个点往左下走，剩下的中间的50在中间
y <- c(rep(1, 150), rep(2, 50))
data <- data.frame(
  x = x,
  y = as.factor(y)
)

# 划分训练集测试集
train <- sample(200, 100)
svmfit <- svm(y ~ ., data = data[train, ], kernel = 'radial', gamma = 1, cost = 1)
plot(svmfit, data = data[train,]) ## 非线性边界
summary(svmfit)

# 增大cost可以减少误分数，但边界更加不规则，容易造成过拟合
svmfit <- svm(y ~ ., data = data[train,], kernal = 'radial', gamma = 1, cost = 1e5)
plot(svmfit, data = data[train,])

# 使用交叉验证来选择最优gamma值和cost值
set.seed(1)
tune.out <- tune(svm, y ~ ., data = data[train, ], kernel = 'radial', 
                 ranges = list(cost = c(0.1, 1, 10 , 100, 1000), 
                               gamma = c(0.5, 1, 2, 3, 4)))
summary(tune.out)

bestmod <- tune.out$best.model
summary(bestmod)
# 测试集上效果
pred <- predict(bestmod, data[-train, -3])
table(true = data$y[-train], pred)

