
# loading packages --------------------------------------------------------

library(tree)
library(ISLR)

# loading data ------------------------------------------------------------

# 将Sales分为两类
head(Carseats)
hist(Carseats$Sales)
Carseats$High <- ifelse(Carseats$Sales <= 8, 'No', 'Yes')  ## 此处不是因子型
Carseats$High <- as.factor(Carseats$High)


# 建立分类树 -------------------------------------------------------------------

tree.carseats <- tree(High ~ .-Sales, data = Carseats)
summary(tree.carseats)
# 绘制树图
plot(tree.carseats)
text(tree.carseats, pretty = 0) ## pretty=0使R输出所有定性预测变量的类别名，而不是首字母
# 树的细节
tree.carseats

# 训练测试集
# 划分测试集
set.seed(2)
train <- sample(1 : nrow(Carseats), 200)
Carseats.test <- Carseats[-train, ]
High.test <- Carseats$High[-train]
# 训练集上
tree.carseats <- tree(High ~.-Sales, data = Carseats, subset = train)
# 在测试集上的预测值
tree.pred <- predict(tree.carseats, Carseats.test, type = 'class') ## 返回预测类别
table(tree.pred, High.test)
mean(tree.pred == High.test)

# 使用交叉验证得到最佳节点个数
set.seed(3)
cv.carseats <- cv.tree(tree.carseats, FUN = prune.misclass)
names(cv.carseats)
cv.carseats ## k为每棵树的终端节点个数，相应的分类错误率，复杂性参数k
# 错误率对size和k的函数
par(mfrow = c(2, 1))
plot(cv.carseats$size, cv.carseats$dev, type = 'b')
plot(cv.carseats$k, cv.carseats$dev, type = 'b')

# 9个节点最佳，剪枝得到9个节点树
prune.carseats <- prune.misclass(tree.carseats, best = 9)
plot(prune.carseats)
text(prune.carseats)
# 在测试集上的预测值
tree.pred <- predict(prune.carseats, Carseats.test, type = 'class')
table(tree.pred, High.test)
mean(tree.pred == High.test)

# 构建回归树 -------------------------------------------------------------------

Boston <- MASS::Boston
set.seed(1)
train <- sample(1 : nrow(Boston), nrow(Boston) / 2)
tree.boston <- tree(medv ~ ., data = Boston, subset = train)
summary(tree.boston)
plot(tree.boston)
text(tree.boston, pretty = 0)
# 剪枝
cv.boston <- cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type = 'b')
prune.boston <- prune.tree(tree.boston, best = 5)
plot(prune.boston)
text(prune.boston)
# 使用未剪枝的树对测试集进行预测
yhat <- predict(tree.boston, newdata = Boston[-train, ])
plot(yhat, Boston[-train, 'medv'])
abline(0, 1)
# 测试均方误差
mean((yhat - Boston[-train, 'medv']) ^ 2) ## 平方根为5.005，预测与真值的中位数之差为5005美元内

# Bagging -----------------------------------------------------------------

# 装袋法是随机森林在m=p时的特例
library(randomForest)
set.seed(1)
bag.boston <- randomForest(medv ~ ., data = Boston, subset = train, mtry = 13, importance = TRUE)
bag.boston
# 在测试集上效果
yhat.bag <- predict(bag.boston, newdata = Boston[-train, ])
plot(yhat.bag, Boston$medv[-train])
abline(0, 1)
mean((yhat.bag - Boston$medv[-train]) ^ 2) ## 均方误差为最好的单颗树的一半

# ntree可改变生成的树的数目
bag.boston <- randomForest(medv ~ .,data = Boston, subset = train, mtry = 13, ntree = 25)
yhat.bag <- predict(bag.boston, newdata = Boston[-train, ])
mean((yhat.bag - Boston$medv[-train]) ^ 2)

# randomForest ------------------------------------------------------------

# 只是mtry值小一些，回归默认是p/3，分类是sqrt(p)
rf.boston <- randomForest(medv ~ ., data = Boston, subset = train, mtry = 6, importance = TRUE)
yhat.rf <- predict(rf.boston, newdata = Boston[-train, ])
mean((yhat.rf - Boston$medv[-train]) ^ 2) ## 比装袋法有所提升
# 查看变量重要性
importance(rf.boston)
varImpPlot(rf.boston)

# boosting ----------------------------------------------------------------

library(gbm)
set.seed(1)
boost.boston <- gbm(medv ~ ., data = Boston[-train, ], distribution = 'gaussian', 
                    n.trees = 5000, interaction.depth = 4)
# 输出相对影像图
summary(boost.boston)

par(mfrow = c(1, 2))
plot(boost.boston, i = 'rm')
plot(boost.boston, i = 'lstat')

# 在测试集上效果
yhat.boost <- predict(boost.boston, newdata = Boston[-train, ], n.trees = 5000)
mean((yhat.boost - Boston$medv[-train]) ^ 2) ## 测试均方误差更小了

# 改变学习率lambda
boost.boston <- gbm(medv ~ ., data = Boston[-train, ], distribution = 'gaussian',
                    n.trees = 5000, interaction.depth = 4, shrinkage = 0.2, verbose = F)
yhat.boost <- predict(boost.boston, newdata = Boston[-train, ], n.trees = 5000)
mean((yhat.boost - Boston$medv[-train]) ^ 2)
