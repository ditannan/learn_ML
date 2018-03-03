## -*- coding: utf-8 -*-
## codes in chapter 4 of ISL about classification

# loading packages and data------------------------------------------------

library("ISLR")
library("magrittr")
library("dplyr")
library("ggplot2")

# glimpse data
glimpse(Smarket)
# 计算相关系数矩阵
cor(Smarket[-9])
# volume散点图
# plot(Smarket$Volume)
Smarket$n <- seq(nrow(Smarket))
Smarket %>% 
  ggplot(aes(n, Volume)) +
  geom_point()

## 股票市场数据
# logistic regression -----------------------------------------------------

glm.logis <- Smarket %$% 
  glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, family = binomial)
summary(glm.logis)
# 拟合模型系数
coef(glm.logis)
summary(glm.logis)$coef
# get p value
summary(glm.logis)$coef[, 4]

# 预测概率
glm.probs <- predict(glm.logis, type = 'response') ## 预测训练数据集
# 将预测概率转化为类别
glm.pred <- rep('Down', nrow(Smarket))
glm.pred[glm.probs > .5] <- 'Up'
# 预测和原始比较
table(glm.pred, Smarket$Direction)
# 训练集预测率
mean(glm.pred == Smarket$Direction)

# 选择训练集和测试集
train <- Smarket$Year < 2005
Smarket.2005 <- Smarket[!train, ]
Smarket.train <- Smarket[train, ]
# 使用测试集拟合模型
glm.logis.train <- Smarket.train %$%
  glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, family = binomial)
# glm.logis.train <- update(glm.logis, data = Smarket.train)
# 预测测试集
glm.probs.test <- predict(glm.logis.train, Smarket.2005, type = 'response')
glm.pred.test <- rep('Down', nrow(Smarket.2005))
glm.pred.test[glm.probs.test > .5] <- 'Up'
table(glm.pred.test, Smarket.2005$Direction)
# 测试集预测率
mean(glm.pred.test == Smarket.2005$Direction)

# 选取p值最小的两个变量进行预测
glm.logis.two.train <- Smarket.train %$%
  glm(Direction ~ Lag1 + Lag2, family = binomial)
# 预测测试集
glm.probs.two.test <- predict(glm.logis.two.train, Smarket.2005, type = 'response')
glm.pred.two.test <- rep('Down', nrow(Smarket.2005))
glm.pred.two.test[glm.probs.two.test > .5] <- 'Up'
# 预测率
mean(glm.pred.two.test == Smarket.2005$Direction)

# LDA ---------------------------------------------------------------------

# 载入函数
lda <- MASS::lda
# 拟合LDA
lda.fit <- Smarket %$% 
  lda(Direction ~ Lag1 + Lag2, subset = train)
lda.fit
# 线性判别图像
plot(lda.fit)
lda.pred <- predict(lda.fit, Smarket.2005)
str(lda.pred)
# 测试集预测率
table(lda.pred$class, Smarket.2005$Direction)
mean(lda.pred$class == Smarket.2005$Direction)
# 后验概率大于.5个数
sum(lda.pred$posterior[, 1] > .5)

# QDA ---------------------------------------------------------------------

# 载入函数
qda <- MASS::qda
qda.fit <- Smarket %$% 
  qda(Direction ~ Lag1 + Lag2, subset = train)
qda.fit
qda.pred <- predict(qda.fit, Smarket.2005)
str(qda.pred)
# 预测情况
table(qda.pred$class, Smarket.2005$Direction)
mean(qda.pred$class == Smarket.2005$Direction)

# KNN ---------------------------------------------------------------------

library("class")
train.x <- Smarket %$% cbind(Lag1, Lag2)[train, ]
test.x <- Smarket %$% cbind(Lag1, Lag2)[!train, ]
train.dir <- Smarket$Direction[train]
# set seed
set.seed(1)
knn.pred <- knn(train.x, test.x, train.dir, k = 1)
# 预测情况
table(knn.pred, Smarket.2005$Direction)
mean(knn.pred == Smarket.2005$Direction)

# k=3模型
knn.pred.3k <- knn(train.x, test.x, train.dir, k = 3)
# 预测情况
table(knn.pred.3k, Smarket.2005$Direction)
mean(knn.pred.3k == Smarket.2005$Direction)


# 大篷车保险数据KNN --------------------------------------------------------------

glimpse(Caravan)
table(Caravan$Purchase) %>% prop.table()
# 标化数据
Caravan.std <- scale(Caravan[, -86])
data.class(Caravan.std)
# 前1000个为测试集
test <- 1 : 1000
train.x <- Caravan.std[-test, ]
test.x <- Caravan.std[test, ]
train.y <- Caravan$Purchase[-test]
test.y <- Caravan$Purchase[test]
set.seed(1)
knn.pred.car <- knn(train.x, test.x, train.y, k = 1)
# 预测情况
# 准确率
mean(test.y == knn.pred.car)
mean(knn.pred.car == 'Yes')
table(knn.pred.car, test.y) ## 预测了9/77购买
# 购买占比例
mean(test.y == 'Yes')

## 当k = 3
knn.pred.car <- knn(train.x, test.x, train.y, k = 3)
# 预测情况
# 准确率
mean(test.y == knn.pred.car)
mean(knn.pred.car == 'Yes')
table(knn.pred.car, test.y) ## 预测了5/26购买

## 当k = 5
knn.pred.car <- knn(train.x, test.x, train.y, k = 5)
# 预测情况
# 准确率
mean(test.y == knn.pred.car)
mean(knn.pred.car == 'Yes')
table(knn.pred.car, test.y) ## 预测了4/15购买

# logistic ----------------------------------------------------------------

glm.fit.car <- glm(Purchase ~ .,data = Caravan, family =binomial, subset = -test)
glm.fit.car.probs <- predict(glm.fit.car, Caravan[test, ], type = 'response')
glm.pred.car <- rep('No', 1000)
glm.pred.car[glm.fit.car.probs > .5] <- 'Yes'
table(glm.pred.car, test.y)

# 阈值改为.25
glm.pred.car[glm.fit.car.probs > .25] <- 'Yes'
# 预测情况
table(glm.pred.car, test.y) ## 11/33
