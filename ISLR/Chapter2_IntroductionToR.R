
# Chapter 2: Introduction to R --------------------------------------------

library("ggplot2")
library("ISLR")
library("magrittr")

# 创建矩阵
(x <- matrix(c(1, 2, 3, 4), nrow = 2))
sqrt(x)
# 生成随机数
(x <- rnorm(50))
y <- x + rnorm(50, mean = 50, sd = .1)
cor(x, y)
# 绘制散点图
ggplot(NULL, aes(x, y)) + geom_point()
# 生成序列
(x <- seq(1, 10))
(y <- 1 : 10)

f <- outer(x, y, function(x, y) cos(y) / (1 + x ^ 2))
# 等高线图
contour(x, y, f, nlevels = 45, add = T)
fa <- (f - t(f)) / 2
contour(x, y, fa, nlevels = 15)
# 热地图
image(x, y, fa)
# 三维图
persp(x, y, fa, theta = 30, phi = 20) ## theta, phi控制观看角度

data(Auto)
hist(Auto$mpg)
# equivalent to
Auto %>% 
  ggplot(aes(mpg)) +
  geom_histogram(fill = 'white', colour = 'black', bins = 9)
pairs(Auto)
Auto %$% 
  plot(horsepower, mpg)
Auto %$%
  identify(horsepower, mpg, name)