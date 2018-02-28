
# Chapter2_Exercise -------------------------------------------------------

library("ISLR")
data("College")
names(College)
# 数据汇总
summary(College)
# 前十列散点图矩阵
pairs(College[, 1 : 10])