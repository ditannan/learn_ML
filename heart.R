# loading 'dplyr' package
library(dplyr)
# url of data
url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"
# read data
heart <- read.csv(url(url), header = FALSE, encoding = 'UTF-8', stringsAsFactors = FALSE)
# heart <- tbl_df(heart)
str(heart)
# variables names
var.name <- c('age', 'sex', 'chest.pain', 'rest.blood', 'cholesterol', 'blood.sugar', 'rest.electro', 
              'max.heartrate', 'angina', 'ST.depre', 'ST.slope', 'vessel.num', 'defect', 'disease.num')
# change variables names of heart-dataset
names(heart) <- var.name
# variables description
var.desc <- c('年龄', '性别', '胸痛类型', '静息血压', '血清胆固醇', '空腹血糖是否大于120mg/dL', '静息心电图显示值',
                  '最大心率', '是否有运动诱导的心绞痛', '运动诱导的ST段压', '峰值运动时ST段斜率', '荧光检查标记的血管数量', '身体缺陷', '疾病状态')
names(var.name) <- var.desc

##### variables type
# var.type <- c(0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 1, 0, 1, 1)
##### loading 'dataMeta' package
# library(dataMeta)
##### linker variables names, description and type
# linker <- build_linker(heart, variable_description = var.desc, variable_type = var.type)
##### build a dictionary
# dict <- build_dict(heart, linker = linker, option_description = NULL,  prompt_varopts = FALSE)
##### check the dict
# knitr::kable(dict, format = 'markdown', caption = 'Data dictionary')
##### add the dictionary as a data attribute
# heart <- incorporate_attr(heart, data.dictionary = dict, main_string = 'This dataset is used to practice decision tree')
# add new attribute
# attr(heart, 'dic') <- dict

# check the first 6 rows of data
head(heart)
# check the structure of heartNew
str(heart)
# check the attributes of heartNew
# attributes(heartNew)

# delete rows of which value=? 
heartNew <- heart %>%
  filter(!defect == '?', !vessel.num == '?')
##### update dict
# dict1 <- build_dict(heartNew, linker = linker, option_description = NULL,  prompt_varopts = FALSE)
##### update attribute
# heartNew <- incorporate_attr(heartNew, data.dictionary = dict1, main_string = 'This dataset is used to practice decision tree')
##### check the new attributes of heartNew
# attributes(heartNew)
# str(heartNew)

# is.disease 
heartNew$disease <- if_else(heartNew$disease.num == 0, 0, 1) %>% as.factor()
##### add label
# attr(heartNew[,"disease"], 'label') <- '是否患心脏病'

# add variables description 
# heartNew.colnames <- colnames(heartNew)
# heartNew.coldesc <- c(var.desc, '是否患心脏病')
# for (i in 1 : length(heartNew)) {
#   attr(heartNew[, heartNew.colnames[i]], 'label') <- heartNew.coldesc[i]
# }
# glimpse the data
glimpse(heartNew)
# check the attributes
attributes(heartNew)
# knitr::kable(summary(heartNew), format = 'markdown')
# loading 'ggplot2' package
library(ggplot2)

# 连续型变量基本描述和可视化
EDA <- function(x) {
  par(mfrow = c(2, 2))        ## 同时显示4个图
  hist(x)                     ## 直方图
  dotchart(x)                 ## 点图
  boxplot(x, horizontal = T)  ## 箱式图
  qqnorm(x); qqline(x)        ## 正态概率图
  par(mfrow = c(1, 1))        ## 恢复单图
}
# age 年龄
EDA(heartNew$age)
summary(heartNew$age)
# cholesterol 血清胆固醇
EDA(heartNew$cholesterol)
# max.heartrate
EDA(heartNew$max.heartrate)
# rest.blood 静息血压
EDA(heartNew$rest.blood)
# sex 性别
table(heartNew$sex)
# chest_pain 胸痛类型
table(heartNew$chest.pain)
# angina 是否有运动诱导的心绞痛
table(heartNew$angina)

# 解释变量和响应变量进一步可视化
# 荧光检查标记的血管数量
heartNew %>% 
  ggplot(aes(vessel.num, rest.blood, color = disease)) + 
  geom_point(size = 3, position = 'jitter') # 随机扰动
#  geom_jitter()
# 添加随机扰动
#heartNew %>% 
#  ggplot(aes(as.numeric(vessel.num) + rnorm(297)*0.1, rest.blood, color = disease)) + 
#  geom_point(size = 3)
# 最大心率
heartNew %>% 
  ggplot(aes(max.heartrate, rest.blood, color = disease)) + 
  geom_point(size = 4)
# 添加性别
heartNew %>%
  ggplot(aes(max.heartrate, age)) + 
  geom_point(aes(colour = disease, shape = factor(sex)), size = 3) +
  scale_shape_discrete(labels = c('female', 'male'))
# 分面
heartNew %>%
  ggplot(aes(max.heartrate, age)) + 
  geom_point(aes(colour = factor(disease)), size = 3) + 
  facet_wrap(~sex) + 
  stat_smooth()

##########################################
## 单个决策树
# 将
library(caret)
cfinal <- 14
# 将数据集切成5等分
folds <- createFolds(y = heartNew[, 'disease'], k = 5)
# delete disease.num variable
heartNew1 <- heartNew %>% 
  select(-disease.num)
# 4个为训练数据集
traindata <- heartNew1[-folds[[1]], ]
# 测试集
testdata <- heartNew1[folds[[1]],]

# rpart
library(rpart)
library(rpart.plot)
ct <- rpart.control(xval = 5, minsplit = 18, cp = 0.01)  ## maxdepth=20
cfit <- rpart(disease~., data = traindata, method = 'class', control = ct)
# 运行结果
pred1 <- predict(cfit, testdata[, -cfinal])
p <- table(ifelse(pred1[, 2] >= 0.5, 1, 0), testdata[, cfinal])
correct <- sum(diag(p))/sum(p)
# write.csv(heartNew1, 'heartNew1.csv')
plot(cfit, margin = 0.1)
text(cfit, use.n = T, all = T, cex = 1)

