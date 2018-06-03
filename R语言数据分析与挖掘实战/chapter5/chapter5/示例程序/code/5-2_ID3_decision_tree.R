# 设置工作空间
# 把“数据及程序”文件夹拷贝到F盘下，再用setwd设置工作空间
setwd("F:/数据及程序/chapter5/示例程序")
# 读入数据
data <- read.csv("./data/sales_data.csv")[, 2:5]
# 数据命名
colnames(data) <- c("x1", "x2", "x3", "result")
# 计算一列数据的信息熵
calculateEntropy <- function(data) {
  t <- table(data)    
  sum <- sum(t)       
  t <- t[t != 0]        
  entropy <- -sum(log2(t / sum) * (t / sum))
  return(entropy)
}
# 计算两列数据的信息熵
calculateEntropy2 <- function(data) {
  var <- table(data[1])
  p <- var/sum(var)
  varnames <- names(var)
  array <- c()
  for (name in varnames) {
    array <- append(array, calculateEntropy(subset(data, data[1] == name,
                                                   select = 2)))
  }
  return(sum(array * p))
}

buildTree <- function(data) {
  if (length(unique(data$result)) == 1) {
    cat(data$result[1])
    return()
  }
  if (length(names(data)) == 1) {
    cat("...")
    return()
  }
  entropy <- calculateEntropy(data$result)  
  labels <- names(data)
  label <- ""
  temp <- Inf
  subentropy <- c()
  for (i in 1:(length(data) - 1)) {
    temp2 <- calculateEntropy2(data[c(i, length(labels))])
    if (temp2 < temp) {         
      temp <- temp2          
      label <- labels[i]      
    }
    subentropy <- append(subentropy,temp2)  
  }
  cat(label)
  cat("[")
  nextLabels <- labels[labels != label]
  for (value in unlist(unique(data[label]))) {
    cat(value,":")
    buildTree(subset(data,data[label] == value, select = nextLabels))
    cat(";")
  }
  cat("]")
}
# 构建分类树
buildTree(data)
