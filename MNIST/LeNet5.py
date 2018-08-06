# -*- coding: utf-8 -*-

import tensorflow as tf
import numpy as np

# 输入数据格式
x = tf.placeholder(tf.float32, [
    BATCH_SIZE,  # batch中样例个数
    mnist_inference.IMAGE_SIZE,  # 图片尺寸
    mnist_inference.IMAGE_SIZE,
    mnist_inference.NUM_CHANNELS  # 深度
], name='x_input')

# 将输入数据格式化
reshaped_xs = np.reshape(xs, (
    BATCH_SIZE,  # batch中样例个数
    mnist_inference.IMAGE_SIZE,  # 图片尺寸
    mnist_inference.IMAGE_SIZE,
    mnist_inference.NUM_CHANNELS  # 深度
))

# 参数
INPUT_NODE = 784
OUTPUT_NODE = 10

IMAGE_SIZE = 28
NUM_CHANNELS = 1
NUM_LABELS = 10

# 第一层卷积层的尺寸和深度
CONV1_DEEP = 32
