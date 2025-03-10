# -*- coding: utf-8 -*-
"""
Created on 2018.08.04

@author: xzm
"""

import tensorflow as tf
import numpy as np

from tensorflow.examples.tutorials.mnist import input_data


# 创建输入输出占位符
def create_placeholder():
    X = tf.placeholder(tf.float32, shape=(None, 28 * 28))
    Y = tf.placeholder(tf.float32, shape=(None, 10))
    keep_prob = tf.placeholder(tf.float32)
    return X, Y, keep_prob


# 初始化参数
def initialize_parameters():
    W1 = tf.get_variable('W1', [5, 5, 1, 6], initializer=tf.contrib.layers.xavier_initializer())
    b1 = tf.get_variable('b1', [6], initializer=tf.zeros_initializer())

    W2 = tf.get_variable('W2', [5, 5, 6, 16], initializer=tf.contrib.layers.xavier_initializer())
    b2 = tf.get_variable('b2', [16], initializer=tf.zeros_initializer())

    W3 = tf.get_variable('W3', [5, 5, 16, 120], initializer=tf.contrib.layers.xavier_initializer())
    b3 = tf.get_variable('b3', [120], initializer=tf.zeros_initializer())

    W4 = tf.get_variable('W4', [120, 84], initializer=tf.contrib.layers.xavier_initializer())
    b4 = tf.get_variable('b4', [84], initializer=tf.zeros_initializer())

    W5 = tf.get_variable('W5', [84, 10], initializer=tf.contrib.layers.xavier_initializer())
    b5 = tf.get_variable('b5', [10], initializer=tf.zeros_initializer())

    para = {'W1': W1, 'b1': b1, 'W2': W2, 'b2': b2, 'W3': W3, 'b3': b3, 'W4': W4, 'b4': b4, 'W5': W5, 'b5': b5}

    return para


# 前向传播
def forward_propagation(X, para, dropout):
    X = tf.reshape(X, [-1, 28, 28, 1])
    X = tf.pad(X, [[0, 0], [2, 2], [2, 2], [0, 0]])

    c1 = tf.nn.bias_add(tf.nn.conv2d(X, para['W1'], strides=[1, 1, 1, 1], padding='VALID'), para['b1'])
    p2 = tf.nn.max_pool(c1, ksize=[1, 2, 2, 1], strides=[1, 2, 2, 1], padding='VALID')

    c3 = tf.nn.bias_add(tf.nn.conv2d(p2, para['W2'], strides=[1, 1, 1, 1], padding='VALID'), para['b2'])
    p4 = tf.nn.max_pool(c3, ksize=[1, 2, 2, 1], strides=[1, 2, 2, 1], padding='VALID')

    c5 = tf.nn.bias_add(tf.nn.conv2d(p4, para['W3'], strides=[1, 1, 1, 1], padding='VALID'), para['b3'])
    c5 = tf.contrib.layers.flatten(c5)

    f6 = tf.nn.tanh(tf.add(tf.matmul(c5, para['W4']), para['b4']))
    f7 = tf.nn.tanh(tf.add(tf.matmul(f6, para['W5']), para['b5']))

    f7 = tf.nn.dropout(f7, dropout)

    return f7


def lenet_model():
    X, Y, keep_prob = create_placeholder()
    para = initialize_parameters()
    f7 = forward_propagation(X, para, dropout)
    prediction = tf.nn.softmax(f7)

    loss_op = tf.reduce_mean(tf.nn.softmax_cross_entropy_with_logits(logits=f7, labels=Y))
    l2_loss = tf.contrib.layers.apply_regularization(regularizer=tf.contrib.layers.l2_regularizer(l2_lambda),
                                                     weights_list=tf.trainable_variables())
    final_loss = loss_op + l2_loss
    optimizer = tf.train.AdamOptimizer(learning_rate=learning_rate)
    train_op = optimizer.minimize(final_loss)

    correct_pred = tf.equal(tf.argmax(prediction, 1), tf.argmax(Y, 1))
    accuracy = tf.reduce_mean(tf.cast(correct_pred, tf.float32))

    init = tf.global_variables_initializer()

    with tf.Session() as sess:
        sess.run(init)
        X_test = mnist.test.images[:10000]
        Y_test = mnist.test.labels[:10000]
        for step in range(1, num_steps + 1):
            batch_x, batch_y = mnist.train.next_batch(batch_size)
            sess.run(train_op, feed_dict={X: batch_x, Y: batch_y, keep_prob: dropout})
            if step % display_step == 0 or step == 1:
                pre, loss, acc, = sess.run([prediction, loss_op, accuracy],
                                           feed_dict={X: batch_x, Y: batch_y, keep_prob: 1.0})
                print("Step " + str(step) + ", Minibatch loss = ",
                      "{:.4f}".format(loss) + ", Training Accuracy = " + "{:.3f}".format(acc))
            if step % test_step == 0 and step > 10000:
                print("Test step " + str(step) + ": Accuracy: ",
                      sess.run(accuracy, feed_dict={X: X_test, Y: Y_test, keep_prob: 1.0}))


if __name__ == '__main__':
    mnist = input_data.read_data_sets('./data/', one_hot=True)
    batch_size = 128
    learning_rate = 0.01
    display_step = 20
    test_step = 500
    num_steps = 10000
    dropout = 0.5
    l2_lambda = 0.001
    lenet_model()
