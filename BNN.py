# Global Variables
import math
import random
import string

n_X = 0
n_Y = 0
n_H = 0
W_xh =0
W_hy =0
theta_h =0
theta_y =0

random.seed(0)





# calculate a random number where:  a <= rand < b
def rand(a, b):
    return (b-a)*random.random() + a


def initMatrix(I, J, fill=0.0):
    m = []
    for i in range(I):
        m.append([fill]*J)
    return m

def initiate_BNN ():
    global W_xh, W_hy, theta_h, theta_y

    W_xh = initMatrix(n_X, n_H)
    W_hy = initMatrix(n_H, n_Y)
    theta_h = [0.0] * n_H
    theta_y = [0.0] * n_Y

    for j in range(n_H):
        theta_h[j] = rand(-1.0, 1.0)
        for i in range(n_X):
            W_xh[i][j] = rand(-1.0, 1.0)

    for j in range(n_Y):
        theta_y[j] = rand(-1.0, 1.0)
        for i in range(n_H):
            W_hy[i][j] = rand(-1.0, 1.0)

    return

def training(samples, I=1000, learning_rate = 10.0):
    initiate_BNN()


    for i in range(I):
        for sample in samples:
            sample_X = sample[0]
            sample_Y = sample[1]
            error = backPropagate(sample_X, sample_Y, learning_rate)

    return


def backPropagate(sample_X, sample_Y, learning_rate):
    global W_xh, W_hy, theta_h, theta_y

    A_h = [0.0] * n_H
    for h in range(n_H):
        sum = theta_h[h]
        for i in range(n_X):
            sum += sample_X[i] * W_xh[i][h]
        A_h[h] = 1/(1+math.exp(-1*sum))

    A_y = [0.0] * n_Y
    for j in range(n_Y):
        sum = theta_y[j]
        for h in range(n_H):
            sum += A_h[h] * W_hy[h][j]
        A_y[j] = 1/(1+math.exp(-1*sum))

    delta_Y = [0.0] * n_Y
    for j in range(n_Y):
        delta_Y[j] = A_y[j] * (1.0 - A_y[j]) * (sample_Y[j] - A_y[j])

    delta_H = [0.0] * n_H
    for h in range(n_H):
        sum = 0.0
        for j in range(n_Y):
            sum += W_hy[h][j] * delta_Y[j]
        delta_H[h] = A_h[h] * (1 - A_h[h]) * sum

    for j in range(n_Y):
        theta_y[j] += learning_rate * delta_Y[j]
        for h in range(n_H):
            W_hy[h][j] += learning_rate * delta_Y[j] * A_h[h]

    for h in range(n_H):
        theta_h[h] += learning_rate * delta_H[h]
        for i in range(n_X):
            W_xh[i][h] += learning_rate * delta_H[h] * sample_X[i]

    return

def predict(samples):
    for sample in samples:
        sample_X = sample[0]
        A_h = [0.0] * n_H
        for h in range(n_H):
            sum = theta_h[h]
            for i in range(n_X):
                sum += sample_X[i] * W_xh[i][h]
            A_h[h] = 1/(1+math.exp(-1*sum))

        A_y = [0.0] * n_Y
        for j in range(n_Y):
            sum = theta_y[j]
            for h in range(n_H):
                sum += A_h[h] * W_hy[h][j]
            A_y[j] = 1/(1+math.exp(-1*sum))

        print sample_X, '->', A_y
    return

def demo():
    global n_X, n_Y, n_H

    samples = [
        [[0,0], [0]],
        [[0,1], [1]],
        [[1,0], [1]],
        [[1,1], [0]]
    ]
    n_X = 2
    n_Y = 1
    n_H = 2

    training(samples)
    predict(samples)

if __name__ == '__main__':
    demo()

