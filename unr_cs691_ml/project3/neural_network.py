import numpy as np


def unique_classes(y):
    cl = {}
    for i in y:
        if i not in cl:
            cl[i] = 1

    return cl


def recast(y):
    cl = unique_classes(y)
    y_ = np.zeros((y.shape[0], len(cl)))
    for i in range(y.shape[0]):
        y_[i][y[i]] = 1
    return np.array(y_)


def calculate_loss(model, X, y):
    N = X.shape[0]
    cl = unique_classes(y)
    y_ = recast(y)
    L = 0
    for i in range(X.shape[0]):
        x = X[i]
        a = x@model['W1'] + model['b1']
        h = np.tanh(a)
        z = h@model['W2'] + model['b2']
        y_hat = np.exp(z)
        y_hat /= np.sum(y_hat)
        for k in y_hat:
            for j in cl:
                L -= y_[i][j]*np.log(k[j])
    return L/N


def predict(model, x):
    a = x@model['W1'] + model['b1']
    h = np.tanh(a)
    z = h@model['W2'] + model['b2']
    y_hat = np.exp(z)
    y_hat /= np.sum(y_hat)
    return np.argmax(y_hat, axis=1)


def back_prop(model, X, y, eta=.001):
    a = X@model['W1'] + model['b1']
    h = np.tanh(a)
    z = h@model['W2'] + model['b2']
    y_hat = np.exp(z)
    y_hat /= np.sum(y_hat, axis=1)[:, None]
    dl_dy = y_hat - recast(y)
    dl_da = np.multiply((1 - h*h), dl_dy@np.transpose(model['W2']))
    model['W1'] -= eta*np.transpose(X)@dl_da
    model['W2'] -= eta*np.transpose(h)@dl_dy
    model['b1'] -= eta*np.ones((1, X.shape[0]))@dl_da
    model['b2'] -= eta*np.ones((1, X.shape[0]))@dl_dy
    return model


def build_model(X, y, nn_hdim, num_passes=20000, print_loss=False):
    # data type: dictionary - consist of key and values
    model = {"W1": np.random.random((X.shape[1], nn_hdim)), "W2": np.random.random((nn_hdim, X.shape[1])), \
        "b1": np.random.random((1, nn_hdim)), "b2":np.random.random((1, len(unique_classes(y))))}
    # print(X.shape[1])
    for i in range(num_passes):
        model = back_prop(model, X,  y)
        # if(print_loss and i%4000 == 0):
        #     print("Loss with hidden layer's size ", nn_hdim, "is:", calculate_loss(model, X, y))
    if(print_loss):
        print("Loss - hid. layer's size", nn_hdim, "is:", calculate_loss(model, X, y))
    return model