import numpy as np
import matplotlib.pyplot as plt
from sklearn.datasets import make_moons
import neural_network as nn

np.random.seed(0)

def plot_decision_boundary(pred_func, X, y):
    x_min, x_max = X[:, 0].min() - .5, X[:, 0].max() + .5
    y_min, y_max = X[:, 1].min() - .5, X[:, 1].max() + .5
    h = .01
    xx, yy = np.meshgrid(np.arange(x_min, x_max, h), np.arange(y_min, y_max, h))
    Z = pred_func(np.c_[xx.ravel(), yy.ravel()])
    Z = Z.reshape(xx.shape)
    plt.contourf(xx, yy, Z, cmap=plt.cm.Spectral)
    plt.scatter(X[:, 0] , X[:, 1] , c=y, cmap=plt.cm.Spectral)

X, y = make_moons (200 , noise =0.20)

plt.scatter(X[:, 0] , X[:, 1] , s=40, c=y, cmap=plt.cm.Spectral)
plt.show()

plt.figure()
hidden_layer_dimensions = [1, 2, 4, 5]
# hidden_layer_dimensions = [10, 20, 50, 100]

for i, nn_hdim in enumerate(hidden_layer_dimensions):
    plt.subplot(2, 2, i+1, aspect='equal')
    plt.title('Hidden Layer Size : '+str(nn_hdim))
    model = nn.build_model(X, y, nn_hdim, 30000, True)
    plot_decision_boundary(lambda x: nn.predict(model, x), X, y)

plt.show()
