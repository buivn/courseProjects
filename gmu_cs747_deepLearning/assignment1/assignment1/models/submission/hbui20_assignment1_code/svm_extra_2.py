"""Support Vector Machine (SVM) model."""
import os
import numpy as np
import math
from sklearn.model_selection import train_test_split
from sklearn.datasets import make_gaussian_quantiles
from matplotlib import pyplot
from sklearn.svm import SVC


# the main function
if __name__ == '__main__':
  
  # generate 2d non-linearly separatable dataset
  X1, Y1 = make_gaussian_quantiles(n_features=2, n_classes=2, n_samples=800,random_state=5)
  pyplot.scatter(X1[:, 0], X1[:, 1], marker='o', c=Y1,
            s=25, edgecolor='k')
  pyplot.show()

  # # convert to binary labels
  binary_Y1 = np.where(Y1 == 0, -1, Y1)

  X_train, X_test, y_train, y_test = train_test_split(X1, binary_Y1, test_size = 0.20)

  svclassifier = SVC(kernel='poly', degree=8)
  svclassifier.fit(X_train, y_train)

  y_pred = svclassifier.predict(X_test)

  y_pred = y_pred*y_test

  correct = len(np.argwhere(y_pred >0))

  accuracy = correct/len(y_pred)
  print(accuracy)
