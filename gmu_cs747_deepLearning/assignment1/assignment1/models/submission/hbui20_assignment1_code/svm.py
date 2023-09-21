"""Support Vector Machine (SVM) model."""
import os
import numpy as np
import math
from sklearn.preprocessing import OneHotEncoder, LabelEncoder
from kaggle_submission import output_submission_csv
import pickle
from typing import Any, Tuple
import pandas as pd
from sklearn.model_selection import train_test_split

def load_pickle(f: str) -> Any:
  """Load a pickle file.
  Parameters: f: the pickle filename
  Returns: the pickled data
  """
  return pickle.load(f, encoding="latin1")


def load_CIFAR_batch(filename: np.ndarray) -> Tuple[np.ndarray, np.ndarray]:
    """Load a single batch of cifar data.
    Parameters: filename - the pickle filename
    Returns:
        the data, the labels """
    with open(filename, "rb") as f:
      datadict = load_pickle(f)
      X = datadict["data"]
      Y = datadict["labels"]
      X = X.reshape(10000, 3, 32, 32).transpose(0, 2, 3, 1).astype("float")
      Y = np.array(Y)
      return X, Y


def load_CIFAR10(ROOT: str) -> Tuple[np.ndarray, np.ndarray, np.ndarray, np.ndarray]:
    """Load all of cifar data.
    Parameters: ROOT: the root directory containing the data
    Returns: training data, training labels, testing data, testing labels
    """
    xs = []
    ys = []
    for b in range(1, 6):
      f = os.path.join(ROOT, "data_batch_{}".format(b))
      X, Y = load_CIFAR_batch(f)
      xs.append(X)
      ys.append(Y)
    Xtr = np.concatenate(xs)
    Ytr = np.concatenate(ys)
    Xte, Yte = load_CIFAR_batch(os.path.join(ROOT, "test_batch"))
    return Xtr, Ytr, Xte, Yte


def get_CIFAR10_data(
    num_training: int = 49000,
    num_validation: int = 1000,
    num_test: int = 10000,
    subtract_mean: bool = True, ):
    """Load the CIFAR-10 dataset from disk and perform preprocessing to prepare
    it for classifiers. These are the same steps as we used for the SVM, but
    condensed to a single function.

    Parameters:
        num_training: number of training images
        num_validation: number of validation images
        num_test: number of test images
        subtract_mean: whether or not to normalize the data
    Returns:
        the train/val/test data and labels
    """
    # Load the raw CIFAR-10 data
    cifar10_dir = os.path.join("cifar10", "cifar-10-batches-py")
    X_train, y_train, X_test, y_test = load_CIFAR10(cifar10_dir)
    # Subsample the data
    mask = list(range(num_training, num_training + num_validation))
    X_val = X_train[mask]
    y_val = y_train[mask]
    mask = list(range(num_training))
    X_train = X_train[mask]
    y_train = y_train[mask]
    mask = list(range(num_test))
    X_test = X_test[mask]
    y_test = y_test[mask]

    # Normalize the data: subtract the mean image
    if subtract_mean:
        mean_image = np.mean(X_train, axis=0)
        X_train -= mean_image
        X_val -= mean_image
        X_test -= mean_image
    # Transpose so that channels come first
    X_train = X_train.transpose(0, 3, 1, 2).copy()
    X_val = X_val.transpose(0, 3, 1, 2).copy()
    X_test = X_test.transpose(0, 3, 1, 2).copy()
    # Package data into a dictionary
    return X_train, y_train, X_val, y_val, X_test, y_test


class SVM:
  def __init__(self, n_class: int, lr: float, epochs: int, reg_const: float):
    """Initialize a new classifier.

    Parameters:
        n_class: the number of classes
        lr: the learning rate
        epochs: the number of epochs to train for
        reg_const: the regularization constant
    """
    self.w = None  # TODO: change this
    self.alpha = lr
    self.epochs = epochs
    self.reg_const = reg_const
    self.n_class = n_class

  def calc_gradient(self, X_train: np.ndarray, y_train: np.ndarray) -> np.ndarray:
    """Calculate gradient of the svm hinge loss.

    Inputs have dimension D, there are C classes, and we operate on
    mini-batches of N examples.
    Parameters:
      X_train: a numpy array of shape (N, D) containing a mini-batch
          of data
      y_train: a numpy array of shape (N,) containing training labels;
          y[i] = c means that X[i] has label c, where 0 <= c < C
    Returns:
      the gradient with respect to weights w; an array of the same shape
          as w
    """
    # delta_w = np.zeros((self.n_class, len(X_train[0])+1))
    delta_w = np.zeros((self.n_class, len(X_train[0])))
    # delta_w = self.alpha*self.reg_const*self.w*len(y_train)/49000.0
    for i in range(len(X_train)):
      data = np.copy(X_train[i])
      # add bias into the data
      # data = np.append(data, 1.0)
      outputs = self.w.dot(data)
      corect_index = y_train[i]
      outputs_1 = np.copy(outputs)
      outputs_1[corect_index] -= 1.0
      for j in range(self.n_class):
        # if the number of incorrect prediction is more, update the weights:
        if outputs_1[corect_index] < outputs_1[j]:
          delta_w[corect_index] += self.alpha*data
          delta_w[j] -= self.alpha*data  
    delta_w = delta_w/len(y_train)
    return delta_w




  def train(self, X_train: np.ndarray, y_train: np.ndarray):
    """Train the classifier.
    Hint: operate on mini-batches of data for SGD.

    Parameters:
      X_train: a numpy array of shape (N, D) containing training data;
          N examples with D dimensions
      y_train: a numpy array of shape (N,) containing training labels
    """
    # TODO: implement me
    decay_rate = self.alpha/self.epochs
    # self.w = np.ones((self.n_class, len(X_train[0])+1))
    # self.w = np.zeros((self.n_class, len(X_train[0])+1))
    self.w = np.zeros((self.n_class, len(X_train[0])))

    batch_size = 32
    numOfRun = math.ceil(len(y_train)/batch_size)
    for j in range(self.epochs):
      # shuffle the data
      p = np.random.permutation(len(y_train))
      X_train = X_train[p]
      y_train = y_train[p]
      start = 0
      end = batch_size
      for i in range(numOfRun):
        if end < len(y_train):
          data_batch = X_train[start:end]
          label_batch = y_train[start:end]
        else:
          data_batch = X_train[start:len(y_train)]
          label_batch = y_train[start:len(y_train)]

        start += batch_size
        end += batch_size
        self.w -=  self.alpha*self.reg_const*self.w/49000.0
        self.w += self.calc_gradient(data_batch, label_batch)
      self.alpha *= (1.0/(1.0+decay_rate*j))

  def predict(self, X_test: np.ndarray) -> np.ndarray:
    """Use the trained weights to predict labels for test data points.

    Parameters:
      X_test: a numpy array of shape (N, D) containing testing data;
          N examples with D dimensions

    Returns:
      predicted labels for the data in X_test; a 1-dimensional array of
          length N, where each element is an integer giving the predicted
          class.
    """
    # TODO: implement me
    results = np.zeros((len(X_test)), dtype=int)
    for i in range(len(X_test)):
      data = np.copy(X_test[i])
      # data = np.append(data, 1.0)
      result = self.w.dot(data)
      results[i] = np.argmax(result)
    return results


# the main function
if __name__ == '__main__':
  X_train, y_train, X_val, y_val, X_test, y_test = get_CIFAR10_data()
  f_X_train = X_train.flatten()
  f_X_train = f_X_train.reshape(49000, 3072)
  f_X_test = X_test.flatten()
  f_X_test = f_X_test.reshape(10000, 3072)
  f_X_val = X_val.flatten()
  f_X_val = f_X_val.reshape(1000, 3072)
  epochs = 150
  lr = 0.07
  n_class = 10
  reg_const = 0.4

  svm = SVM(n_class, lr, epochs, reg_const) 
  svm.train(f_X_train, y_train)
  
  results = svm.predict(f_X_train)
  res = y_train - results  
  correct = len(np.argwhere(res == 0))/(len(y_train))
  print("The accuracy of X_train: ", correct)

  results = svm.predict(f_X_val)
  res = y_val - results  
  correct = len(np.argwhere(res == 0))/(len(y_val))
  print("The accuracy of X_eval: ", correct)


  results = svm.predict(f_X_test)
  res = y_test - results  
  correct = len(np.argwhere(res == 0))/(len(y_test))
  print("The accuracy of X_test: ", correct)

  output_path = '/home/dzungbui/Dropbox/phd_gmu/cs747_deepLearning/assignments/CS747-assignment1/models/svm_outputs.csv'
  output_submission_csv(output_path, results)


  # res = y_test - results
  # correct = len(np.argwhere(res == 0))
  # print(correct)