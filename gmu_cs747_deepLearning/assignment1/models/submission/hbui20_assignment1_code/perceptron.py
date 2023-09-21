"""Perceptron model."""
import os
import numpy as np
from sklearn.preprocessing import OneHotEncoder
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



class Perceptron:
  def __init__(self, n_class: int, lr: float, epochs: int):
    """Initialize a new classifier.
      Parameters:
      n_class: the number of classes
      lr: the learning rate
      epochs: the number of epochs to train for """
    self.w = None  # TODO: change this
    self.lr = lr
    self.epochs = epochs
    self.n_class = n_class

  def train(self, X_train: np.ndarray, y_train: np.ndarray):       
    """Parameters:
      X_train: a number array of shape (N, D) containing training data;
          N examples with D dimensions
      y_train: a numpy array of shape (N,) containing training labels
    """
    decay_rate = self.lr/self.epochs
    self.w = np.ones((self.n_class, len(X_train[0])+1))

    for j in range(self.epochs):
      for i in range(len(X_train)):
        data = np.copy(X_train[i])
        # add bias into the data
        data = np.append(data, 1.0)
        outputs = self.w.dot(data)
        if np.argmax(outputs) == y_train[i]:
          pass
        else:
          # if the number of incorrect prediction is more, update the weights:
          cor_index = y_train[i]
          # correct_indices = np.argwhere(results >= 0.0)
          for k in range(self.n_class):
            if (outputs[k] >= outputs[cor_index]) and (k !=cor_index):
              self.w[cor_index] += self.lr*data
              self.w[k] -= self.lr*data  
      self.lr *= (1.0/(1.0+decay_rate*j))
        
  def predict(self, X_test: np.ndarray) -> np.ndarray:
    """
    Parameters:
        X_test: a numpy array of shape (N, D) containing testing data;
            N examples with D dimensions
    Returns:
        predicted labels for the data in X_test; a 1-dimensional array of
        length N, where each element is an integer giving the predicted class.
    """
    results = np.zeros((len(X_test)), dtype=int)
    # X_test = X_test/123.0
    for i in range(len(X_test)):
      data = np.copy(X_test[i])
      data = np.append(data, 1.0)
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
  

  epochs = 100
  lr = 0.3
  n_class = 10

  perceptron_ = Perceptron(n_class, lr, epochs)

  perceptron_.train(f_X_train, y_train)
  
  results = perceptron_.predict(f_X_train)
  res = y_train - results  
  correct = len(np.argwhere(res == 0))/(len(y_train))
  print("The accuracy of X_train: ", correct)

  results = perceptron_.predict(f_X_val)
  res = y_val - results  
  correct = len(np.argwhere(res == 0))/(len(y_val))
  print("The accuracy of X_eval: ", correct)


  results = perceptron_.predict(f_X_test)
  res = y_test - results  
  correct = len(np.argwhere(res == 0))/(len(y_test))
  print("The accuracy of X_test: ", correct)

  output_path = '/home/dzungbui/Dropbox/phd_gmu/cs747_deepLearning/assignments/CS747-assignment1/models/perceptron_outputs.csv'
  output_submission_csv(output_path, results)

