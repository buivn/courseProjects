import numpy as np

def perceptron_train(X, Y):
  w = np.zeros(len(X[0]))
  bias = 1.0
  running = True
  count = 0
  a = 0.0

  while (running):

    for i in range(len(X)):
      for j in range(len(X[0])):
        a += w[j]*X[i][j]
      a += bias
      if not (a*Y[i] > 0):
        for k in range(len(X[0])):
          w[k] += X[i][k]*Y[i]
        bias += Y[i]
      else:
        count += 1
      a = 0
    # check the update of weight in an epoch.
    if (count == len(X)):
      running = False
    count = 0

  print("The returned weights ", w, " and bias: ", bias)     
  return w, bias

def perceptron_test(X_test, Y_test, w, b):
    
  accuracy = 0.0
  correct = 0.0
  a = 0.0

  for i in range(len(X_test)):
    for j in range(len(w)):
      a += w[j]*X_test[i][j]
    a += b
    if (a*Y_test[i] > 0):
      correct += 1.0
    a = 0.0

  accuracy = float(correct)/float(len(Y_test))

  return accuracy




if __name__ == '__main__':
    

  X = np.array([[-2,1], [1,1], [1.5,-0.5], [-2,-1], [-1,-1.5], [2,-2],[-2,1], [1,1], [1.5,-0.5]])
  Y = np.array([[1],[1],[1],[-1],[-1],[-1],[1],[1],[1]])

  W = perceptron_train(X,Y)
  test_acc = perceptron_test(X,Y,W[0],W[1])
  print("The accuracy: ", test_acc*100, "%")


  