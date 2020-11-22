import numpy as np

def KNN_test(X_train, Y_train, X_test, Y_test, K):
  # initialize neighbors array, their label and distance
  nnLabel = np.zeros(K)
  nnDistance = np.zeros(K)

  accuracy = 0.0
  count = 0
  distance = 0

  for j in range(len(X_test)):
    # find a set of nearest neighboor
    for k in range(len(X_train)):
        for m in range(len(X_train[0])):
            distance += (X_train[k][m]-X_test[j][m])**2
        if (k < K):
            nnDistance[k] = distance
            nnLabel[k] = Y_train[k]
        else:     
            maxd, index = maxDistance(nnDistance)
            if (distance < maxd):
                nnDistance[index] = distance
                nnLabel[index] = Y_train[k]
        distance = 0
    # print(nnDistance)
    decision = 0
    # prediction the data type
    for l in range(K):
        decision += nnLabel[l]
    if (decision*Y_test[j] > 0.0):
        count += 1

  return round(count/len(X_test), 2)


def maxDistance(DistanceArray):
    maxd = DistanceArray[0]
    index = 0
    for i in range(len(DistanceArray)):
        if (maxd < DistanceArray[i]):
            index = i
            maxd = DistanceArray[i]
    return maxd, index


def choose_K(X_train, Y_train, X_val, Y_val):
    
  best_K = 0
  best_accuracy = 0.0
  acc = 0.0

  for i in range(len(X_train)):
    acc = KNN_test(X_train, Y_train, X_val, Y_val, 2*i+1)
    if (acc > best_accuracy):
      best_K = 2*i+1
      best_accuracy = acc
  
  return best_K, best_accuracy




if __name__ == '__main__':
    

  X_train = np.array([[1,5], [2,6], [2,7], [3,7], [3,8], [4,8],[5,1], [5,9], [6,2], [7,2], [7,3], [8,3], [8,4], [9,5]])
  
  Y_train = np.array([[-1],[-1],[1],[-1],[1],[-1],[1],[-1],[1],[-1], [1],[-1], [1],[1]])

  X_test = np.array([[1,1], [2,1], [0,10], [10,10], [5,5], [3,10], [9,4], [6,2], [2,2], [8,7]])
  Y_test = np.array([[1], [-1], [1], [-1], [1], [-1], [1], [-1], [1], [-1]])
  # print(Y_test)

  # acc = KNN_test(X_train,Y_train,X_test,Y_test,5)
  # print("The accuracy is: ", acc*100, "%")
  K1, acc1 = choose_K(X_train,Y_train, X_test, Y_test)
  print("For this data, the best K is: ", K1, "with accuracy ", acc1*100, "%")


  