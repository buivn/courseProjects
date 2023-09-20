import numpy as np

def Distance(X,Y):
    i = 0
    total = 0
    while(i < len(X)):
        total = total + pow((X[i] - Y[i]),2)
        i = i + 1
    return total**2

def KNN_test(X_train,Y_train,X_test,Y_test,K):
    i = 0
    all_distances = []
    #This gets all the distances and stores it into all_distances
    while (i < len(X_test)):
        j = 0
        distances = []
        while (j < len(X_train)):
            distances.append([Distance(X_train[j],X_test[i]),j])
            j = j + 1
        distances.sort()
        all_distances.append(distances[0:K])
        i = i + 1
    
    i = 0
    all_labels = []
    #This gets all the labels and puts them in all_labels
    while(i < len(X_test)):
        j = 0
        label = 0
        while (j <  K):
            label = label  + Y_train[all_distances[i][j][1]]
            j = j +1
        if(label != 0):
            label = label/(abs(label))
            all_labels.append(label)
        else : 
            all_labels.append(1)
        i = i + 1
        
    i = 0
    num_Correct = 0
    while(i < len(X_test)):
        if (all_labels[i] == Y_test[i] ):
            num_Correct = num_Correct + 1
        i = i + 1
    accuracy = num_Correct/(len(X_test))
    return accuracy

def choose_K(X_train,Y_train,X_val,Y_val):
    best_acc = 0
    k = 0
    i = 1
    while(i < len(X_train)):
        acc = KNN_test(X_train,Y_train,X_val,Y_val, i)
        if(acc > best_acc):
            best_acc = acc
            k = i
        i = i + 2
    return k, best_acc

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