import numpy as np
import matplotlib.pyplot as plt
import helpers

def svm_train_brute(training_data):
    # w for data 1,2,3,4
    w = np.array([[1,0], [0,1], [1,-1], [1,1]])
    # w1 = np.array([1,0])    # vertical line
    # for data 2
    # w2 = np.array([0,1])  # horizontal line
    # for data 3 
    # w3 = np.array([1,-1])
    # for data 4 
    # w4 = np.array([1,1])
    b = np.array([-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10])
    sup_vector_neg = np.zeros
    margin = np.zeros((4,21)).astype(int)
    for i in range(len(w)):
        for j in range(len(b)):
            margin[i][j] = compute_margin(training_data, w[i],b[j])


    return 0

def distance_point_to_hyperplan(pt, w, b):
    distance = 0
    # assuming the line is in the form: w1x + w2y + b = 0
    numerator = np.absolute(w[0]*pt[0] + w[1]*pt[1] + b)
    denominator = np.sqrt(np.square(w[0])+np.square(w[1]))
    distance = numerator/denominator

    return distance

def compute_margin(data, w, b):
    # the margin from the point to the boundary
    inputData = np.array([0,0])
    sup_vector_pos = np.array([0,0])
    sup_vector_neg = np.array([0,0])
    min_negative = 0
    min_positive = 0
    check_side = 0
    for i in range(len(data)):
        inputData[0] = data[i][0]
        inputData[1] = data[i][1]
        distance = distance_point_to_hyperplan(inputData, w, b)
        check_side = w[0]*data[i][0]+w[1]*data[i][1]+b
        if (data[i][2] == 1):
            if (check_side < 0):
                print("The boundary does not classified the data")
                return 0 
            if ((distance < min_positive) or (min_positive==0)):
                min_positive = distance
                sup_vector_pos[0] = data[i][0]
                sup_vector_pos[1] = data[i][1]

        if (data[i][2] == -1):
            if (check_side > 0):
                print("The boundary does not classified the data")
                return 0 
            if ((distance < min_negative) or (min_negative == 0)):
                min_negative = distance
                sup_vector_neg[0] = data[i][0]
                sup_vector_neg[1] = data[i][1]
    return min_negative+min_positive, sup_vector_neg, sup_vector_pos

def svm_test_brute(w,b,x):
    classified = w[0]*x[0] + w[1]x[1] + b
    if classified > 0:
        return 1
    if classified < 0:
        return -1

if __name__ == '__main__':

    # pt = np.array([0,-1])
    # w = np.array([1,1])
    # b = 1
    # dis = distance_point_to_hyperplan(pt,w,b)
    # print(dis)
    
    data1 = helpers.generate_training_data_binary(1)
    # for data 1 - > w = [1,0]    # vertical line
    # for data 2 - > w = [0,1]    # horizontal line
    # for data 3 - > w = [1,-1]
    # for data 4 - > w = [1,1]
    draw = helpers.plot_training_data_binary(data1)
