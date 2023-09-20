import numpy as numpy
import math


# calculate the Entropy of a dataset with label
def Entropy(Y):
    # count the number of samples
    n_samples = len(Y)
    # calculate the entropy of the system
    n_sampleszero = 0
    n_samplesone = 0
    for i in range(n_samples):
        if (Y[i] == 0):
            n_sampleszero += 1
    n_samplesone = n_samples - n_sampleszero
    p_one = n_samplesone/n_samples
    p_zero = 1 - p_one
    if (p_zero == 0 ) or (p_one == 0):
       entropy = 0
    else:
        entropy = -(p_one*math.log(p_one,2) + p_zero*math.log(p_zero,2))

    return entropy


# function to calculate the Information Gain of entire dataset
def IG(X,Y):
    # count the number of samples
    n_samples = len(Y)
    # count the number of features
    n_features = len(X[0])
    # get the entropy of the whole system
    entro = Entropy(Y)

    # list of IG for each features
    ig = []

    # calculate the sub entropy each feature
    for i in range(n_features):
        # split the data deu to the features
        featureOne = []
        featureZero = []
        for j in range(n_samples):
            if (X[j][i] == 0):
                featureZero.append(j)
            else:
                featureOne.append(j)
        
        # processing the value 0 of this feature
        labelZero = 0
        # labelfeatureZero = []
        for k in range(len(featureZero)):
            if (Y[featureZero[k]] == 0):
                labelZero += 1
        p_zero_featureZero = labelZero/len(featureZero)
        p_one_featureZero = 1 - p_zero_featureZero

        # processing the value 1 of this feature
        labelOne = 0
        for k in range(len(featureOne)):
            if (Y[featureOne[k]] == 1):
                labelOne += 1
        p_one_featureOne = labelOne/len(featureOne)
        p_zero_featureOne = 1 - p_one_featureOne

        
        if (p_zero_featureZero == 0 ) or (p_one_featureZero == 0):
            subEntropy1 = 0
        else:
            subEntropy1 = -p_zero_featureZero*math.log(p_zero_featureZero,2) - p_one_featureZero*math.log(p_one_featureZero,2)

        if (p_zero_featureOne == 0 ) or (p_one_featureOne == 0):
            subEntropy2 = 0
        else:
            subEntropy2 = -(p_zero_featureOne*math.log(p_zero_featureOne,2) + p_one_featureOne*math.log(p_one_featureOne,2))

        # sum of all sub Entropy
        sumEntro = (subEntropy1*len(featureZero) + subEntropy2*len(featureOne))/n_samples
        
        # Information Gain of this feature
        inforGain = entro - sumEntro
        ig.append(inforGain)

    return ig # return the list of IG of all features     

# function to calculate the Information Gain of one feature
def IGonefeature(X1,Y1, feature):
    # count the number of samples
    n_samples = len(Y1)

    # get the entropy of the whole system
    entro = Entropy(Y1)

    ################## calculate the sub entropy of the feature

    # split the data due to the features
    featureOne = []
    featureZero = []
    for j in range(n_samples):
        if (X[j][feature] == 0):
            featureZero.append(j)
        else:
            featureOne.append(j)
    
    # processing the value 0 of this feature
    labelfeatureZero = []
    for k in range(len(featureZero)):
        labelfeatureZero.append(Y[featureZero[k]])
    # entropy of featureZero dataset
    entrofeatureZero = Entropy(labelfeatureZero)
    if (entrofeatureZero == 0):
        print("The branch Zero of feature ", feature, " is completed")

    labelfeatureOne = []
    for k in range(len(featureOne)):
        labelfeatureOne.append(Y[featureOne[k]])
    # entropy of featureOne dataset
    entrofeatureOne = Entropy(labelfeatureOne)
    if (entrofeatureOne == 0):
        print("The branch One of feature ", feature, " is completed")

    # sum of all sub Entropy
    sumEntro = (entrofeatureZero*len(featureZero) + entrofeatureOne*len(featureOne))/n_samples
    
    # Information Gain of this feature
    inforGain = entro - sumEntro

    return inforGain # return IG value of the feature     




def split_data(datatoSplit, dataLabel, featuretosplit):

    # get the data length
    n_samples = len(Y)
    # split data due to the featuretosplit
    featureOne = []
    featureZero = []
    for j in range(n_samples):
        if (X[j][featuretosplit] == 0):
            featureZero.append(j)
        else:
            featureOne.append(j)
    return featureOne, featureZero


# the decision tree which returns the features order to split
def DT_train_binary(X, Y, max_depth):

    # count the number of samples
    n_samples = len(Y)
    # count the number of features
    n_features = len(X[0])
    if (max_depth==-1):

    # else:



    return n_features #a decision tree - set of feature order to ask



# the test function
# def DT_test_binary(X, Y, DT):



# def DT_train_binary_best(X_train, Y_train, X_val, Y_val):
    

# test Gini values
# print(gini_index([[[1, 1], [1, 0]], [[1, 1], [1, 0]]], [0, 1]))

X = numpy.array([[0,0,1], [1,1,1], [0,1,0], [0,1,1], [1,0,0], [1,1,1], [1,1,0], [0,0,0], [1,1,0], [1,0,1]])
# Y = numpy.array([[0], [1], [1], [0], [1], [1], [0], [0], [0], [1]])
Y1 = numpy.array([[0], [0], [0], [0], [1], [1], [0], [0], [0], [1]])
max_depth = 2

# print(DT_train_binary(X, Y, max_depth))
# print(IG(X, Y1))