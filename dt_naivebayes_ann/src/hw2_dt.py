# homework 2 - CS584 - Introduction to Data Mining - GMU
# author: Hoang-Dung Bui, G01301478

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split
from sklearn.tree import DecisionTreeClassifier
from sklearn.metrics import classification_report, confusion_matrix
# from sklearn.preprocessing import OrdinalEncoder
from sklearn.preprocessing import LabelEncoder
from sklearn import tree

#mathplotlib inline



if __name__ == '__main__':
    data_train = pd.read_csv("/home/bui/Dropbox/phd_gmu/CS584_DM_SP21/assignment2/train_data.csv")
    data_test = pd.read_csv("/home/bui/Dropbox/phd_gmu/CS584_DM_SP21/assignment2/test_data.csv") 

    no_id_data = data_train.drop('id', axis=1)
    only_train_data1 = no_id_data.drop('credit', axis=1) # Drop the label column, so only attribute data
    # print(only_train_data1.head())

    # preprocessing the data - convert string data to ordinal number
    le = LabelEncoder()
    # only_train_data = only_train_data1[only_train_data1.columns['F10']].apply(le.fit_transform)
    only_train_data1['F10'] = le.fit_transform(only_train_data1['F10'])
    only_train_data1['F11'] = le.fit_transform(only_train_data1['F11'])    
    train_label_set = data_train['credit'] # the label set
    # split the data into train_set and test_set
    # data_train, data_test, label_train, label_test = train_test_split(only_data, label_set, test_size=0.20)
    
    decisionTree = DecisionTreeClassifier(criterion='entropy', max_depth=16)	# declare a decision tree classifer
    clf = decisionTree.fit(only_train_data1, train_label_set) # train the decision tree
    print(clf.tree_.max_depth)
    # tree.plot_tree(model)
    # plot.show() 
    # preprocessing the test data
    no_id_test_data = data_test.drop('id', axis=1)
    no_id_test_data['F10'] = le.fit_transform(no_id_test_data['F10'])
    no_id_test_data['F11'] = le.fit_transform(no_id_test_data['F11'])

    # after training, make the prediction
    y_predict = decisionTree.predict(no_id_test_data)


    # print(y_predict)

    # evaluate the algorithm
    # print(confusion_matrix(label_test, y_predict))
    # print(classification_report(label_test, y_predict))

    np.savetxt('decisiom_tree_result16_en.txt', y_predict, fmt="%1d")
