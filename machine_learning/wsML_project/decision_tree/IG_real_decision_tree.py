import numpy as np
import math
import pandas as pd


class Node:
    def __init__(self):
        self.split = None   # which feature will be splitted on (Taste)
        self.splitValue = None
        self.leaf = False   # check whether it is leaf node or not
        self.left = None    # represents left and right child nodes (both are null for a leaf node)
        self.right = None
        self.result = None  # tell the output is true or false at this node -> use to predict the output value for new data
        self.entropy = None       # Entropy of this node
        self.X = None    # Data at this node
        self.Y = None
        self.level=0        # represent depth of the tree

    def __repr__(self):
        return '['+self.split+'  '+str(self.split)+']'


class DecisionTree:
    #takes data and target column
    def __init__(self, data, label='E', depth=None, limitA=5.8, limitB=2.9, limitC=2.5, limitD=0.9):
        self.data = data
        self.label = label
        self.root = None
        self.depth = depth      # maximum depth allowed
        # self.v = v          # v = 1 to see teh tree building process
        self.limitA = limitA
        self.limitB = limitB
        self.limitC = limitC
        self.limitD = limitD

        
    # calculate the Entropy of a dataset with label
    def Entropy(self, data, label):
        # count the number of samples
        n_samples = len(data.INDEX)
        if n_samples == 0:
            print('no samples to calculate entropy')
            return 0
        # calculate the entropy of the system
        
        n_pos = len(data[data[label]=='positive'])
        # n_neg = len(data[data[label]=='negative'])

        p_pos = n_pos/n_samples
        p_neg = 1 - p_pos
        if (p_pos == 0 ) or (p_neg == 0):
           entropy = 0
        else:
            entropy = -p_pos*math.log(p_pos,2) - p_neg*math.log(p_neg,2)

        # print('The entropy of the current dataset: ', round(entropy, 4))
        return round(entropy, 4)


    # function to calculate the Information Gain of entire dataset
    def IG(self, data, label):
        # count the number of samples
        n_samples = len(data.INDEX)
        # count the number of features
        n_features = len(data.columns) - 2
        print('number of positive and negative: ', n_samples, n_features)
        # get the entropy of the whole system
        entro = self.Entropy(data, label)

        # list of IG for each features
        ig = []
        # calculate the sub entropy each feature
        for feature in data.columns:
            # not split on label or index colums
            if feature == label or feature == 'INDEX':
                # print(feature)
                continue
            # print(feature)
            # split the data due to the features
            dataLeft, dataRight = self.split_data(data, feature)

    #         # check whether any data subset is empty?
    #         if (len(featureZero) == 0) or (len(featureOne) == 0):
    #             inforGain = 0
    #         else:
    #             # processing the value 0 of this feature
    #             label_Zero = 0
    #             # labelfeatureZero = []
    #             for k in range(len(labelZero)):
    #                 if (labelZero[k] == 0):
    #                     label_Zero += 1

    #             p_zero_featureZero = float(label_Zero)/float(len(labelZero))
    #             p_one_featureZero = 1 - p_zero_featureZero

    #             # processing the value 1 of this feature
    #             label_One = 0
    #             for m in range(len(labelOne)):
    #                 if (labelOne[m] == 1):
    #                     label_One += 1
    #             p_one_featureOne = float(label_One)/float(len(featureOne))
    #             p_zero_featureOne = 1.0 - p_one_featureOne
                
    #             if (p_zero_featureZero == 0 ) or (p_one_featureZero == 0):
    #                 subEntropy1 = 0
    #             else:
    #                 subEntropy1 = -(p_zero_featureZero*math.log(p_zero_featureZero,2) + p_one_featureZero*math.log(p_one_featureZero,2))

    #             if (p_zero_featureOne == 0 ) or (p_one_featureOne == 0):
    #                 subEntropy2 = 0
    #             else:
    #                 subEntropy2 = -(p_zero_featureOne*math.log(p_zero_featureOne,2) + p_one_featureOne*math.log(p_one_featureOne,2))

    #             # sum of all sub Entropy
    #             sumEntro = (subEntropy1*float(len(featureZero)) + subEntropy2*float(len(featureOne)))/n_samples
            
    #             # Information Gain of this feature
    #             inforGain = entro - sumEntro

    #         # print('The Information Gain: ', inforGain)
            
    #         ig.append(round(inforGain,3))
    #     # convert ig to array
    #     ig = np.array(ig)

    #     return ig # return the list of IG of all features

    def split_data(self, data, feature):
        # get the data length
        n_samples = len(data.INDEX)
        # split data due to the featuretosplit
        limit = 0.0
        if feature == 'A':
            limit = self.limitA
        elif feature == 'B':
            limit = self.limitB
        elif feature == 'C':
            limit = self.limitC
        else:
            limit = self.limitD

        # df = np.isclose(data[feature], limit)
        # unique = data[feature].unique()
        # print(unique)
        # lvals = [unique[x] ]

        # for  i in range(n_samples):
            
        # data[feature] = data[feature].astype(float)
        # leftData = data[data[feature].astype(float) < limit]
        # print(leftData)
        # rightData = data[(df[feature] >= limit)]
        # print(rightData)


        return 0 #leftData, rightData

     
    # #This method builds the Decision tree
    # def build(self, X, Y, level=0):
    #     if self.v==1:
    #         print('======================================')
    #         print('Building Tree for the data at level ', level)
    #         print('======================================')
    #         print(X)
    #         print(Y)
    #     max_IG = None
    #     best_feature = None

        
    #     # declare a node -------------
    #     node = Node()
    #     node.X = X
    #     node.Y = Y
    #     node.level = level
    #     node.entropy = self.Entropy(Y)
    #     print('The entropy of the current dataset: ', node.entropy)
        
    #     # Check whether it meets the limit to stop building tree ------------
    #     if (self.depth is not None) and (level==self.depth):
    #         node.leaf = True
    #         if (2*np.count_nonzero(Y==1) > len(Y)):
    #             node.result = 1
    #         else:
    #             node.result = 0
    #         return node
    #     #If there is no impurity in this data, make it a leaf node
    #     if (node.entropy == 0):
    #         # if self.v==1:
    #         print('The data is pure, no split is needed ')
    #         node.leaf = True
    #         if (2*np.count_nonzero(Y==1) > len(Y)):
    #             node.result = 1
    #         else:
    #             node.result = 0
    #         return node
    #     # ------------------------------------------------------------------
            
    #     # adding a new node to the tree ++++++++++++++++++++++++++++++++++++
    #     # find feature to split by IG
    #     ig = self.IG(X,Y)
    #     # print('List of IG index for', len(ig), 'features: ', ig)
    #     max_IG = ig[0]
    #     best_feature = 0
    #     for k in range(len(ig)):
    #         if (max_IG < ig[k]):
    #             max_IG = ig[k]
    #             best_feature = k
    #     node.split = best_feature
    #     # if self.v==1:
    #     print('Best split is on feature ',best_feature,' with IG value ', max_IG)
    #     print()
    #     # print()
    #     lset_Data, lset_Label, rset_Data, rset_Label = self.split_data(X, Y, best_feature)
                
    #     #Build tree for left dataset
    #     if (len(lset_Data) == 0) or (len(rset_Data) == 0):
    #         node.leaf = True
    #     #Build tree for right dataset
    #     else:
    #         node.left = self.build(lset_Data,lset_Label, level+1)
    #         # print()
    #         node.right = self.build(rset_Data, rset_Label, level+1)
            
    #     #If both the trees are not built, it has to be leaf
    #     # if node.left==None and node.right==None:
            
        
    #     return node        
        
    
    # def fit(self):
    #     self.root = self.build(self.data, self.label)
    #     return self.root
        
    # def __predict__(self,dataCheck,root):
    #     if root is None:
    #         return False
    #     if root.leaf:
    #         return root.result
    #     if (dataCheck[root.split] == 0):
    #         return self.__predict__(dataCheck,root.left)
    #     else:
    #         return self.__predict__(dataCheck,root.right)
        
        
    # def predict(self,dataCheck):
    #     return self.__predict__(dataCheck,self.root)


# def t(x):
#     if x is None:
#         return ""
#     return str(x)

# def build_tree(root):
#     if root==None:
#         return ""
#     return {'feature split ': t(root.split), 'level ': str(root.level), 'left ': build_tree(root.left), 'right ': build_tree(root.right) }


def DT_train_binary(data, label, max_depth, limitA, limitB, limitC, limitD):
    # create a decision tree class with input as data
    if (max_depth == -1):
        n_features = len(X[0])
        tree = DecisionTree(data, label, n_features, limitA, limitB, limitC, limitD)
    else:
        tree = DecisionTree(data, label, max_depth, limitA, limitB, limitC, limitD)

    # activate to build tree's node for the decision tree
    # root = tree.fit()

    tree.IG(data, label)
    
    # print()
    # print("Print out the decision tree on the screen")
    # g = build_tree(root)
    # g = str(g).replace('\'','"')
    # g = g.replace('["','')
    # g = g.replace('"]','')
    # print(g)

    return 0 #root



# def DT_make_prediction(x, DT):
#     if DT is None:
#         return False
#     if DT.leaf:
#         return DT.result
#     if x[DT.split] == 1:
#         return DT_make_prediction(x, DT.right)
#     return DT_make_prediction(x, DT.left)


# # test function - returning the accurracy
# def DT_test_binary(X_test, Y_test, DT):
#     score = 0
#     for i in range(len(X_test)):
#         pred = DT_make_prediction(X_test[i], DT)
#         # pred = tree.predict(X_test[i])
#         if pred and Y_test[i]:
#             score+=1
#         elif not pred and not Y_test[i]:
#             score+=1
#     acc = float(score*100)/float(len(X_test))
#     print('Accuracy on training data is ', round(acc,2), '%')
#     return round(acc,2)

# # return a tree with best accurracy
# def DT_train_binary_best(X_train, Y_train, X_val, Y_val):
#     decisionTree = DT_train_binary(X_train, Y_train, -1)
#     best_acc = DT_test_binary(X_val, Y_val, decisionTree)

#     print("The highest accurracy of this decision Tree is ", best_acc, '%')

#     return decisionTree



if __name__ == '__main__':
    

    # t = newTable("Data Table", 16, 6)
    # t.setColName(col, "INDEX", "A", "B", "C", "D", "E")
    # print(t)
    dataset = {'INDEX':['1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','16'],
             'A':['4.8','5','5','5.2','5.2','4.7','4.8','5.4','7','6.4','6.9','5.5','6.5','5.7','6.3','4.9'],
             'B':['3.4','3','3.4','3.5','3.4','3.2','3.1','3.4','3.2','3.2','3.1','2.3','2.8','2.8','3.3','2.4'],
             'C':['1.9','1.6','1.6','1.5','1.4','1.6','1.6','1.5','4.7','4.7','4.9','4','4.6','4.5','4.7','3.3'],
             'D':['0.2','1.2','0.2','0.2','0.2','0.2','0.2','0.4','1.4','1.5','1.5','1.3','1.5','1.3','1.6','1'],
             'E':['positive','positive','positive','positive','positive','positive','positive','positive','negative','negative','negative','negative','negative','negative','negative','negative']}

    df = pd.DataFrame(dataset,columns=['INDEX','A','B','C','D','E'])

    # print(df)

    max_depth = 5


    # build and print out the IG binary decision tree - 
    limitA = 5.8
    limitB = 2.9
    limitC = 2.5
    limitD = 0.9
    DT = DT_train_binary(df, 'E' , max_depth, limitA, limitB, limitC, limitD)

    # calculate the accuracy of the decision tree
    # acc = DT_test_binary(X_test2, Y_test2, DT)

    # DT = DT_train_binary(X_invi_last, Y_invi_last, max_depth)

    # acc = DT_test_binary(X_invi_test, Y_invi_test, DT)

    # print('For the last traini data group and second test sample')
    # # Answer = DT_make_prediction(X_test[1], DT)
    # if (Answer == 1):
    #     print('The answer is: YES')
    # else:
    #     print('The answer is: NO')

    # dt1 = DT_train_binary_best(X_train2, Y_train, X_val2, Y_val2)
    
