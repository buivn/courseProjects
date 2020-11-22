import numpy as np
import math

class Node:
    def __init__(self):
        self.split = None   # which feature will be splitted on (Taste)
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
    def __init__(self, X, Y, depth=None, v=0):
        self.data = X
        self.label = Y
        self.root = None
        self.depth = depth      # maximum depth allowed
        self.v = v          # v = 1 to see teh tree building process
        
    # calculate the Entropy of a dataset with label
    def Entropy(self, Y):
        # count the number of samples
        n_samples = len(Y)
        if n_samples == 0:
            print('no samples to calculate entropy')
            return 0
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
            entropy = -p_one*math.log(p_one,2) - p_zero*math.log(p_zero,2)

        # print('The entropy of the current dataset: ', round(entropy, 4))
        return round(entropy, 4)


    # function to calculate the Information Gain of entire dataset
    def IG(self, X,Y):
        # count the number of samples
        n_samples = len(self.label)
        # count the number of features
        n_features = len(self.data[0])
        # get the entropy of the whole system
        entro = self.Entropy(Y)

        # list of IG for each features
        ig = []
        # calculate the sub entropy each feature
        for i in range(n_features):
            # split the data due to the features
            featureZero, labelZero, featureOne, labelOne = self.split_data(X, Y, i)

            # check whether any data subset is empty?
            if (len(featureZero) == 0) or (len(featureOne) == 0):
                inforGain = 0
            else:
                # processing the value 0 of this feature
                label_Zero = 0
                # labelfeatureZero = []
                for k in range(len(labelZero)):
                    if (labelZero[k] == 0):
                        label_Zero += 1

                p_zero_featureZero = float(label_Zero)/float(len(labelZero))
                p_one_featureZero = 1 - p_zero_featureZero

                # processing the value 1 of this feature
                label_One = 0
                for m in range(len(labelOne)):
                    if (labelOne[m] == 1):
                        label_One += 1
                p_one_featureOne = float(label_One)/float(len(featureOne))
                p_zero_featureOne = 1.0 - p_one_featureOne
                
                if (p_zero_featureZero == 0 ) or (p_one_featureZero == 0):
                    subEntropy1 = 0
                else:
                    subEntropy1 = -(p_zero_featureZero*math.log(p_zero_featureZero,2) + p_one_featureZero*math.log(p_one_featureZero,2))

                if (p_zero_featureOne == 0 ) or (p_one_featureOne == 0):
                    subEntropy2 = 0
                else:
                    subEntropy2 = -(p_zero_featureOne*math.log(p_zero_featureOne,2) + p_one_featureOne*math.log(p_one_featureOne,2))

                # sum of all sub Entropy
                sumEntro = (subEntropy1*float(len(featureZero)) + subEntropy2*float(len(featureOne)))/n_samples
            
                # Information Gain of this feature
                inforGain = entro - sumEntro

            # print('The Information Gain: ', inforGain)
            
            ig.append(round(inforGain,3))
        # convert ig to array
        ig = np.array(ig)

        return ig # return the list of IG of all features

    def split_data(self, data, Label, feature):
        # get the data length
        n_samples = len(Label)
        # split data due to the featuretosplit
        leftData = []
        leftLabel = []
        rightData = []
        rightLabel = []
        for j in range(n_samples):
            if (data[j][feature] == 0):
                leftData.append(data[j])
                leftLabel.append(Label[j])
            
            if (data[j][feature] == 1):
                rightData.append(data[j])
                rightLabel.append(Label[j])
        # convert back to array
        leftData = np.array(leftData)
        leftLabel = np.array(leftLabel)
        rightData = np.array(rightData)
        rightLabel = np.array(rightLabel)

        return leftData, leftLabel, rightData, rightLabel

     
    #This method builds the Decision tree
    def build(self, X, Y, level=0):
        if self.v==1:
            print('======================================')
            print('Building Tree for the data at level ', level)
            print('======================================')
            print(X)
            print(Y)
        max_IG = None
        best_feature = None

        
        # declare a node -------------
        node = Node()
        node.X = X
        node.Y = Y
        node.level = level
        node.entropy = self.Entropy(Y)
        print('The entropy of the current dataset: ', node.entropy)
        
        # Check whether it meets the limit to stop building tree ------------
        if (self.depth is not None) and (level==self.depth):
            node.leaf = True
            if (2*np.count_nonzero(Y==1) > len(Y)):
                node.result = 1
            else:
                node.result = 0
            return node
        #If there is no impurity in this data, make it a leaf node
        if (node.entropy == 0):
            # if self.v==1:
            print('The data is pure, no split is needed ')
            node.leaf = True
            if (2*np.count_nonzero(Y==1) > len(Y)):
                node.result = 1
            else:
                node.result = 0
            return node
        # ------------------------------------------------------------------
            
        # adding a new node to the tree ++++++++++++++++++++++++++++++++++++
        # find feature to split by IG
        ig = self.IG(X,Y)
        # print('List of IG index for', len(ig), 'features: ', ig)
        max_IG = ig[0]
        best_feature = 0
        for k in range(len(ig)):
            if (max_IG < ig[k]):
                max_IG = ig[k]
                best_feature = k
        node.split = best_feature
        # if self.v==1:
        print('Best split is on feature ',best_feature,' with IG value ', max_IG)
        print()
        # print()
        lset_Data, lset_Label, rset_Data, rset_Label = self.split_data(X, Y, best_feature)
                
        #Build tree for left dataset
        if (len(lset_Data) == 0) or (len(rset_Data) == 0):
            node.leaf = True
        #Build tree for right dataset
        else:
            node.left = self.build(lset_Data,lset_Label, level+1)
            # print()
            node.right = self.build(rset_Data, rset_Label, level+1)
            
        #If both the trees are not built, it has to be leaf
        # if node.left==None and node.right==None:
            
        
        return node        
        
    
    def fit(self):
        self.root = self.build(self.data, self.label)
        return self.root
        
    def __predict__(self,dataCheck,root):
        if root is None:
            return False
        if root.leaf:
            return root.result
        if (dataCheck[root.split] == 0):
            return self.__predict__(dataCheck,root.left)
        else:
            return self.__predict__(dataCheck,root.right)
        
        
    def predict(self,dataCheck):
        return self.__predict__(dataCheck,self.root)


def t(x):
    if x is None:
        return ""
    return str(x)

def build_tree(root):
    if root==None:
        return ""
    return {'feature split ': t(root.split), 'level ': str(root.level), 'left ': build_tree(root.left), 'right ': build_tree(root.right) }


def DT_train_binary(X,Y,max_depth):
    # create a decision tree class with input as data
    if (max_depth == -1):
        n_features = len(X[0])
        tree = DecisionTree(X, Y, n_features,v=1)
    else:
        tree = DecisionTree(X, Y, max_depth,v=1)

    # activate to build tree's node for the decision tree
    root = tree.fit()
    
    print()
    print("Print out the decision tree on the screen")
    g = build_tree(root)
    g = str(g).replace('\'','"')
    g = g.replace('["','')
    g = g.replace('"]','')
    print(g)

    return root



def DT_make_prediction(x, DT):
    if DT is None:
        return False
    if DT.leaf:
        return DT.result
    if x[DT.split] == 1:
        return DT_make_prediction(x, DT.right)
    return DT_make_prediction(x, DT.left)


# test function - returning the accurracy
def DT_test_binary(X_test, Y_test, DT):
    score = 0
    for i in range(len(X_test)):
        pred = DT_make_prediction(X_test[i], DT)
        # pred = tree.predict(X_test[i])
        if pred and Y_test[i]:
            score+=1
        elif not pred and not Y_test[i]:
            score+=1
    acc = float(score*100)/float(len(X_test))
    print('Accuracy on training data is ', round(acc,2), '%')
    return round(acc,2)

# return a tree with best accurracy
def DT_train_binary_best(X_train, Y_train, X_val, Y_val):
    decisionTree = DT_train_binary(X_train, Y_train, -1)
    best_acc = DT_test_binary(X_val, Y_val, decisionTree)

    print("The highest accurracy of this decision Tree is ", best_acc, '%')

    return decisionTree



if __name__ == '__main__':
    

    X_train1 = np.array([[0,1], [0,0], [1,0], [0,0], [1,1]])
    Y_train1 = np.array([[1],[0],[0],[0],[1]])
    X_val1 = np.array([[0,0], [0,1], [1,0], [1,1]])
    Y_val1 = np.array([[0], [1], [0], [1]])
    X_test1 = np.array([[0,0], [0,1], [1,0], [1,1]])
    Y_test1 = np.array([[1], [1], [0], [1]])

    X_train2 = np.array([[0,1,0,0], [0,0,0,1], [1,0,0,0], [0,0,1,1], [1,1,0,1], [1,1,0,0], [1,0,0,1], [0,1,0,1], [0,1,0,0]])
    Y_train2 = np.array([[0],[1],[0],[0],[1],[0],[1],[1],[1]])
    
    X_val2 = np.array([[1,0,0,0], [0,0,1,1], [1,1,0,1], [1,1,0,0], [1,0,0,1], [0,1,0,0]])
    Y_val2 = np.array([[0], [0], [1], [0], [1], [1]])
    X_test2 = np.array([[0,1,0,0], [0,0,0,1], [1,0,0,0], [0,0,1,1], [1,1,0,1], [1,1,0,0], [1,0,0,1], [0,1,0,1], [0,1,0,0]])
    Y_test2 = np.array([[0],[1],[0],[0],[1],[0],[1],[1],[1]])

    # Data for friend's invitation
    X_invi = np.array([[1,1,1,0,1,1,0], [0,0,1,1,0,1,1], [0,1,0,0,1,0,0], [1,1,0,1,0,0,1], [1,0,1,0,1,1,1], [1,1,0,1,1,0,1], [1,1,0,0,1,1,0], [0,0,0,1,0,1,1]])
    Y_invi = np.array([[1],[1],[0],[1],[0],[1],[1],[0]])


    X_invi_first = np.array([[1,1,1,0,1,1,0], [0,0,1,1,0,1,1], [0,1,0,0,1,0,0], [1,1,0,1,0,0,1], [1,0,1,0,1,1,1]])
    Y_invi_first = np.array([[1],[1],[0],[1],[0]])

    X_invi_last = np.array([[1,1,0,1,0,0,1], [1,0,1,0,1,1,1], [1,1,0,1,1,0,1], [1,1,0,0,1,1,0], [0,0,0,1,0,1,1]])
    Y_invi_last = np.array([[1],[0],[1],[1],[0]])

    X_invi_middle = np.array([[0,1,0,0,1,0,0], [1,1,0,1,0,0,1], [1,0,1,0,1,1,1], [1,1,0,1,1,0,1], [1,1,0,0,1,1,0]])
    Y_invi_middle = np.array([[0],[1],[0],[1],[1]])

    X_invi_test = np.array([[0,1,1,1,0,1,0], [0,1,1,1,0,0,1], [1,0,0,1,1,0,0]])
    Y_invi_test = np.array([[0],[1],[0]])   

    max_depth = 5


    # build and print out the IG binary decision tree
    DT = DT_train_binary(X_train2, Y_train2, max_depth)

    # calculate the accuracy of the decision tree
    acc = DT_test_binary(X_test2, Y_test2, DT)

    # DT = DT_train_binary(X_invi_last, Y_invi_last, max_depth)

    # acc = DT_test_binary(X_invi_test, Y_invi_test, DT)

    # print('For the last traini data group and second test sample')
    Answer = DT_make_prediction(X_test[1], DT)
    if (Answer == 1):
        print('The answer is: YES')
    else:
        print('The answer is: NO')

    dt1 = DT_train_binary_best(X_train2, Y_train, X_val2, Y_val2)
    
