import numpy as np
# eps = np.finfo(float).eps
# from numpy import log2 as log


class Node:
    def __init__(self):
        self.split = None   # which feature will be splitted on (Taste)
        self.leaf = False   # check whether it is leaf node or not
        self.left = None    # represents left and right child nodes (both are null for a leaf node)
        self.right = None
        self.result = None  # tell the output is true or false at this node -> use to predict the output value for new data
        self.gini = 0       # parameters to decide the best split
        self.X = None    # Data at this node
        self.Y = None
        self.level=0        # represent depth of the tree

    def __repr__(self):
        return '['+self.split+'  '+str(self.split)+']'


class DecisionTree:
    #takes data and target column
    def __init__(self, X, Y, depth=None, v=0):
        self.data = X
        # self.target = target    # target variable (eat or not)
        self.root = None
        self.depth = depth      # maximum depth allowed
        self.v = v          # v = 1 to see teh tree building process
        
    #This helper function calculates gini index of the dataset
    def giniIndex(self,Y):
        first = np.count_nonzero(Y==1)
        second = float(len(Y))       
        return 1-(first/second)**2-((second-first)/second)**2

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
            print('Building Tree for the data')
            print('======================================')
            print(X)
            print(Y)
        left_dataset = None
        left_Label = None
        right_dataset = None
        right_Label = None
        min_gini = np.inf
        best_feature = None
        # feature_values = None
        
        # gini = len(data)*DecisionTree.giniIndex(data,self.target)
        # print(self.giniIndex(Y))
        gini = len(X)*self.giniIndex(Y)
        if self.v==1:
            print('GiniIndex for total data = ', gini)
        
        # declare a node
        node = Node()
        node.X = X
        node.Y = Y
        node.level = level
        node.gini = gini
        
        if self.depth is not None and level==self.depth:
            node.leaf = True
            if (2*np.count_nonzero(Y==1) > len(Y)):
                node.result = 1
            else:
                node.result = 0
            return node
        
        #If there is no impurity in this data, make it a leaf node
        if gini==0:
            if self.v==1:
                print('The data is pure, no split is needed ')
            node.gini=0
            node.leaf = True
            if (2*np.count_nonzero(Y==1) > len(Y)):
                node.result = 1
            else:
                node.result = 0
            return node
            
        
        for feature in range(len(X[0])):
            # print(len(X[0]))
            
            #Find all unique values for the feature
            unique = True
            for i1 in range(len(Y)):
                if not(X[0][feature] == X[i1][feature]):
                    unique = False

            if self.v==1:
                print('________________________________________________________')
                print('Evaluating the splits for the feature :',feature)
            
            #Initialize gini, left and right datasets and best feature values
            tmngini = np.inf
            tldset = None
            trdset = None
            tbftr = None  #We can't split based on a single value,There must be atleast 2 unique values to be able to split
            # check value of feature
            if unique:
                print('Ignoring this feature as it has only a single unique value')
                continue
            
            lset_Data, lset_Label, rset_Data, rset_Label = self.split_data(X, Y, feature)
            #Find gini index for left split
            lgini = self.giniIndex(lset_Label)
            # print(lset_Label)
            #Find gini impurity for right split
            rgini = self.giniIndex(rset_Label)

            # Find the total weighted gini. 
            tgini = float(len(lset_Label))*lgini + float(len(rset_Label))*rgini
            if self.v==1:                    
                print('-----------------------')
                print('Left dataset')
                print(lset_Data)
                print('-----------------------')
                print('Right dataset')
                print(rset_Data)
                print('Weighted Gini for this split ',tgini)
            #Update minimum gini
            if tgini<tmngini:
                tmngini=tgini

  
            if self.v==1:
                print('Best gini for feature ',feature,' is ',tmngini)
                
            #Update minimum gini
            if tmngini<min_gini:
                min_gini = tmngini
                left_dataset = lset_Data
                left_Label = lset_Label
                right_dataset = rset_Data
                right_Label = rset_Label
                # feature_values = tbftr
                best_feature = feature
                
        #No improvement in gini value after split, Make it as leaf node
        if min_gini>tmngini:
            node.leaf = True
            if (2*np.count_nonzero(Y==1) > len(Y)):
                node.result = 1
            else:
                node.result = 0
            return node
            
        node.min_gini= min_gini
        # node.feature_values = feature_values
        node.split = best_feature
        if self.v==1:
            print('Best split is "',best_feature,' and GiniIndex is ',min_gini)
        
        #Build tree for left dataset
        if left_dataset is not None:
            node.left = self.build(left_dataset,left_Label, level+1)
            
        #Build tree for right dataset
        if right_dataset is not None:
            node.right = self.build(right_dataset, right_Label, level+1)
            
        #If both the trees are not built, it has to be leaf
        if node.left==None and node.right==None:
            node.leaf = True
        
        return node        
        
    
    def fit(self):
        self.root = self.build(X, Y)
        return self.root
        
    def __predict__(self,s,root):
        if root is None:
            return False
        if root.leaf:
            return root.result
        if (s[root.split] == 0):
            return self.__predict__(s,root.left)
        else:
            return self.__predict__(s,root.right)
        
        
    def predict(self,s):
        return self.__predict__(s,self.root)


def t(x):
    if x is None:
        return ""
    return str(x)

def build_tree(root):
    if root==None:
        return ""
    return {"split" : t(root.split)+' '+str(root.level), "left":build_tree(root.left), "right":build_tree(root.right) }

def DT_train_binary(X,Y,max_depth):
    return 1


if __name__ == '__main__':
    

    # data features: Spicy, Hot, Hard,
    X = np.array([[0,1,0],[1,1,0],[1,1,1],[1,0,1],[1,1,1],[0,0,0],[0,0,0],[0,1,0],[1,0,0],[0,1,1]])
    Y = np.array([[0],[0],[1],[0],[1],[1],[0],[1],[1],[1]])

    
    # create a decision tree class with input as data, "Eat" is the label
    tree = DecisionTree(X, Y, depth=3,v=1)

    # build node's list for the decision tree
    root = tree.fit()
    
    print()
    print("Print out the decision tree on the screen")
    g = build_tree(root)
    g = str(g).replace('\'','"')
    g = g.replace('["','')
    g = g.replace('"]','')
    print(g)
    
    score = 0
    for i in range(len(Y)):
        pred = tree.predict(X[i])
        if pred and Y[i]:
            score+=1
        elif not pred and not Y[i]:
            score+=1
    print('Accuracy on training data is ',(score*100/len(Y)))

    print()
    print("Just try to predict")
    pred = tree.predict(X[1])
    print(pred)
