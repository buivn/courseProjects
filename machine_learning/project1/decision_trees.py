import numpy as numpy
import math

# I think it is making more sense to me if we save the tree as class of individual tree nodes with different features which I will try to utilize better as I progress through the code 
class Tree_Node:
	def __init__(self,key):
	    self.left = None
	    self.right = None
	    self.gini = 0
	    self.depth = 0
	    #In order to differentiate between expanded individual nodes of the tree
	    # the value of feature and "None" will be used
	    self.feature = 999
	    self.val = key
	    self.Last = True

#Calculate Information Gain for Each Feature
def Info_Gain(GP, nc0, nc1, GL, GR):
	IG = GP - (nc0/(nc0+nc1))*GL  + (nc1/(nc0+nc1))*GR
	return IG

#Feature Selection based on the Highest Information Gain
def Feature_Select(IGain, Features):
	i = 0
	Gain_Max = 0
	F_num = 0
	while(i <  Features):
		# print("IGain")
		# print(IGain[i])
		if IGain[i] > Gain_Max:
			Gain_Max = IGain[i]
			F_num = i
		i+=1	
	# print("Max Gain")
	# print(Gain_Max)
	# print(F_num)
	return Gain_Max, F_num

#Calculate Gini/Entropy for Each Feature
def Gini_Calculate(GiniP, X, Y):
																																																																																																				
	Data_Size = X.shape
	Data_Features = Data_Size[1]
	Data_Elements = Data_Size[0]
	# print("Data Features")
	# print(Data_Features)
	# print("Data Elements")
	# print(Data_Elements)
	
	size = Y.size
	if size == Data_Elements:
	#	print("Both are equal")
	
	IGain = []
	Gini_Left = []
	Gini_Right = []
	i = 0
	j = 0
			
			
	for i in range(Data_Features):
		nclass0 = 0
		nclass1 = 0
		
		pclass00= 0
		pclass01= 0
		
		pclass10= 0
		pclass11= 0
	
		#Calculating Gini for Left and Right Children
		for j in range(Data_Elements):
			if X[j][i] == 0 and Y[j][0] == 0:
				nclass0+=1
				pclass00+=1
			if X[j][i] == 0 and Y[j][0] == 1:
				nclass0+=1
				pclass01+=1
			if X[j][i] == 1 and Y[j][0] == 0:
				nclass1+=1
				pclass10+=1
			if X[j][i] == 1 and Y[j][0] == 1:
				nclass1+=1
				pclass11+=1
		#Checks to prevent division by zero
		# print("pclass00 + pclass01")
		# print(pclass00 + pclass01)
		# print("pclass10 + pclass11")
		# print(pclass10 + pclass11)
		if (pclass00 + pclass01) == 0:
			A1 = 0
			B1 = 0
		else:
			A1 = pclass00/(pclass00 + pclass01)
			B1 = pclass01/(pclass00 + pclass01)
		if (pclass10 + pclass11) == 0:
			A2 = 0
			B2 = 0
		else:
			A2 = pclass10/(pclass10 + pclass11)
			B2 = pclass11/(pclass10 + pclass11)
		#####################################
		Ginileft = 1 - (A1**2 + B1**2)
		# print("Gini Left")
		# print(Ginileft)
		Gini_Left.append(Ginileft)
		Giniright = 1 - (A2**2 + B2**2)
		# print("Gini Right")
		# print(Giniright)
		Gini_Right.append(Giniright)
		#Calculate Information Gain for each feature
		IG = Info_Gain(GiniP,nclass0,nclass1,Ginileft,Giniright)
		# print("Information Gain")
		# print(IG)
		
		IGain.append(IG)
	return Gini_Left,  Gini_Right, IGain

def Add_Node(Pnode, Depth, Feature, GiniL, GiniR):
# If the value of Gini for Left Node is greater than Right Node, 
# We have to split on the Left Node of Current Feature and create a new node for Next Feature
	Pnode.Last =  False
	if GiniL[Feature] > GiniR[Feature]:
		NewNode = Tree_Node(0)
		Pnode.left = NewNode
		#print("New Left Node created")
				
		NewNode.gini = GiniL[Feature]
		NewNode.depth = Depth + 1
# If the value of Gini for Right Node is greater than Left Node, 
# We have to split on the Right Node of Current Feature and create a new node for Next Feature
	if GiniR[Feature] > GiniL[Feature]:
		NewNode = Tree_Node(1)
		Pnode.right = NewNode
		#print("New Right Node created")
		
		NewNode.gini = GiniR[Feature]
		NewNode.depth = Depth + 1
	return NewNode

def DT_Train_Binary(X, Y, max_depth):
	feature = []
	#Step 1: Develop the First Tree Node
	First_Node = Tree_Node(1)
	First_Node.depth = 1
	
	Data_Size = X.shape
	#print(Data_Size)

	index1 = Data_Size[0]
	index2 = Data_Size[1]
	#print(index1)
	#print(index2)

	Data_Features = Data_Size[1]
	#print(Data_Features)

	Data_Elements = X.size
	#print(Data_Elements)

	#Checking for depth condition based on requirements
	if  max_depth == -1:
		depth = Data_Features
	else:
		depth = max_depth

	# Gini for Parent
	pclass0 = 0
	pclass1 = 0

	size=Y.size
	print("Y size")
	print(size)
	for y in range(0,size):
		if Y[y][0] == 0:
			pclass0+=1
		if Y[y][0] == 1:
			pclass1+=1

	# print("Class 0:")
	# print(pclass0)
	# print("Class 1:")
	# print(pclass1)
	Giniparent = 1 - ((pclass0/(pclass0 + pclass1))**2 + (pclass1/(pclass0+ pclass1))**2)
	First_Node.gini = Giniparent
	# print("Gini for parent")
	# print(Giniparent)

	IG = []
	Gini_Left = []
	Gini_Right = []
	Depth = 0
	node = 1

	while Depth <= depth:
		#First_Node = Tree_Node(999)
		#Calculate Gini/Entropy for Each Feature
		Gini_Left, Gini_Right, IG = Gini_Calculate(Giniparent, X, Y)
		# print(Gini_Left)
		# print(Gini_Right)
		# print(IG)
		#Feature Selection based on the Highest Information Gain
		Max_IG, Feature = Feature_Select(IG, Data_Features)
		#Adding a new node to the binary tree based on IG and Gini values
		if node == 1:
			First_Node.feature = Feature
			This_Node = Add_Node(First_Node, Depth, Feature, Gini_Left, Gini_Right)
			
		else:
			Next_Node = Add_Node(This_Node, Depth, Feature, Gini_Left, Gini_Right)
			Next_Node.feature = Feature
		
		Depth +=1
		node +=1
	return First_Node


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
    acc = score*100/len(X_test)
    print('Accuracy on training data is %f', acc)
    return acc

# return a tree with best accurracy
def DT_train_binary_best(X_train, Y_train, X_val, Y_val):
    decisionTree = DT_train_binary(X_train, Y_train, -1)
    best_acc = DT_test_binary(X_val, Y_val, decisionTree)

    print("The highest accurracy of this decision Tree is %f", best_acc)

    return decisionTree





def DT_train_real(X, Y, max_depth):
    l = 1
    return l


def DT_test_real(X, Y, DT):
    p = 1.0
    return p

def DT_train_real_best(X_train, Y_train, X_val, Y_Val):

    k = 2.0
    return k


# test Gini values
# print(gini_index([[[1, 1], [1, 0]], [[1, 1], [1, 0]]], [0, 1]))

X = numpy.array([[0,1,0,1], [1,1,1,1], [0,0,0,1]])
Y = numpy.array([[1], [1], [0]])
# Y1 = numpy.array([[0], [0], [0], [0], [1], [1], [0], [0], [0], [1]])
max_depth = 2


DT = DT_Train_Binary(X, Y, max_depth)

# test_acc = DT_test_binary(X, Y, DT)

# decisionTreeBest = DT_train_binary_best(X, Y, X_val, Y_Val)


# print(DT_train_binary(X, Y, max_depth))
# print(IG(X, Y1))
