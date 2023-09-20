import numpy as np
import random 

def K_Means(X, K):
    C = []
    C_last = []
    howfar = np.zeros(K)  # this one stores the distance of each data to the corresponding center
    Y = np.zeros(len(X))  # handle the for belong index for each center

    ran = random.choice(range(len(X)-K))
    for i in range(K):
        C.append(X[ran+i].astype(float))
        C_last.append(X[ran+i].astype(float))
        # howfar.append(0.0)

    # convert from list to array
    C = np.array(C)
    C_last = np.array(C_last)
    
    running = True
    while (running):
        # recalculate the dataset for each center           
        for j in range(len(X)):
            # reset the howfar array
            for t in range(len(howfar)):
                howfar[t] = 0.0
            for h in range(len(X[0])):
                for m in range(K):
                    howfar[m] += (X[j][h] - C[m][h])**2

            # select the minimum distance and set the center for the data (Y array)
            minDistance = howfar[0]
            Y[j] = 0  # this data belong to Center 0
            for n in range(K):
                if (minDistance > howfar[n]):
                    minDistance = howfar[n]
                    Y[j] = n  # this data belong to Center 

        # save the previous center
        for r in range(len(C)):
            for a in range(len(C[0])):
                C_last[r][a] = C[r][a]
        
        # recalculate the center's location
        for p in range(K):
            # reset the location of each center
            for z in range(len(X[0])):
                C[p][z] = 0.0
            # reset the number of data belong to a particular center
            countp = 0
            for q in range(len(Y)):
                if (Y[q] == p):
                    countp += 1
                    for x in range(len(X[0])):
                        C[p][x] += float(X[q][x])
            for y in range(len(X[0])):
                C[p][y] = round(C[p][y]/float(countp), 2)
            # print("Number of data belong to group center", p, "is:", countp)
        # print("Array Y:", Y)

        
        # check whether the entire algorithm terminated??
        convergence = True
        for b in range(len(C)):
            for d in range(len(C[0])):
                if not (C_last[b][d] == C[b][d]):
                    convergence = False
        if convergence:
            running = False
    return C

def K_Means_better(X, K):
    count = 0
    percent = 0.0
    center_array = []
    center_index = []
    Datasize = K*len(X[0])
    center_vector = np.zeros(Datasize)
    nCenter = True  # this variable use to check whether the new center exit or not?
    selectCenterIndex = 0
    while ((count < 100) and (percent < 0.8)):
        
        nCenter = True
        center = K_Means(X, K)
        center_vector = center.ravel() 
        # print("The center of the invoke ", count)
        # print(center)
        if (count == 0):    # if this is the first center

            center_array.append(center_vector)
            center_index.append(1)
        else:
            # check whether the new center is coincides with any existed center with unorder appearing
            compareArray = 0
            for i in range(len(center_array)):
                for j in range(Datasize):
                        for m in range(Datasize):
                            if (center_vector[j] == center_array[i][m]):    
                                compareArray += 1
                if (compareArray == Datasize):
                    center_index[i] += 1
                    nCenter = False
                    # print("There is NOOOOOOOO new center ---- just for check")
                    break

            # add the new center into the array after checking.
            if nCenter:
                # print("A new center has just come ---------")
                center_array.append(center_vector)
                center_index.append(1)
        count += 1
        checkPercent = 0.0
        # print("Just check the center index", center_index)
        if (count > 95):
            percent = 0.0
            for k in range(len(center_index)):
                checkPercent = float(center_index[k])/float(count)
                # print("The percentage at iteration ", count ,"is: ", checkPercent)
                if (percent < checkPercent):
                    percent = checkPercent
                    selectCenterIndex = k
    # print("The number of test: ", count)
    print("The percentage: ", percent)
    print("The center index to be selected is:", selectCenterIndex)

    return center_array[selectCenterIndex]




if __name__ == '__main__':
    

    X = np.array([[1,0], [7,4], [9,6], [2,1], [4,8], [0,3], [13,5], [6,8], [7,3], [3,6], 
        [2,1], [8,3], [10,2], [3,5], [5,1], [1,9], [10,3], [4,1], [6,6], [2,2]])
    
    K = 3
    # C = K_Means(X, K)
    # print(C)
    centers = K_Means_better(X, K)
    print("The better centers: ")
    n = int(len(centers)/len(X[0]))
    indicenter = np.zeros(len(X[0]))
    for f in range(n): 
        for l in range(len(X[0])):
            indicenter[l] = centers[f*len(X[0])+l]
        # print("Center", f+1, ":", indicenter)