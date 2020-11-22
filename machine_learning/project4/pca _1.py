import numpy as np
import numpy.linalg as linalg

def compute_Z(X, centering=True, scaling=False):
    if centering == True:
        col_averages = np.mean(X, axis=0)
        for i in range(X.shape[1]):
            X[:,i] = X[:,i] - col_averages[i]
            
    if scaling == True:
        col_std = np.std(X, axis=0)
        
        for i in range(X.shape[1]):
            X[:,i] = X[:,i] / col_std[i]

    return X

def compute_covariance_matrix(Z):
    cov = np.matmul(Z.T,Z)
    return cov

def find_pcs(COV):
    eigenValues, eigenVectors = linalg.eig(COV)
    idx = eigenValues.argsort()[::-1]   
    eigenValues = eigenValues[idx]
    eigenVectors = eigenVectors[:,idx]

    return eigenValues, eigenVectors

def project_data(Z, PCS, L, k, var):

    return 1


# Test Data
X = np.array([[-1,-1],[-1,1],[1,-1],[1,1]])

# Test Script
Z = compute_Z(X)
COV = compute_covariance_matrix(Z)
L, PCS = find_pcs(COV) 
Z_star = project_data(Z, PCS, L, 1, 0)
