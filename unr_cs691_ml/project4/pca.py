import numpy as np
import numpy.linalg as linalg


def compute_Z(X, centering=True, scaling=False):
  Z = X.astype(np.double)
  if (centering == True):
    # calculate the mean of each column (axis = 0)
    col_averages = np.mean(X, axis=0)
    for i in range(X.shape[1]):
      Z[:,i] = X[:,i] - col_averages[i]
          
  if (scaling == True):
    # calculate the standard deviation by column (axis = 0)
    col_std = np.std(X, axis=0)
    
    # print(X.shape[1])
    # print(X.shape[0])
    for j in range(X.shape[1]):
      for m in range(X.shape[0]):
        Z[m][j] = Z[m][j] / col_std[j]
  return Z

def compute_covariance_matrix(Z):
  cov = np.matmul(Z.transpose(),Z)
  # print(Z.transpose())
  return cov


def find_pcs(COV):
  eigenValues, eigenVectors = linalg.eig(COV)
  idx = eigenValues.argsort()[::-1]
  eigenValues = eigenValues[idx]
  eigenVectors = eigenVectors[:,idx]
  return eigenValues, eigenVectors


def project_data(Z, PCS, L, k, var):
  if ((k==0) and (var ==0)):
    print("Error inputs")
    return 0

  if (not(k==0) and not(var ==0)):
    print("Error inputs")
    return 0

  if (var == 0):
    if ((k < 0) or (k>len(PCS[0]))):
      print("Error input parameters due to k")
      return 0
    space = np.zeros((len(PCS),k), dtype=complex)
    for i in range(len(PCS)):
      for j in range(k):
        space[i][j] = PCS[i][j]
    pro_data = np.matmul(Z,space)
    return pro_data

  if (k == 0):
    if ((var < 0.0) or (var>1.0)):
      print("Error input parameters due to var")
      return 0
    S = np.sum(L)
    check = 0.0
    for m in range(len(L)):
      check += L[m]
      if (check/S >= var):
        space = np.zeros((len(PCS),m+1), dtype=complex)
        for i in range(len(PCS)):
          for j in range(m+1):
            space[i][j] = PCS[i][j]
        pro_data = np.matmul(Z,space)
        return pro_data



# if __name__ == '__main__':
    

  # X = np.array([[-1,-1], [-1,1], [1,-1], [1,1]])
  X = np.array([[-1,-1], [-1,1], [1,-1], [1,1], [2,6]])

  Z = compute_Z(X)
  COV = compute_covariance_matrix(Z)
  L, PCS = find_pcs(COV)
  Z_star = project_data(Z,PCS, L, 0, 1)
  print(Z_star)



  