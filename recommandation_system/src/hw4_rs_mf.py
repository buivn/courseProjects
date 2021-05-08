# Name: Hoang Dung Bui
# G01301478

import os
import time


import numpy as np
import pandas as pd


class recommender_MF:
  def __init__(self, path_test, path_rat, latent_features, steps=5, alpha=0.0002, beta=0.02, min_error=0.001):
    self.path_test = path_test  # path to movies data
    self.path_rat = path_rat # path to ratings data    
    self.steps = steps         # steps: iterations
    self.alpha = alpha        # alpha: learning rate
    self.beta = beta          # beta: regularization parameter
    self.min_error = min_error
    self.latent_features = latent_features
    self.user_size = 0
    self.item_size = 0
    

  def preprocess_data(self):
    # - set up movie-user matrix
    # read data from a .dat file and save as DataFrame - pandas
    df_test = pd.read_table(
      os.path.join(self.path_test), delimiter=' ',
      usecols=['userID', 'movieID'],
      dtype={'userID': 'int32', 'movieID': 'int32'})

    df_train = pd.read_table(
      os.path.join(self.path_rat), delimiter=' ',
      usecols=['userID', 'movieID', 'rating'],
      dtype={'userID': 'int32', 'movieID': 'int32', 'rating': 'float32'})

    mov_use_mat = df_train.pivot(
        index='movieID', columns='userID', values='rating').fillna(0)
        
    self.item_size = len(mov_use_mat)
    self.user_size = len(mov_use_mat.columns)
    # clean up
    del df_train
    return mov_use_mat, df_test
  

  def matrix_factorization(self, R1):
    # R: rating matrix
    R = R1.to_numpy()
    # P: User - features matrix)
    P = np.random.rand(len(R), self.latent_features)
    # Q: Item - features matrix)
    Q = np.random.rand(len(R[0]), self.latent_features)

    Q = Q.T  # transpose the Q

    for step in range(self.steps):
      print("Step: ", step+1)
      P, Q = self._update_P_Q(R, P, Q)

      if (step > 0) and ((step % 100) == 0):
        e = self._calculate_error(R,P,Q)
        if e < self.min_error:
          break
    return P, Q.T

  # @jit(nopython=True)
  # this function can be run parallel
  def _update_P_Q(self, R,P,Q):
    for i in range(len(R)):
      for j in range(len(R[0])):
        if R[i][j] > 0:   # only calculat the error if the rating is nonzero
          # calculate error
          eij = R[i][j]  - np.dot(P[i,:], Q[:,j])
          for k in range(self.latent_features):
            # calculate gradient with then update the matrices P and Q
            P[i][k] = P[i][k] + self.alpha * (2 * eij * Q[k][j] - self.beta * P[i][k])
            Q[k][j] = Q[k][j] + self.alpha * (2 * eij * P[i][k] - self.beta * Q[k][j])
    return P, Q

  # @jit(nopython=True)
  # this function can be run parallel
  def _calculate_error(self, R,P,Q):
    e = 0.0
    # check the error sum after updating
    for i in range(len(R)):
      for j in range(len(R[i])):
        if R[i][j] > 0:
          e = e + pow(R[i][j] - np.dot(P[i,:],Q[:,j]), 2)
          for k in range(self.latent_features):
            e = e + (self.beta/2) * (pow(P[i][k],2) + pow(Q[k][j],2))
    return e

  def scoring(self, R_pandas, test_data, R_new):
    # run for all data in the test file
    score_list = []
    for i in range(len(test_data)):
      # print(i)
      movieID =  test_data.loc[i][1]
      userID = test_data.loc[i][0]
      # get the row and column numbers as knowing the index
      idx_movie = next(iter(np.where(R_pandas.index==movieID)[0]), 'not matched')
      idx_user = R_pandas.columns.get_loc(userID)
      if (idx_movie == 'not matched'):
        print(idx_movie)
        score = 0.0
        score_list.append(score)
      else:
        score1 = R_new[idx_movie, idx_user]
        score_list.append(score1)
    return score_list   


if __name__ == '__main__':  
  data_path="/home/dzungbui/Desktop/additional_files/"
  movies_rating = "test.dat"
  ratings_filename = "train.dat"

  latent_features = 5
  
  t0 = time.time()

  recommender_mf = recommender_MF(
      os.path.join(data_path, movies_rating),
      os.path.join(data_path, ratings_filename), latent_features, steps=250)

  R_pandas, test_data = recommender_mf.preprocess_data()

  P, Q = recommender_mf.matrix_factorization(R_pandas)

  R_new = np.dot(P, Q.T)
  
  score_list = recommender_mf.scoring(R_pandas, test_data, R_new)

  with open('output_score_MF.txt', 'w') as f:
    for item in score_list:
      f.write("%s\n" % item)  

  print('It took {:.2f}s to make inference \n\
    '.format(time.time() - t0))
