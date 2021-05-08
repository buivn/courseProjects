# Name: Hoang Dung Bui
# G01301478

import os
import time
import gc

# data science imports
import pandas as pd
from scipy.sparse import csr_matrix
from sklearn.neighbors import NearestNeighbors
import numpy as np


class recommender_knn:
  # Item-based CF recommender class with KNN implmented by sklearn
  def __init__(self, path_test, path_rat, mov_rat_thres, use_rat_thres, n_nei, alg, met, job=None):
      self.path_test = path_test  # path to movies data
      self.path_rat = path_rat # path to ratings data
      self.mov_rat_thres = mov_rat_thres
      self.use_rat_thres = use_rat_thres
      self.k_nn = n_nei
      # get the NearestNeighbors model
      self.model = NearestNeighbors()
      # n_neighbors: int, # algorithm: brute, # metric: (default 'minkowski') = 'cosine', # n_jobs: int or None
      if job and (job > 1 or job == -1):
          os.environ['JOBLIB_TEMP_FOLDER'] = '/tmp'
      self.model.set_params(**{
          'n_neighbors': self.k_nn,
          'algorithm': alg,
          'metric': met,
          'n_jobs': job})        

  def preprocess_data(self):
      # - set up movie-user matrix
      # read data from a .dat file and save as DataFrame - pandas
      df_test = pd.read_table(
          os.path.join(self.path_test), delimiter=' ',
          usecols=['userID', 'movieID'],
          dtype={'userID': 'int32', 'movieID': 'int32'})

      df_rat = pd.read_table(
          os.path.join(self.path_rat), delimiter=' ',
          usecols=['userID', 'movieID', 'rating'],
          dtype={'userID': 'int32', 'movieID': 'int32', 'rating': 'float32'})

      # filter data
      df_mov_coun = pd.DataFrame(
          df_rat.groupby('movieID').size(),
          columns=['count'])
      # get a list of movies which received enough rating
      pop_mov = list(set(df_mov_coun.query('count >= @self.mov_rat_thres').index))  # noqa
      mov_fil = df_rat.movieID.isin(pop_mov).values
      # print out all elements of a pandas DataFrame
      pd.set_option('display.max_columns', None)
      # print out all element of an array
      np. set_printoptions(threshold=np. inf)

      # just consider the users who rated more than the film number threshold
      df_use_coun = pd.DataFrame(
          df_rat.groupby('userID').size(),
          columns=['count'])
      act_use = list(set(df_use_coun.query('count >= @self.use_rat_thres').index))  
      
      use_fil = df_rat.userID.isin(act_use).values
      # erase the unactive user and unpopular movies
      df_rat_fil = df_rat[mov_fil & use_fil]

      # create movie-user matrix by pivot function
      mov_use_mat = df_rat_fil.pivot(
          index='movieID', columns='userID', values='rating').fillna(0)

      # transform to a sparse matrix
      mov_use_spa_mat = csr_matrix(mov_use_mat.values)

      # clean up
      del df_rat, df_rat_fil
      del df_mov_coun, df_use_coun
      return mov_use_spa_mat, mov_use_mat, df_test


  def _rating(self, model, data, sparse_data, rating_movies, n_recom):
    # return top n similar movie recommendations based on user's input movie
    # model: sklearn model, knn model,  data: movie-user matrix, sparse_data = sparse matrix
    # guess_movies: list of movies need to be rated
    # n_recom: top n recommendations

    # Return: # list of top n similar movie recommendations
    # fit
    model.fit(sparse_data)
    # get input movie index
    print('You have a list of movies needed to be rated')
    t0 = time.time()
    print("The rating score for movies and user:")
    # run for all data in the test file
    score_list = []
    for i in range(len(rating_movies)):
      print(i)
      movieID =  rating_movies.loc[i][1]
      userID = rating_movies.loc[i][0]
      # get the row ordered number as knowing the index
      idx = next(iter(np.where(data.index==movieID)[0]), 'not matched')
      if (idx == 'not matched'):
        score = 0.0
        score_list.append(score)
      else:
        # the first item is the point itself
        distances, indices = model.kneighbors(
          sparse_data[idx],
          n_neighbors=self.k_nn+1)

        # calculate the score for the item:
        count = 0
        mean_list = np.empty(self.k_nn) # rating mean of all neighbor items
        mean_rate_item = 0.0  # rating mean of the current rating item
        user_item_rate = np.empty(self.k_nn)  # mean of the similarity x (the difference of )
        nomin = np.empty(self.k_nn)
        denom = np.empty(self.k_nn)

        # for loop to calculate the elements for the scoring formula
        for index in indices[0]:
          if count >0:
            mean_list[count-1] = sparse_data.mean(axis=1)[index].squeeze().squeeze()
            user_item_rate[count-1] = data.loc[data.iloc[[index]].index[0], userID]
            # calculate the denominator
            denom[count-1] = distances.squeeze()[count]
            # calculate the nominator item
            nomin[count-1] = distances.squeeze()[count]*(user_item_rate[count-1] - mean_list[count-1])
          else:
            mean_rate_item = sparse_data.mean(axis=1)[index].squeeze().squeeze()
          count += 1
        
        # calculate the score
        score1 = nomin.sum()/denom.sum() + mean_rate_item[0,0]
        if score1 < 0:
          score_list.append(0)
        elif score1 > 5:
          score_list.append(score1)
        else:
          score_list.append(score1)
    
    print('It took {:.2f}s to make inference \n\
          '.format(time.time() - t0))
    return score_list

  def make_predictions(self):  # make n movie recommendations
    # guess_movie: list of movies which need to be rated, n_recom: n recommendations
    # get data
    mov_use_spa_mat, data, movies_list_rating = self.preprocess_data()
    # get recommendations
    score_list = self._rating(
        self.model, data, mov_use_spa_mat, movies_list_rating, self.k_nn)
    return score_list


if __name__ == '__main__':  
  data_path="/home/dzungbui/Desktop/additional_files/"
  movies_rating = "test.dat"
  ratings_filename = "train.dat"
  

  act_user = 4
  pop_mov = 4
  k_nn = 10
  alg = 'brute'
  sim = 'cosine'
  job = -1

  # initial recommender system
  recommender = recommender_knn(
      os.path.join(data_path, movies_rating),
      os.path.join(data_path, ratings_filename), act_user, pop_mov, k_nn, alg, sim, job)

  # # make recommendations
  scores = recommender.make_predictions()
  # print(scores)

  with open('output_score_knn_10.txt', 'w') as f:
    for item in scores:
      f.write("%s\n" % item)