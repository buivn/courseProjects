# Hoang-Dung Bui, bui.hoangdungtn@gmail.com
# homwork 3 - part 2
# G01301479

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import random
import math
from statistics import mean
from numba import vectorize, jit, njit
import copy

def agglomerative_clustering(data, cluster_number):
  num_rows = np.shape(data)[0]
  proximity_matrix = np.zeros((num_rows, num_rows), float)

  list_of_types = []
  for i in range(len(data)):
    list_of_types.append([data[i].tolist()])

  # the first time calculate the proximity matrix
  for i in range(len(data)):
    proximity_matrix[i,i] = 0.0
    for j in range(i+1, len(data)):
      similarity = gpu_cosin_similarity (data[i], data[j])
      proximity_matrix[i,j] = similarity
      proximity_matrix[j,i] = similarity

  while len(list_of_types) > cluster_number:
    max_similarity = np.amax(proximity_matrix)
    max_index = np.where(proximity_matrix == max_similarity)

    # get the two closest data
    merge1 = list_of_types[max_index[0][0]]
    merge2 = list_of_types[max_index[0][1]]
    # merge them into a new list
    for i in range(len(merge1)):
      merge2.append(merge1[i])

    # remove two clusters from the cluster list 
    if max_index[0][0] > max_index[0][1]:
      list_of_types.pop(max_index[0][0])
      list_of_types.pop(max_index[0][1])
    else:
      list_of_types.pop(max_index[0][1])
      list_of_types.pop(max_index[0][0])

    # calculate the proximity vector
    proximity_column = np.zeros((1,len(list_of_types)+1), float)
    proximity_column[0,len(list_of_types)] = 0.0

    proximity_row = np.zeros((1,len(list_of_types)), float)
     
    for i in range(len(list_of_types)):
      farthest_points = 1.0
      for k in range(len(list_of_types[i])):
        for m in range(len(merge2)):
          distance = gpu_cosin_similarity (np.array(list_of_types[i][k]), np.array(merge2[m]))
          if farthest_points > distance:
            farthest_points = distance

      proximity_column[0,i] = farthest_points
      proximity_row[0,i] = farthest_points

    # add the new cluster
    list_of_types.append(merge2)

    if len(list_of_types) ==cluster_number:
      for i in range(len(list_of_types)):
        item = list_of_types[i]
        print(np.array(item).shape)

    # remove from the matrices
    if max_index[0][0] > max_index[0][1]:
      proximity_matrix = np.delete(proximity_matrix, max_index[0][0],0)
      proximity_matrix = np.delete(proximity_matrix, max_index[0][1],0)
      proximity_matrix = np.delete(proximity_matrix, max_index[0][0],1)
      proximity_matrix = np.delete(proximity_matrix, max_index[0][1],1)
    else:
      proximity_matrix = np.delete(proximity_matrix, max_index[0][1],0)
      proximity_matrix = np.delete(proximity_matrix, max_index[0][0],0)
      proximity_matrix = np.delete(proximity_matrix, max_index[0][1],1)
      proximity_matrix = np.delete(proximity_matrix, max_index[0][0],1)      
    # add new row and column
    proximity_matrix = np.append(proximity_matrix, proximity_row, axis=0)
    proximity_matrix = np.column_stack((proximity_matrix, proximity_column[0]))

  list_of_centroids = [] 
  # # calculate the centroid of the new cluster
  for k in range(len(list_of_types)):
    centroid = np.array(list_of_types[k]).mean(0)
    list_of_centroids.append(centroid)
  
  list_of_centroids = np.array(list_of_centroids)
  # call the k-mean function to do the final clustering
  result = k_mean(data, cluster_number, True, list_of_centroids)
  return result

def bisecting_k_mean_algorithm(data, cluster_number):
  list_of_types = []  # list of cluster_number clusters
  centroids = np.zeros((cluster_number, len(data[0])), dtype=float)
  data_new = copy.deepcopy(data)
  # bisecting_k_mean_algorithm's core part
  while True:
    if (len(list_of_types) == cluster_number):
      break
    # clustering the data into two
    label, sets = k_mean(data_new, 2, False, None)
    list_of_types.append(sets[0])
    list_of_types.append(sets[1])
    max_number = 0
    max_index = 0
    # select the next cluster to separate based on its items
    for i in range(len(list_of_types)):
      if max_number < len(list_of_types[i]):
        max_number = len(list_of_types)
        max_number = i
    data_new = list_of_types[max_index]
    list_of_types.pop(max_index)
  
  # calculuate the centroids for 10 clusters
  for m in range(cluster_number):
    centroids[m] = copy.deepcopy(np.array([float (sum(col))/len(col) for col in zip(*list_of_types[m])]))
  
  # call k_mean with provided centroids
  label, data_cluster = k_mean(data, cluster_number, True, centroids)
  return label


def k_mean(data, cluster_number, provided_centroid, centroid_set):
  # working with cosin similarity
  # randomly select 10 centroids which are far to each others
  centroids = np.zeros((cluster_number, len(data[0])), dtype=float)
  last_cens = np.zeros((cluster_number, len(data[0])), dtype=float)
  k=0
  # select 10 centroids which are far to others
  if provided_centroid:
    centroids = centroid_set
  else:
    for i in range(cluster_number):
      # print("we stuck here", k)
      while True:
        print("we stuck here", k)
        k += 1
        too_close = False
        index = random.randint(0,len(data)-1)
        if i == 0:
          # print("how many first time check")
          centroids[i] = data[index]
          break # exit the while
        else:
          for j in range(i):  # check with all previous centroids 
            if (gpu_cosin_similarity(centroids[j], data[index]) > 0.87):
              too_close = True   
          if not too_close:
            centroids[i] = copy.deepcopy(data[index])
            break  # break the while loop
  list_of_types = []  # list of 10 clusters
  for i in range(cluster_number):
    list_of_types.append([])
  # list of classified instances
  class_result_list = []
  # save the result if the clusters are stable
  save_the_result = False
  distances = np.zeros(cluster_number)

  while True:  # core part of k-mean algorithm
    # clear the lists
    for i in range(cluster_number):
      list_of_types[i].clear()
    # classify the data 
    for item in range(len(data)):
      for k in range(cluster_number):
        distances[k] = gpu_cosin_similarity(data[item], centroids[k])
      # get the most similarity
      g_index = np.argmax(distances)
      list_of_types[g_index].append(data[item])

      # save the classified result only the last run
      if save_the_result:
        class_result_list.append(g_index+1)
    # break the while loop - k-mean loop
    if save_the_result:
      break

    # recalculate the centroids from the list of vectors
    for m in range(cluster_number):
      centroids[m] = copy.deepcopy(np.array([float (sum(col))/len(col) for col in zip(*list_of_types[m])]))
    
    # check convergence condition
    for m in range(cluster_number):
      if (last_cens[m] == centroids[m]).all():
        if m==0:
          save_the_result = True
        else:
          save_the_result = save_the_result and True
      else:
        save_the_result = False
    
    last_cens = copy.deepcopy(centroids)
  return class_result_list



@jit(nopython=True)
def gpu_cosin_similarity (array1, array2):
  similarity = 0.0 # very different
  nom = 0.0
  dis1 = 0.0
  dis2 = 0.0
  # for i in range(array1.shape[0]):
  for i in range(len(array1)):
    nom += array1[i]*array2[i]
    dis1 += math.pow(array1[i],2)
    dis2 += math.pow(array2[i],2)
  similarity = nom/(math.sqrt(dis1)*math.sqrt(dis2))
  # print(similarity)
  return similarity

def data_preprocess (file):
  processed_data = np.empty((0, 784), float) # create an empty array with 4 columns and 0 rows
  only_one = True
  for line in file:
    # convert to an array
    data_convert = np.fromstring(line, dtype=int, sep=',')
    # Append a row to the 2D numpy array
    processed_data = np.append(processed_data, np.array([data_convert]), axis=0)
  # print((type(processed_data)))  
  return processed_data


if __name__ == '__main__':
  # print("read data from file")
  dataset = open("/content/drive/MyDrive/cs584/k_mean/test-data.txt", "r")
  # dataset = open("/content/drive/MyDrive/cs584/k_mean/sub_data.txt", "r")

  processed_data = data_preprocess(dataset)
  # shuffle the data
  np.random.shuffle(processed_data)
  
  # run the k-mean algorithm
  # result_list = k_mean(processed_data, 10, False, None)

  # run the bisecting-k-mean algorithm:
  # result_list = bisecting_k_mean_algorithm(processed_data, 10)
  
  # Call agglomerative clustering algorithm
  result_list = agglomerative_clustering(processed_data, 10)
  
  # print(result_list)

  with open('/content/drive/MyDrive/cs584/k_mean/output_file_cosin.txt', 'w') as f:
    for item in result_list:
      f.write("%s\n" % item)

  