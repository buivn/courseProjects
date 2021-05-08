# Hoang-Dung Bui, bui.hoangdungtn@gmail.com
# homwork 3

# import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import random
import math
from statistics import mean
from sklearn.preprocessing import StandardScaler
# from sklearn.model_selection import train_test_split
# from sklearn.tree import DecisionTreeRegressor
# from sklearn import metrics


def k_mean(data, cluster_number):
  # print(len(data[0]))

  # randomly select three centroids
  index = random.randint(0,149)
  centroid1 = data[index]
  # select centroid2 far enough the centroid1 
  centroid2 = []
  centroid3 = []
  while True:
    index = random.randint(0,149)
    if (distance_function(centroid1, data[index]) > 0.3):
      centroid2 = data[index]
      break
  # select centroid3 far enough to centroid1 and centroid2 
  while True:
    index = random.randint(0,149)
    if (distance_function(centroid1, data[index]) > 0.3) and (distance_function(centroid2, data[index]) > 0.3):
      centroid3 = data[index]
      break

  G1 = []
  G2 = []
  G3 = []
  last_cen1 = np.empty((0, len(data[0])), float)
  last_cen2 = np.empty((0, len(data[0])), float)
  last_cen3 = np.empty((0, len(data[0])), float)
  class_result_list = []
  save_the_result = False

  while True:
    G1.clear()
    G2.clear()
    G3.clear()
    class_result_list = []
    # classify the data 
    for item in range(len(data)):
      dis1 = distance_function(data[item], centroid1)
      dis2 = distance_function(data[item], centroid2)
      dis3 = distance_function(data[item], centroid3)
      d_min = dis1
      g_index = 1
      if dis2 < d_min:
        d_min = dis2
        g_index = 2
      if dis3 < d_min:
        d_min = dis3
        g_index = 3
      if g_index == 1:
        G1.append(data[item])
      if g_index == 2:
        G2.append(data[item])
      if g_index == 3:
        G3.append(data[item])

      # save the classified result only the last run
      if save_the_result:
        class_result_list.append(g_index)
    # break the while loop
    if save_the_result:
      break
    # print(G1)
    # recalculate the centroids from the list of vectors
    centroid1 = np.array([float (sum(col))/len(col) for col in zip(*G1)])
    centroid2 = np.array([float (sum(col))/len(col) for col in zip(*G2)])
    centroid3 = np.array([float (sum(col))/len(col) for col in zip(*G3)])
    # print(centroid1)
    if (last_cen1 == centroid1).all() and (last_cen2 == centroid2).all() and (last_cen3 == centroid3).all():
      save_the_result = True
    
    last_cen1 = centroid1
    last_cen2 = centroid2
    last_cen3 = centroid3
  return class_result_list


def k_mean_2(data, cluster_number):
  # working with cosin similarity
  # randomly select three centroids
  index = random.randint(0,149)
  centroid1 = data[index]
  # select centroid2 far enough the centroid1 
  centroid2 = []
  centroid3 = []
  while True:
    index = random.randint(0,149)
    
    if (cosin_similarity(centroid1, data[index]) < 0.985):
      centroid2 = data[index]
      break
  # select centroid3 far enough to centroid1 and centroid2 
  while True:
    index = random.randint(0,149)
    # print(cosin_similarity(centroid1, data[index]))
    # print(cosin_similarity(centroid2, data[index]))
    if (cosin_similarity(centroid1, data[index]) < 0.987) and (cosin_similarity(centroid2, data[index]) < 0.987):
      centroid3 = data[index]
      break

  G1 = []
  G2 = []
  G3 = []
  last_cen1 = np.empty((0, len(data[0])), float)
  last_cen2 = np.empty((0, len(data[0])), float)
  last_cen3 = np.empty((0, len(data[0])), float)
  class_result_list = []
  save_the_result = False

  while True:
    G1.clear()
    G2.clear()
    G3.clear()
    class_result_list = []
    # classify the data 
    for item in range(len(data)):
      dis1 = cosin_similarity(data[item], centroid1)
      dis2 = cosin_similarity(data[item], centroid2)
      dis3 = cosin_similarity(data[item], centroid3)
      d_max = dis1
      g_index = 1
      if dis2 > d_max:
        d_max = dis2
        g_index = 2
      if dis3 > d_max:
        d_max = dis3
        g_index = 3
      if g_index == 1:
        G1.append(data[item])
      if g_index == 2:
        G2.append(data[item])
      if g_index == 3:
        G3.append(data[item])

      # save the classified result only the last run
      if save_the_result:
        class_result_list.append(g_index)
    # break the while loop
    if save_the_result:
      break
    # print(G1)
    # recalculate the centroids from the list of vectors
    centroid1 = np.array([float (sum(col))/len(col) for col in zip(*G1)])
    centroid2 = np.array([float (sum(col))/len(col) for col in zip(*G2)])
    centroid3 = np.array([float (sum(col))/len(col) for col in zip(*G3)])
    # print(centroid1)
    if (last_cen1 == centroid1).all() and (last_cen2 == centroid2).all() and (last_cen3 == centroid3).all():
      save_the_result = True
    
    last_cen1 = centroid1
    last_cen2 = centroid2
    last_cen3 = centroid3
  return class_result_list



def distance_function(array1, array2):
  distance = 0.0
  for i in range(len(array1)):
    distance += math.pow((array1[i] - array2[i]),2)
  return math.sqrt(distance)

def cosin_similarity (array1, array2):
  similarity = 0.0 # very different
  nom = 0
  dis1 = 0
  dis2 = 0
  for i in range(len(array1)):
    nom += array1[i]*array2[i]
    dis1 += math.pow(array1[i],2)
    dis2 += math.pow(array2[i],2)
  return nom/(math.sqrt(dis1)*math.sqrt(dis2))


def data_preprocess (file):
  processed_data = np.empty((0, 4), float) # create an empty array with 4 columns and 0 rows
  for line in file:
    # convert to an array
    data_convert = np.fromstring(line, sep=' ')
    # print(data_convert)
    # Append a row to the 2D numpy array
    processed_data = np.append(processed_data, np.array([data_convert]), axis=0)  
  # print(processed_data)
  return processed_data

def data_preprocess_2 (file):
  # This function will concatenate the width and length into one data then normalize them
  processed_data = np.empty((0, 2), float) # create an empty array with 4 columns and 0 rows
  
  max_feature1 = 0
  max_feature2 = 0
  for line in file:
    # print("check something working")
    # convert to an array
    data_convert = np.fromstring(line, sep=' ')
    feature1 = math.sqrt(math.pow(data_convert[0],2) + math.pow(data_convert[1],2))
    feature2 = math.sqrt(math.pow(data_convert[2],2) + math.pow(data_convert[3],2))
    # print(feature1)
    if feature1 > max_feature1:
      max_feature1 = feature1
    if feature2 > max_feature2:
      max_feature2 = feature2
    # Append a row to the 2D numpy array
    processed_data = np.append(processed_data, np.array([[feature1, feature2]]), axis=0)
  
  # normalize by dividing each feature for their max values
  max_feature = np.array((max_feature1, max_feature2))
  for i in range(len(processed_data)):
    processed_data[i] = np.divide(processed_data[i], max_feature)
  # print(max_feature1)
  # print(max_feature2)
  return processed_data


def data_preprocess_3 (file):
  # This function will concatenate the width and length into one data then normalize them
  processed_data = np.empty((0, 4), float) # create an empty array with 4 columns and 0 rows
  std_slc = StandardScaler()
  for line in file:
    data_convert = np.fromstring(line, sep=' ')
    processed_data = np.append(processed_data, np.array([data_convert]), axis=0)
  std_slc.fit(processed_data)
  return std_slc.transform(processed_data)



if __name__ == '__main__':
  
  dataset = open("iris_test_data.txt", "r")

  processed_data = data_preprocess(dataset)
  # b = np.array((0.5, 0.5, 1, 2.0))
  # for i in range(len(processed_data)):
    # print(cosin_similarity(processed_data[i], b))

  # print(processed_data)

  # processed_data_3 = data_preprocess_3(dataset)
  # print(processed_data_3)
  
  result_list = k_mean_2(processed_data, 3)

  # print(result_list)
  # print(len(result_list))

  with open('output_file_cosin.txt', 'w') as f:
    for item in result_list:
      f.write("%s\n" % item)

    
  # a1 = np.array(((1, 2, 1), (2,2,3)))
  # print(a1)
  # b = np.array((0.5, 0.5, 1))
  # c = np.array((0.0, 0.1, 5))
  # print(cosin_similarity(b, c))
  # for i in range(len(a1)):
  #   a1[i] = np.divide(a1[i], b)
  # print(a1)
    # l1 = []
    # l1.append([1, 2, 1])
    # l1.append([2,5,2])
    # l1.append([3,5,3])
    # aver = [float (sum(col))/len(col) for col in zip(*l1)]

    # print(aver)
    # a1 = np.delete(a1)
    # a2 = np.array([2, 2, 1])