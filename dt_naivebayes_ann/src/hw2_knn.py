import numpy as np
import pandas as pd
import os
import sys

def data_preprocessing(raw_data, data_type): # data_type is train or test data
  # drop the id column
  data = raw_data.drop('id', axis=1)
  if data_type == "train":
    # separate the label and data
    label = data[["credit"]]
    # print(label)
    data = data.drop('credit', axis=1)
  data1 = pd.get_dummies(data, columns=["F10", "F11"])
  # print(data1.head())
  # print(data.head()) 
  if data_type == "train":
    return data1, label
  else:
    return data1

# def data_selected(data):
  # asfs 

def distance(row1, row2, feature_range, measure):
  sum_total = 0.0
  # print(row1)
  row3 = row1 - row2
  item = 0
  if measure == "euclidean":
    for column in row3:
      sum_total += np.power((column/feature_range[item]), 2)
      item += 1
  # print(np.sqrt(sum_total))
  return np.sqrt(sum_total)

 
if __name__ == '__main__':
  # define the device as the first visible cude device if we have CUDA available
  # device = torch.device("cuda:0" if torch.cuda.is_available() else "cpu")
  # print(device)    
  measure = "euclidean"
  # read the data frame
  csv_file = os.path.join(sys.path[0], "train_data.csv")
  train_data = pd.read_csv(csv_file, delimiter=',')
  csv_file = os.path.join(sys.path[0], "test_data4.csv")
  test_data = pd.read_csv(csv_file, delimiter=',')

  pd.set_option('display.max_columns', None)

  train_data, label = data_preprocessing(train_data, "train")
  test_data = data_preprocessing(test_data, "test")
  
  k_nn = 1
  output_list = []
  feature_range = []
  for column in train_data:
    max1 = train_data[column].max()
    min1 = train_data[column].min()
    # print("This is the min: \n", min1)
    feature_range.append(max1-min1)
  # print(feature_range)
  loop=0
  if (measure=="euclidean"):
    for _, row_test in test_data.iterrows():
      distance_list = []
      for index, row_train in train_data.iterrows():
        # print(index)
        distance1 = distance(row_test, row_train, feature_range, measure)
        # distance_list.append(((distance1, )))
        distance_list.append(distance1)
      # for i in range(3):
      #   print(distance_list[i])
      distance_list1 = np.array(distance_list)
      # get the indices of the sorted array
      distance_list2 = np.argsort(distance_list1, axis=0) 
      k_neighbor = distance_list2[:k_nn]
      # for j in range(len(k_neighbor)):
        # print(distance_list[k_neighbor[j]])
      zero_count = 0
      one_count = 0
      for i in range(k_nn):
        if label.iloc[k_neighbor[i]]['credit'] == 0:
          zero_count +=1
        else:
          one_count +=1
      if zero_count > one_count:
        output_list.append(0)
      else:
        output_list.append(1)


      
  with open('knn_results1_4.txt', 'w') as filehandle:
    for listitem in output_list:
        filehandle.write('%s\n' % listitem)
  filehandle.close()

