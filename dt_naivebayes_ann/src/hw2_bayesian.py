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
  if data_type == "train":
    return data, label
  else:
    return data

 
if __name__ == '__main__':
  # define the device as the first visible cude device if we have CUDA available
  # device = torch.device("cuda:0" if torch.cuda.is_available() else "cpu")
  # print(device)    
  # measure = "euclidean"
  # read the data frame
  csv_file = os.path.join(sys.path[0], "train_data.csv")
  train_data = pd.read_csv(csv_file, delimiter=',')
  csv_file = os.path.join(sys.path[0], "test_data.csv")
  test_data = pd.read_csv(csv_file, delimiter=',')

  pd.set_option('display.max_columns', None)

  
  label_zero = train_data.loc[train_data['credit']==0]
  # print(label_zero)
  # print("the number of data in label_zero:", len(label_zero))
  label_one = train_data.loc[train_data['credit']==1]
  P_C_0 = len(label_zero)/(len(label_zero)+len(label_one))
  P_C_1 = len(label_one)/(len(label_zero)+len(label_one))

  label_zero, _ = data_preprocessing(label_zero, "train")
  label_one, _ = data_preprocessing(label_one, "train")
  
  # test_data = data_preprocessing(test_data, "test")
  
  # For attribute 1
  # Label Zero
  att01_1 = label_zero.loc[label_zero['F1'] < 5]
  P_C0_att01_1 = len(att01_1)/len(label_zero)
  att01_2 = label_zero.loc[(label_zero['F1'] >= 5) & (label_zero['F1'] < 15)]
  P_C0_att01_2 = len(att01_2)/len(label_zero)
  att01_3 = label_zero.loc[label_zero['F1'] >= 15]
  P_C0_att01_3 = len(att01_3)/len(label_zero)

  # Label One
  att11_1 = label_one.loc[label_one['F1'] < 5]
  P_C1_att11_1 = len(att11_1)/len(label_one)
  att11_2 = label_one.loc[(label_one['F1'] >= 5) & (label_one['F1'] < 15)]
  P_C1_att11_2 = len(att11_2)/len(label_one)
  att11_3 = label_one.loc[label_one['F1'] >= 15]
  P_C1_att11_3 = len(att11_3)/len(label_one)
  attribute1_list = []
  attribute1_list.append([5, P_C0_att01_1, P_C1_att11_1])
  attribute1_list.append([15, P_C0_att01_2, P_C1_att11_2])
  attribute1_list.append([16, P_C0_att01_3, P_C1_att11_3])
  # print(attribute1_list)

  # For attribute 2
  # class 0
  att02_1 = label_zero.loc[label_zero['F2'] < 10]
  P_C0_att02_1 = len(att02_1)/len(label_zero)
  P_C0_att02_2 = len(label_zero.loc[(label_zero['F2'] >= 10) & (label_zero['F2'] < 25)])/len(label_zero)
  P_C0_att02_3 = len(label_zero.loc[(label_zero['F2'] >= 25) & (label_zero['F2'] < 50)])/len(label_zero)
  P_C0_att02_4 = len(label_zero.loc[label_zero['F2'] >= 50])/len(label_zero)
  # class 1
  P_C1_att12_1 = len(label_one.loc[label_one['F2'] < 10])/len(label_one)
  P_C1_att12_2 = len(label_one.loc[(label_one['F2'] >= 10) & (label_one['F2'] < 25)])/len(label_one)
  P_C1_att12_3 = len(label_one.loc[(label_one['F2'] >= 25) & (label_one['F2'] < 50)])/len(label_one)
  P_C1_att12_4 = len(label_one.loc[label_one['F2'] >= 50])/len(label_one)

  attribute2_list = []
  attribute2_list.append([10, P_C0_att02_1, P_C1_att12_1])
  attribute2_list.append([25, P_C0_att02_2, P_C1_att12_2])
  attribute2_list.append([50, P_C0_att02_3, P_C1_att12_3])
  attribute2_list.append([51, P_C0_att02_4, P_C1_att12_4])

  # For attribute 3
  # class 0
  variable_list3 = train_data['F3'].unique()
  attribute3_list = []
  # i = 0
  for var in variable_list3:
    P_C0_att = len(label_zero.loc[label_zero['F3'] == var])/len(label_zero)
    P_C1_att = len(label_one.loc[label_one['F3'] == var])/len(label_one)
    attribute3_list.append([var, P_C0_att, P_C1_att])

  # For attribute 4
  variable_list = train_data['F4'].unique()
  # print(variable_list )
  attribute4_list = []
  for var in variable_list:
    P_C0_att = len(label_zero.loc[label_zero['F4'] == var])/len(label_zero)
    P_C1_att = len(label_one.loc[label_one['F4'] == var])/len(label_one)
    attribute4_list.append([var, P_C0_att, P_C1_att])
  # print(attribute4_list)

  # For attribute 5
  # class 0
  P_C0_att05_1 = len(label_zero.loc[label_zero['F5'] == 0])/len(label_zero)
  P_C0_att05_2 = len(label_zero.loc[(label_zero['F5'] >= 0) & (label_zero['F5'] < 500)])/len(label_zero)
  P_C0_att05_3 = len(label_zero.loc[label_zero['F5'] >= 500])/len(label_zero)
  # class 1
  P_C1_att15_1 = len(label_one.loc[label_one['F5'] == 0])/len(label_one)
  P_C1_att15_2 = len(label_one.loc[(label_one['F5'] >= 0) & (label_one['F5'] < 500)])/len(label_one)
  P_C1_att15_3 = len(label_one.loc[label_one['F5'] >= 500])/len(label_one)

  attribute5_list = []
  attribute5_list.append([0, P_C0_att05_1, P_C1_att15_1])
  attribute5_list.append([500, P_C0_att05_2, P_C1_att15_2])
  attribute5_list.append([501, P_C0_att05_3, P_C1_att15_3])

  # For attribute 6
  # class 0
  P_C0_att06_1 = len(label_zero.loc[label_zero['F6'] == 0])/len(label_zero)
  P_C0_att06_2 = len(label_zero.loc[(label_zero['F6'] >= 0) & (label_zero['F6'] < 500)])/len(label_zero)
  P_C0_att06_3 = len(label_zero.loc[label_zero['F6'] >= 500])/len(label_zero)
  # class 1
  P_C1_att16_1 = len(label_one.loc[label_one['F6'] == 0])/len(label_one)
  P_C1_att16_2 = len(label_one.loc[(label_one['F6'] >= 0) & (label_one['F6'] < 500)])/len(label_one)
  P_C1_att16_3 = len(label_one.loc[label_one['F6'] >= 500])/len(label_one)

  attribute6_list = []
  attribute6_list.append([0, P_C0_att06_1, P_C1_att16_1])
  attribute6_list.append([500, P_C0_att06_2, P_C1_att16_2])
  attribute6_list.append([501, P_C0_att06_3, P_C1_att16_3])

  # For attribute 7
  variable_list = train_data['F7'].unique()
  # print(variable_list )
  attribute7_list = []
  for var in variable_list:
    P_C0_att = len(label_zero.loc[label_zero['F7'] == var])/len(label_zero)
    P_C1_att = len(label_one.loc[label_one['F7'] == var])/len(label_one)
    attribute7_list.append([var, P_C0_att, P_C1_att])
  # print(attribute7_list)

  # For attribute 8
  variable_list = train_data['F8'].unique()
  # print(variable_list )
  attribute8_list = []
  for var in variable_list:
    P_C0_att = len(label_zero.loc[label_zero['F8'] == var])/len(label_zero)
    P_C1_att = len(label_one.loc[label_one['F8'] == var])/len(label_one)
    attribute8_list.append([var, P_C0_att, P_C1_att])


  # For attribute 9
  variable_list = train_data['F9'].unique()
  # print(variable_list )
  attribute9_list = []
  for var in variable_list:
    P_C0_att = len(label_zero.loc[label_zero['F9'] == var])/len(label_zero)
    P_C1_att = len(label_one.loc[label_one['F9'] == var])/len(label_one)
    attribute9_list.append([var, P_C0_att, P_C1_att])

  # For attribute 10
  variable_list = train_data['F10'].unique()
  # print(variable_list )
  attribute10_list = []
  for var in variable_list:
    P_C0_att = len(label_zero.loc[label_zero['F10'] == var])/len(label_zero)
    P_C1_att = len(label_one.loc[label_one['F10'] == var])/len(label_one)
    attribute10_list.append([var, P_C0_att, P_C1_att])

  # For attribute 11
  variable_list = train_data['F11'].unique()
  # print(variable_list )
  attribute11_list = []
  for var in variable_list:
    P_C0_att = len(label_zero.loc[label_zero['F11'] == var])/len(label_zero)
    P_C1_att = len(label_one.loc[label_one['F11'] == var])/len(label_one)
    attribute11_list.append([var, P_C0_att, P_C1_att])
  output_list = []
  # first time print out
  first = True
  i = 0
  for _, row_test in test_data.iterrows():
    P_C0_test = 1.0
    P_C1_test = 1.0

    for i in range(11):
      if i ==0:
        text ='F1'
        if row_test[text] < 5:
          P_C0_test *= attribute1_list[0][1]
          P_C1_test *= attribute1_list[0][2]
        elif row_test[text] < 15:
          P_C0_test *= attribute1_list[1][1]
          P_C1_test *= attribute1_list[1][2]
        else:
          P_C0_test *= attribute1_list[2][1]
          P_C1_test *= attribute1_list[2][2]

      if i ==1:
        text ='F2'
        if row_test[text] < 10:
          P_C0_test *= attribute2_list[0][1]
          P_C1_test *= attribute2_list[0][2]
        elif row_test[text] < 25:
          P_C0_test *= attribute2_list[1][1]
          P_C1_test *= attribute2_list[1][2]
        elif row_test[text] < 50:
          P_C0_test *= attribute2_list[2][1]
          P_C1_test *= attribute2_list[2][2]
        else  :
          P_C0_test *= attribute2_list[3][1]
          P_C1_test *= attribute2_list[3][2]          


      if i ==2:
        text ='F3'
        variable_list = train_data['F3'].unique()
        list_convert = variable_list.tolist() 
        for var in variable_list:
          if row_test[text] == var:
            # print(list_convert.index(var)) 
            P_C0_test *= attribute3_list[list_convert.index(var)][1]
            P_C1_test *= attribute3_list[list_convert.index(var)][2]

      if i ==3:
        text ='F4'
        variable_list = train_data['F4'].unique()
        list_convert = variable_list.tolist() 
        for var in variable_list:
          if row_test[text] == var:
            # print(list_convert.index(var)) 
            P_C0_test *= attribute4_list[list_convert.index(var)][1]
            P_C1_test *= attribute4_list[list_convert.index(var)][2]

      if i ==4:
        text ='F5'
        if row_test[text] == 0:
          P_C0_test *= attribute5_list[0][1]
          P_C1_test *= attribute5_list[0][2]
        elif row_test[text] < 500:
          P_C0_test *= attribute5_list[1][1]
          P_C1_test *= attribute5_list[1][2]
        else  :
          P_C0_test *= attribute5_list[2][1]
          P_C1_test *= attribute5_list[2][2]  

      if i ==5:
        text ='F6'
        if row_test[text] == 0:
          P_C0_test *= attribute6_list[0][1]
          P_C1_test *= attribute6_list[0][2]
        elif row_test[text] < 500:
          P_C0_test *= attribute6_list[1][1]
          P_C1_test *= attribute6_list[1][2]
        else  :
          P_C0_test *= attribute6_list[2][1]
          P_C1_test *= attribute6_list[2][2]  

      if i ==6:
        text ='F7'
        variable_list = train_data['F7'].unique()
        list_convert = variable_list.tolist() 
        for var in variable_list:
          if row_test[text] == var:
            # print(list_convert.index(var)) 
            P_C0_test *= attribute7_list[list_convert.index(var)][1]
            P_C1_test *= attribute7_list[list_convert.index(var)][2]

      if i ==7:
        text ='F8'
        variable_list = train_data['F8'].unique()
        list_convert = variable_list.tolist() 
        for var in variable_list:
          if row_test[text] == var:
            # print(list_convert.index(var)) 
            P_C0_test *= attribute8_list[list_convert.index(var)][1]
            P_C1_test *= attribute8_list[list_convert.index(var)][2]

      if i ==8:
        text ='F9'
        variable_list = train_data['F9'].unique()
        list_convert = variable_list.tolist() 
        for var in variable_list:
          if row_test[text] == var:
            # print(list_convert.index(var)) 
            P_C0_test *= attribute9_list[list_convert.index(var)][1]
            P_C1_test *= attribute9_list[list_convert.index(var)][2]

      if i ==9:
        text ='F10'
        variable_list = train_data['F10'].unique()
        list_convert = variable_list.tolist() 
        for var in variable_list:
          if row_test[text] == var:
            # print(list_convert.index(var)) 
            P_C0_test *= attribute10_list[list_convert.index(var)][1]
            P_C1_test *= attribute10_list[list_convert.index(var)][2]

      if i ==10:
        text ='F11'
        variable_list = train_data['F11'].unique()
        list_convert = variable_list.tolist() 
        for var in variable_list:
          if row_test[text] == var:
            # print(list_convert.index(var)) 
            P_C0_test *= attribute11_list[list_convert.index(var)][1]
            P_C1_test *= attribute11_list[list_convert.index(var)][2]

    P_C0_test = P_C_0*P_C0_test
    P_C1_test = P_C_1*P_C1_test
    if P_C0_test > P_C1_test:
      output_list.append(0)
    else:
      output_list.append(1)


      
  with open('bayesian_results1.txt', 'w') as filehandle:
    for listitem in output_list:
        filehandle.write('%s\n' % listitem)
  filehandle.close()

