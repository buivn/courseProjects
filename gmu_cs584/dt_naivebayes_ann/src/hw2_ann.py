import torch
import torch.nn as nn
import numpy as np
import pandas as pd
from matplotlib import pyplot as plt
# %matplotlib inline
import seaborn as sns

import os
import sys
from sklearn.metrics import classification_report, confusion_matrix, accuracy_score


# define the device as the first visible cude device if we have CUDA available
device = torch.device("cuda:0" if torch.cuda.is_available() else "cpu")

# read the data frame
csv_file = os.path.join(sys.path[0], "train_data.csv")
train_data = pd.read_csv(csv_file, delimiter=',')
csv_file = os.path.join(sys.path[0], "test_data.csv")
test_data = pd.read_csv(csv_file, delimiter=',')

# drop the id column
train_data = train_data.drop('id', axis=1)
test_data = test_data.drop('id', axis=1)

# categorical columns
categorical_columns = ['F10', 'F11']
#  numerical columns
# numerical_columns = ['F1', 'F2', 'F3', 'F4', 'F5', 'F6', 'F7', 'F8', 'F9']
numerical_columns = ['F1', 'F2', 'F3', 'F4', 'F5', 'F6', 'F7', 'F8', 'F9']

# outputs variable
outputs = ['credit']

# convert the types for categorical columns to category
for category in categorical_columns:
    train_data[category] = train_data[category].astype('category')
    test_data[category] = test_data[category].astype('category')

# convert the categorical columns to tensors
# step 1: convert data in the four categorical columns into numpy array
train_F10 = train_data['F10'].cat.codes.values
train_F11 = train_data['F11'].cat.codes.values
test_F10 = test_data['F10'].cat.codes.values
test_F11 = test_data['F11'].cat.codes.values

# stack all the columns horizontally
categorical_train_data = np.stack([train_F10, train_F11], 1)
categorical_test_data = np.stack([test_F10, test_F11], 1)

# create a tensor from the aforementioned numpy array
categorical_train_data = torch.tensor(categorical_train_data, dtype=torch.int64)
categorical_test_data = torch.tensor(categorical_test_data, dtype=torch.int64)

# convert the numerical columns to tensors
numerical_train_data = np.stack([train_data[col].values for col in numerical_columns], 1)
numerical_train_data = torch.tensor(numerical_train_data, dtype=torch.float)
numerical_test_data = np.stack([test_data[col].values for col in numerical_columns], 1)
numerical_test_data = torch.tensor(numerical_test_data, dtype=torch.float)

# convert the output numpy array into a tensor object.
train_outputs = torch.tensor(train_data[outputs].values).flatten()


# Embedding - convert the categorical columns in the form of N-dimensional vector instead of a single integer
# N - embedded size = divide the number of unique values in the column by 2 (not over 50)
categorical_column_sizes = [len(train_data[column].cat.categories) for column in categorical_columns]
categorical_embedding_sizes = [(col_size, min(50, (col_size+1)//2)) for col_size in categorical_column_sizes]

# send the data to cuda
categorical_train_data = categorical_train_data.to(device)
categorical_test_data = categorical_test_data.to(device)
numerical_train_data = numerical_train_data.to(device)
numerical_test_data = numerical_test_data.to(device)
train_outputs = train_outputs.to(device)

# Creating a Model for Prediction
class Model(nn.Module):
  def __init__(self, embedding_size, num_numerical_cols, output_size, layers, p=0.4):
    super().__init__()
    self.all_embeddings = nn.ModuleList([nn.Embedding(ni, nf) for ni, nf in embedding_size])
    self.embedding_dropout = nn.Dropout(p)
    self.batch_norm_num = nn.BatchNorm1d(num_numerical_cols)
    # all_embeddings variable contains a list of ModuleList objects for all the categorical columns. 
    # embedding_dropout stores the dropout value for all the layers. 
    # batch_norm_num stores a list of BatchNorm1d objects for all the numerical columns.

    all_layers = []
    num_categorical_cols = sum((nf for ni, nf in embedding_size))
    input_size = num_categorical_cols + num_numerical_cols
    # input_size size of the input layer = the number of categorical + numerical columns

    for i in layers:
        # Linear: calculate the dot product between the inputs and weight matrixes
        all_layers.append(nn.Linear(input_size, i))
        all_layers.append(nn.ReLU(inplace=True))
        # BatchNorm1d: Used to apply batch normalization to the numerical columns
        all_layers.append(nn.BatchNorm1d(i))
        # Dropout: Used to avoid overfitting
        all_layers.append(nn.Dropout(p))
        input_size = i
    # output layer is appended to the list of layers
    all_layers.append(nn.Linear(layers[-1], output_size))

    self.layers = nn.Sequential(*all_layers)

  def forward(self, x_categorical, x_numerical):
    # The embedding of the categorical columns
    embeddings = []
    for i,e in enumerate(self.all_embeddings):
      embeddings.append(e(x_categorical[:,i]))
    x = torch.cat(embeddings, 1)
    x = self.embedding_dropout(x)
    # The batch normalization of the numerical columns is applied
    x_numerical = self.batch_norm_num(x_numerical)
    x = torch.cat([x, x_numerical], 1)
    x = self.layers(x)
    return x

# Training the Model
# three hidden layers with 80, 40, and 10 neurons
# with cude
model = Model(categorical_embedding_sizes, numerical_test_data.shape[1], 2, [80,40,10], p=0.4).to(device)

# print out the model parameters
# loss function (for a classification problem) - use the cross entropy loss.
loss_function = nn.CrossEntropyLoss()
# optimizer function use the adam optimizer.
optimizer = torch.optim.Adam(model.parameters(), lr=0.001)
# optimizer = torch.optim.SGD(model.parameters(), lr=0.001, momentum=0.9)

epochs = 600
aggregated_losses = []

for i in range(epochs):
    i += 1
    # set the gradient to zero
    optimizer.zero_grad()
    # forward - backward - optimize
    y_pred = model(categorical_train_data, numerical_train_data)
    single_loss = loss_function(y_pred, train_outputs)
    # the weights are updated by backward() function of the single_loss object
    aggregated_losses.append(single_loss)
    single_loss.backward()
    # the step() method updates the gradient
    optimizer.step()

    if i%25 == 1:
        print(f'epoch: {i:3} loss: {single_loss.item():10.8f}')

print(f'epoch: {i:3} loss: {single_loss.item():10.10f}')
print('Finish Training')


# Making Predictions
with torch.no_grad():
    y_val = model(categorical_test_data, numerical_test_data)
    # loss = loss_function(y_val, test_outputs)
# print(f'Loss: {loss:.8f}')

# print('The output of first 5 data:\n', y_val[:5])
_, y_val1 = torch.max(y_val, 1)
# print('The prediction result of first 5 data:\n', y_val1[:5])

with open('results.txt','a') as fd:
    fd.write('\n'.join(map(str, y_val1.detach().tolist())) + '\n')




