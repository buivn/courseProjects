import numpy as np
import matplotlib.pyplot as plt
import matplotlib.image as img
import os
import pca

def load_data(input_dir):
  # train_image_name = [f for f in os.listdir(basepath + foldername+ '/train/original/') if '.pgm' in f]
  # name list of all image with .pgm in a folder
  train_image_name = [f for f in os.listdir(input_dir) if '.pgm' in f]
  image = img.imread(input_dir+train_image_name[0])

  Data = np.zeros((len(image)*len(image[0]), len(train_image_name)))
  
  i = 0 # runs with the image order
  for image_name in train_image_name:
    image = img.imread(input_dir+image_name)   
    for k in range(len(image)):
      for m in range(len(image[0])):
        Data[k*len(image[0])+m][i] = image[k][m]
    i += 1 # move to the next image
  
  return Data



def compress_images(DATA, k):
  # transpose command 
  Data_T = DATA.transpose()

  Z = pca.compute_Z(Data_T, True, False)
  COV = pca.compute_covariance_matrix(Z)
  L, PCS = pca.find_pcs(COV)
  pr_Data = pca.project_data(Z, PCS, L, k, 0)
  
  # get the principle component - complex matrix
  U = np.zeros((len(PCS),k), dtype=complex)
  for i in range(len(PCS)):
    for j in range(k):
      U[i][j] = PCS[i][j]

  # transpose of the principles components -( eigenvector)
  U_T = U.transpose()

  # get the data back the original size
  X = np.matmul(pr_Data, U_T)

  X -= X.min()
  # scaling to range 0-255
  X_inter = np.divide(X,255/X.max())
  # convert to int
  X_inter2 = X_inter.astype(int)


  # check folder exists?
  foldername = './Data/output'
  # # check the folder output exist?
  if (not os.path.isdir(foldername)):
    os.mkdir(foldername) 

  backImage = np.zeros([60,48], dtype =int)
  row = 0
  column = 0
  for j in range(len(X_inter2)):
    # plt.imsave(foldername,X_compressed, format="'png'")
    for k in range(len(X_inter2[0])):
      if (column == 48):
        row += 1
        column = 0
      backImage[row][column] = X_inter2[j][k]
      column += 1
    row = 0
    column = 0
    name = str(j)
    plt.imsave('./Data/output/'+name, backImage, format="png")

  return 0


if __name__ == '__main__':
    

  X = load_data('Data/Train/')
  # print(X)
  # compress_images(X, 10)
  compress_images(X, 100)



  