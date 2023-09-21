import torch
import torch.nn as nn
from gan.spectral_normalization import SpectralNorm

class Discriminator(torch.nn.Module):
    def __init__(self, input_channels=3):
        super(Discriminator, self).__init__()
        
        #Hint: Hint: Apply spectral normalization to convolutional layers. Input to SpectralNorm should be your conv nn module
        ####################################
        #          YOUR CODE HERE          #
        ####################################
        # self.conv1 = SpectralNorm(nn.Conv2d(3, 128, kernel_size=4, stride=2, padding=1, bias=False))
        # self.conv2 = SpectralNorm(nn.Conv2d(128, 128, kernel_size=4, stride=2, padding=1, bias=False))
        # self.conv3 = SpectralNorm(nn.Conv2d(128, 256, kernel_size=4, stride=2, padding=1, bias=False))       
        # self.conv4 = SpectralNorm(nn.Conv2d(256, 256, kernel_size=4, stride=2, padding=1, bias=False))
        # self.conv5 = SpectralNorm(nn.Conv2d(256, 512, kernel_size=4, stride=1, padding=1, bias=False))
        # self.conv6 = SpectralNorm(nn.Conv2d(512, 1024, kernel_size=4, stride=1, padding=1, bias=False))
        # self.conv7 = SpectralNorm(nn.Conv2d(1024, 1, kernel_size=2, stride=2, padding=0, bias=False))

        self.conv1 = SpectralNorm(nn.Conv2d(3, 128, kernel_size=4, stride=2, padding=1, bias=False))
        self.conv2 = SpectralNorm(nn.Conv2d(128, 256, kernel_size=4, stride=2, padding=1, bias=False))
        self.conv3 = SpectralNorm(nn.Conv2d(256, 512, kernel_size=4, stride=2, padding=1, bias=False))       
        self.conv4 = SpectralNorm(nn.Conv2d(512, 512, kernel_size=4, stride=2, padding=1, bias=False))
        self.conv5 = SpectralNorm(nn.Conv2d(512, 1024, kernel_size=4, stride=2, padding=1, bias=False))
        self.conv6 = SpectralNorm(nn.Conv2d(1024, 1, kernel_size=1, stride=1, padding=0, bias=False))
        self.fc = nn.Linear(4*4, 1)
        self.leakyRelu = nn.LeakyReLU(negative_slope=0.2)
        # self.out = torch.nn.Sigmoid()
        
        
        ##########       END      ##########
    
    def forward(self, x):
        
        ####################################
        #          YOUR CODE HERE          #
        ####################################
        # inputdata_dimension = 128x128
        # Pass data through conv1 
        x = self.conv1(x)
        # x = SpectralNorm(x)
        # Use the rectified-linear activation function over x
        x = self.leakyRelu(x)
        # x = 64x64
        x = self.conv2(x)
        x = self.leakyRelu(x)
        # x = 32x32
        x = self.conv3(x)
        x = self.leakyRelu(x)
        # x = 16x16
        x = self.conv4(x)
        x = self.leakyRelu(x)
        # x = 8x8
        x = self.conv5(x)
        x = self.leakyRelu(x)
        # x = 4x4
        x = self.conv6(x)
        x = self.leakyRelu(x)

        # convert the image into a vector
        x = x.view(x.size()[0], 1 * 4 * 4)
        x = self.fc(x)
        
        #########       END      ##########
        
        return x


class Generator(torch.nn.Module):
    def __init__(self, noise_dim, output_channels=3):
        super(Generator, self).__init__()    
        self.noise_dim = noise_dim
        
        ####################################
        #          YOUR CODE HERE          #
        ####################################

        self.conv1_transpose = nn.ConvTranspose2d(self.noise_dim, 1024, kernel_size=4, stride=1, padding=0, bias=False)
        self.conv1_bn=nn.BatchNorm2d(1024)
        self.conv2_transpose = nn.ConvTranspose2d(1024, 512, kernel_size=4, stride=2, padding=1, bias=False)
        self.conv2_bn=nn.BatchNorm2d(512)
        self.conv3_transpose = nn.ConvTranspose2d(512, 512, kernel_size=4, stride=2, padding=1, bias=False)
        self.conv3_bn=nn.BatchNorm2d(512)
        self.conv4_transpose = nn.ConvTranspose2d(512, 256, kernel_size=4, stride=2, padding=1, bias=False)
        self.conv4_bn=nn.BatchNorm2d(256)
        self.conv5_transpose = nn.ConvTranspose2d(256, 128, kernel_size=4, stride=2, padding=1, bias=False)
        self.conv5_bn=nn.BatchNorm2d(128)
        self.conv6_transpose = nn.ConvTranspose2d(128, 3, kernel_size=4, stride=2, padding=1, bias=False)
        self.leakyRelu = nn.LeakyReLU(negative_slope=0.2)
        self.tan = nn.Tanh()
        
        # self.pool = nn.MaxPool2d(2, 2)
        # self.fc1 = nn.Linear(26 * 26, 1)        
        
        ##########       END      ##########
    
    def forward(self, x):
        
        ####################################
        #          YOUR CODE HERE          #
        ####################################
        x = self.conv1_transpose(x)
        x = self.conv1_bn(x)
        x = self.leakyRelu(x)
        
        x = self.conv2_transpose(x)
        x = self.conv2_bn(x)
        x = self.leakyRelu(x)
        
        x = self.conv3_transpose(x)
        x = self.conv3_bn(x)
        x = self.leakyRelu(x)

        x = self.conv4_transpose(x)
        x = self.conv4_bn(x)
        x = self.leakyRelu(x)


        x = self.conv5_transpose(x)
        x = self.conv5_bn(x)
        x = self.leakyRelu(x)

        x = self.conv6_transpose(x)
        x = self.tan(x)

        
        ##########       END      ##########
        return x
    

