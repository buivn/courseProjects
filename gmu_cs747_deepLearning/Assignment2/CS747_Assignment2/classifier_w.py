import torch
import torch.nn as nn
from torch.autograd import Variable
import torch.nn.functional as F
from torch import optim
import numpy as np

NUM_CLASSES = 21


class SimpleClassifier(nn.Module):
    def __init__(self):
        super(SimpleClassifier, self).__init__()
        self.conv1 = nn.Conv2d(3, 64, 5)
        self.conv2 = nn.Conv2d(64, 32, 3)
        self.conv3 = nn.Conv2d(32, 16, 3)
        self.pool = nn.MaxPool2d(2, 2)
        self.fc1 = nn.Linear(16 * 26 * 26, 120)
        self.fc2 = nn.Linear(120, 84)
        self.fc3 = nn.Linear(84, NUM_CLASSES)

    def forward(self, x):
        x = self.pool(F.relu(self.conv1(x)))
        x = self.pool(F.relu(self.conv2(x)))
        x = self.pool(F.relu(self.conv3(x)))
        x = x.view(x.size()[0], 16 * 26 * 26)
        x = F.relu(self.fc1(x))
        x = F.relu(self.fc2(x))
        x = self.fc3(x)
        return x

 
class block(nn.Module):
    def __init__(self, in_channels, out_channels, iden_downsample=None, stride=1):
        super(block, self).__init__()
        # self.expansion = 4
        self.conv1 = nn.Conv2d(in_channels, out_channels, kernel_size=7, stride=1, padding=3)
        self.conv1_bn=nn.BatchNorm2d(out_channels)
        self.conv2 = nn.Conv2d(out_channels, out_channels, kernel_size=5, stride=1,padding=2)
        self.conv2_bn=nn.BatchNorm2d(out_channels)
        self.relu = nn.ReLU()
        self.pool = nn.MaxPool2d(kernel_size=2, stride=stride)
        self.iden_downsample = iden_downsample

    def forward(self, x):
        iden = x
        # print(iden.shape)
        x = self.conv1(x)
        # print(x.shape)
        x = self.conv1_bn(x)
        # print(iden.shape)
        x = self.relu(x)
        x = self.pool(x)
        # print(x.shape)
        x = self.conv2(x)
        # print(x.shape)
        x = self.conv2_bn(x)
        # print(x.shape)
        if self.iden_downsample is not None:
            iden = self.iden_downsample(iden)
        # print(iden.shape)
        x += iden
        x = self.relu(x)
        return x


class block3(nn.Module):
    def __init__(self, in_channels, out_channels, iden_downsample=None, stride=1):
        super(block, self).__init__()
        # self.expansion = 4
        self.conv1 = nn.Conv2d(in_channels, in_channels, kernel_size=3, stride=1,padding=1)
        self.conv1_bn=nn.BatchNorm2d(out_channels)
        self.conv2 = nn.Conv2d(in_channels, in_channels, kernel_size=3, stride=stride,padding=1)
        self.conv2_bn=nn.BatchNorm2d(out_channels)
        self.conv3 = nn.Conv2d(in_channels, out_channels, kernel_size=3, stride=1,padding=1)
        self.conv3_bn=nn.BatchNorm2d(out_channels)
        self.relu = nn.ReLU()
        self.iden_downsample = iden_downsample

    def forward(self, x):
        iden = x
        # print(iden.shape)
        x = self.conv1(x)
        x = self.conv1_bn(x)
        x = self.relu(x)
        x = self.conv2(x)
        x = self.conv2_bn(x)
        x = self.relu(x)
        # print(x.shape)
        x = self.conv3(x)
        x = self.conv3_bn(x)
        # print(x.shape)
        if self.iden_downsample is not None:
            iden = self.iden_downsample(iden)
        # print(iden.shape)
        x += iden
        x = self.relu(x)
        return x


class Classifier(nn.Module):  
    # TODO: implement me
    def __init__(self, block=block):
        super(Classifier, self).__init__()

        self.conv1 = nn.Conv2d(3, 64, kernel_size=7, stride=1, padding=3)
        # torch.nn.init.xavier_uniform(self.conv1.weight)
        self.conv1_bn=nn.BatchNorm2d(64)
        self.relu = nn.ReLU()
        self.pool1 = nn.MaxPool2d(kernel_size=2, stride=2, padding=1)
        self.pool2 = nn.MaxPool2d(kernel_size=2, stride=2)

        # adding block layers
        self.b_layer1 = self.make_layers(block, 3, in_channels=64, out_channels=80, stride=2)
        self.b_layer2 = self.make_layers(block, 3, in_channels=80, out_channels=128, stride=2)
        # self.b_layer2 = self.make_layers(block, 2, in_channels=128, out_channels=128, stride=2)
        # self.b_layer3 = self.make_layers(block, 2, in_channels=128, out_channels=256, stride=2)
        # self.b_layer4 = self.make_layers(block, 2, in_channels=256, out_channels=512, stride=2)
        
        # self.conv2 = nn.Conv2d(256, 128, kernel_size=3, stride=2,padding=1)
        # self.conv2_bn=nn.BatchNorm2d(512)

        
        self.avgpool = nn.AdaptiveAvgPool2d((1,1))
        self.fc1 = nn.Linear(128, 64)        
        # self.fc1 = nn.Linear(256, 128)
        self.fc2 = nn.Linear(64, NUM_CLASSES)
        # self.softmax = nn.softmax()
        self.drop_out = nn.Dropout(0.5)


    def forward(self, x):
        # print(x.shape)
        x = self.conv1(x)
        # print(x.shape)
        x = self.conv1_bn(x)
        x = self.relu(x)
        # print(x.shape)
        x = self.pool1(x)
        # print(x.shape)


        x = self.b_layer1(x)
        print(x.shape)
        x = self.pool2(x)
        print(x.shape)
        x = self.b_layer2(x)
        print(x.shape)
        # x = torch.flatten(x)
        # print(x.shape)
        
        # x = self.b_layer2(x)
        # x = self.b_layer3(x)
        # x = self.b_layer4(x)
        # x = self.conv2(x)
        # x = self.conv2_bn(x)
        # x = self.relu(x)
        # print(x.shape)
        # x = x.view(x.size()[0], 128 * 15 * 15)
        x = self.avgpool(x)
        # print(x.shape)
        x = x.reshape(x.shape[0],-1)
        # print(x.shape)
        x = self.fc1(x)
        x = self.relu(x)
        x = self.drop_out(x)
        x = self.fc2(x)
        # print(x.shape)
        # x = self.softmax(x, dim=0)

        return x
        

    def make_layers(self, block, num_blocks, in_channels, out_channels, stride):
        iden_downsample = None
        layers = []

        if (stride != 1) or (len(layers)==0):
            iden_downsample = nn.Sequential(nn.Conv2d(in_channels, out_channels, \
                                            kernel_size=1, stride=1),
                                    nn.BatchNorm2d(out_channels),
                                    nn.MaxPool2d(kernel_size=2, stride=stride))
        layers.append(block(in_channels, out_channels, iden_downsample, stride))
        
        # layers.append(block(out_channels, out_channels))
        # layers.append(block(out_channels, out_channels))
        return nn.Sequential(*layers)

def Check():
    return Classifier()


def test():
    net = Check()
    x = torch.randn(2,3,224,224)
    y = net(x).to('cuda')
    print(y.shape)

test()

