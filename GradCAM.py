import cv2
import numpy as np
import torch
from torch import nn
from torchvision import transforms
from pytorch_grad_cam import GradCAM
from pytorch_grad_cam.utils.image import show_cam_on_image
from torchvision.transforms import Compose, Normalize, ToTensor
from pytorch_grad_cam.utils.image import show_cam_on_image, preprocess_image
import random
import torchvision
from PIL import Image
import os
import shutil
import copy
from torchviz import make_dot


class CNN(nn.Module):
    def __init__(self,num=2,f=60):
        super(CNN,self).__init__()
        self.conv0 = nn.Conv2d(3, 3, kernel_size=12,padding=2,stride = 2)
        self.conv1 = nn.Conv2d(3, 6, kernel_size=5,padding=2)
        self.conv2 = nn.Conv2d(6,16,kernel_size=5)
        self.conv3 = nn.Conv2d(16,120,kernel_size=5)
        self.mp = nn.MaxPool2d(2)
        self.relu = nn.LeakyReLU()
        self.fc1 = nn.Linear(120,f)
        self.bn = nn.BatchNorm1d(f)
        self.fc2 = nn.Linear(f,num)
        self.softmax = nn.Softmax()
        self.dropout = nn.Dropout(0.1)
    
    
    def forward(self,x):
        
        if x.size(-1) != 120 or x.size(-2) != 120:
            x = torch.nn.functional.interpolate(x, size=(120, 120), mode='bilinear', align_corners=False)
            
        in_size = x.size(0)  
        out = self.relu(self.mp(self.conv0(x)))
        out = self.relu(self.mp(self.conv1(out)))
        out = self.relu(self.mp(self.conv2(out)))
        out = self.relu(self.conv3(out))
        out = out.view(in_size, -1)
        out = self.relu(self.fc1(out))
        out = self.relu(self.bn(out))
        out = self.fc2(out)
        return out
    
    
#Grad-CAM

net = CNN()
net.load_state_dict(torch.load(PATH))

cam = GradCAM(model=net, target_layers=[net.conv0,net.conv1,net.conv2,net.conv3], use_cuda=False)
# cam = GradCAM(model=net, target_layers=net.feature_extractor, use_cuda=False)
target_category = None
grayscale_cam = cam(input_tensor=input_tensor, targets=target_category)
grayscale_cam = grayscale_cam[0]
alpha = 0.3
    
visualization = show_cam_on_image(0, grayscale_cam)

img = np.array(img)

if len(img.shape) == 3:
    img = cv2.cvtColor(img, cv2.COLOR_RGB2BGR)
    
alpha = 0.7
savepath = ""

mixpic = cv2.addWeighted(visualization, alpha, img, 1-alpha,0,0,dtype = cv2.CV_32F)
mixpic = cv2.normalize(mixpic,None,0,255,cv2.NORM_MINMAX)
allpic = np.concatenate([img,visualization,mixpic],axis=1)
if(savepath):
    if(os.path.exists(savepath)):
        shutil.rmtree(savepath)
    os.mkdir(savepath)
    cv2.imwrite(savepath + '\\' + 'ori.jpg',img)
    cv2.imwrite(savepath + '\\' + 'cam.jpg',visualization)
    cv2.imwrite(savepath + '\\' + 'mix.jpg',mixpic)
    cv2.imwrite(savepath + '\\' + 'all.jpg',allpic)
