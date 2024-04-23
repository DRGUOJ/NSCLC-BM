import cv2
import numpy as np


# standard
path = ''
savepath = ''

pic = cv2.imread(path)
            
mean = np.mean(pic)
var = np.mean(np.square(pic-mean))
pic = 255*(pic - mean)/np.sqrt(var)

# cv2.imwrite(savepath,pic)


#Scale
pic = 255*((pic  - np.min(pic)) / (np.max(pic) - np.min(pic)))




#Padding & Resize
l = 120
w = 120
newl = l
neww = w
oril = pic.shape[1]
oriw = pic.shape[0]
if(oril >= l):#如果图像过大则不padding
    x = 0
    newl = oril
else:#否则padding  x为横着padding的大小
    x = l - oril
    x = int(x/2)
if(oriw >= w):
    y = 0
    neww = oriw
else:
    y = w - oriw
    y = int(y/2)
pic = cv2.copyMakeBorder(pic, x, x, y, y, cv2.BORDER_CONSTANT, value=(0,0,0))
pic = cv2.resize(pic,(newl,neww),interpolation=cv2.INTER_AREA)

