import os
import shutil
import cv2
import numpy as np



oripath = r''
topath = r''


#Prewitt
names = os.listdir(oripath)
for name in names:
    path = oripath + '\\' + name
    savepath = topath + '\\' + name
    
    image = cv2.imread(path, cv2.IMREAD_GRAYSCALE)
    
    kernel_x = np.array([[-1, 0, 1], [-1, 0, 1], [-1, 0, 1]])
    kernel_y = np.array([[-1, -1, -1], [0, 0, 0], [1, 1, 1]])
    prewitt_x = cv2.filter2D(image, -1, kernel_x)
    prewitt_y = cv2.filter2D(image, -1, kernel_y)
    prewitt_x = prewitt_x.astype(np.float32)
    prewitt_y = prewitt_y.astype(np.float32)
    prewitt_edges = cv2.magnitude(prewitt_x, prewitt_y)
    
    cv2.imwrite(savepath, prewitt_edges)
    


#Canny
names = os.listdir(oripath)
for name in names:
    path = oripath + '\\' + name
    savepath = topath + '\\' + name
    
    image = cv2.imread(path, cv2.IMREAD_GRAYSCALE)
    
    canny_edges = cv2.Canny(image, 40, 80)
    
    cv2.imwrite(savepath, canny_edges)
    
    
    
#Laplacian √
kernel=np.array([[0,-1,0],[-1,6,-1],[0,-1,0]],np.float32)

names = os.listdir(oripath)
for name in names:
    path = oripath + '\\' + name
    savepath = topath + '\\' + name
    
    image = cv2.imread(path, cv2.IMREAD_GRAYSCALE)
    
    pic = cv2.filter2D(image,-1,kernel=kernel)
    pic = np.clip(pic,0,255)
    
    cv2.imwrite(savepath, pic)
    
    
#Sobel
names = os.listdir(oripath)
for name in names:
    path = oripath + '\\' + name
    savepath = topath + '\\' + name
    
    image = cv2.imread(path, cv2.IMREAD_GRAYSCALE)
    
    sobel_x = cv2.Sobel(image, cv2.CV_64F, 1, 0, ksize=3)
    sobel_y = cv2.Sobel(image, cv2.CV_64F, 0, 1, ksize=3)
    sobel_edges = cv2.magnitude(sobel_x, sobel_y)
    
    
    cv2.imwrite(savepath, sobel_edges)