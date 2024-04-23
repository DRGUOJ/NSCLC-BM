import os
import cv2
import heapq 
import numpy as np

def MaxROI(path,maskname):
    length = 0
    width = 0
    mid = []
    midname = []
    picname = []
    L = []
    W = []
    names = os.listdir(path)
    numm = len(names)
    count = 0
    for name in names:
        count += 1
        
        names2 = os.listdir(path + '\\' + str(name) + '\\' + maskname)
        maxx = 0
        maxname = 'x'
        for name2 in names2:
            img = cv2.imread(path + '\\' + str(name) + '\\' + maskname + '\\' + str(name2))
            img = cv2.cvtColor(img,cv2.COLOR_BGR2GRAY)
            
            contours, hierarchy = cv2.findContours(img, cv2.RETR_LIST, cv2.CHAIN_APPROX_NONE)
            contoursshape = []  
            for i in contours:
                contoursshape.append(i.shape[0])
            max_number = heapq.nlargest(1, contoursshape)
            if(len(max_number)):
                t = max_number[0]
                if(t>maxx):
                    maxx = t
                    maxname = name2
        
        img = cv2.imread(path + '\\' + str(name) + '\\' + maskname + '\\' + str(maxname))
        img = cv2.cvtColor(img,cv2.COLOR_BGR2GRAY)
        
        contours, hierarchy = cv2.findContours(img, cv2.RETR_LIST, cv2.CHAIN_APPROX_NONE)
        contoursshape = []  
        for i in contours:
            contoursshape.append(i.shape[0])
        max_index = list(map(contoursshape.index, heapq.nlargest(1, contoursshape)))
        contours = contours[max_index[0]]
        x = []
        y = []
        for i in range(contours.shape[0]):
            x.append(contours[i][0][0])
            y.append(contours[i][0][1])
                    
        left = min(x)
        right = max(x)
        up = min(y)
        down = max(y)
        l = right - left
        w = down - up
        L.append(l)
        W.append(w)
        length += l
        width += w
        picname.append(maxname)
        midname.append(name)
        mid.append([round((left+right)/2,0),round((up+down)/2,0)])
    
    length /= numm
    width /= numm

    return midname,picname,mid,length,width,L,W



def Cut(midname,picname,mid,L,W,path,outpath,oriname,siz,maskname,black,roibig):
    count = 0
    num = len(midname)

    for k in range(num):

        count += 1

        tempts = path + '\\' + str(midname[k]) + '\\' + oriname + '\\' + str(picname[k])

        img = cv2.imread(tempts)
        img = cv2.cvtColor(img, cv2.COLOR_BGR2GRAY)




        midx = mid[k][0]
        midy = mid[k][1]

        length = L[k]*siz
        width = W[k]*siz
        left = int(midx) - int(length/2)
        right = int(midx) + int(length/2)
        up = int(midy) - int(width/2)
        down = int(midy) + int(width/2)


        img =  img[int(up):int(down),int(left):int(right)]


        if (black):
            maskimg = cv2.imread(path + '\\' + str(midname[k]) + '\\' + maskname + '\\' + str(picname[k]))
            maskimg = cv2.cvtColor(maskimg, cv2.COLOR_BGR2GRAY)
            kernel = np.ones((roibig + 1, roibig + 1), np.uint8)
            maskimg = cv2.dilate(maskimg, kernel, iterations=1)
            maskimg = maskimg[int(up):int(down), int(left):int(right)]
            maskimg = maskimg.astype(img.dtype)
            img = cv2.bitwise_and(img, maskimg)




        out_path = outpath + '\\' + str(midname[k]) + '.jpg'
        cv2.imwrite(out_path,img)
        
        
        
