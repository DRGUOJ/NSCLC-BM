import cv2
import numpy as np
import os


image_phase1 = cv2.imread("", cv2.IMREAD_GRAYSCALE)
image_phase2 = cv2.imread("", cv2.IMREAD_GRAYSCALE)


merged_image_avg = cv2.addWeighted(image_phase1, 0.5, image_phase2, 0.5, 0)
merged_image_avg = (merged_image_avg - np.min(merged_image_avg)) / (np.max(merged_image_avg) - np.min(merged_image_avg)) * 255
merged_image_avg = merged_image_avg.astype(np.uint8)

merged_image_concat = cv2.merge((image_phase1, image_phase2,merged_image_avg))

cv2.imwrite("",merged_image_concat)



