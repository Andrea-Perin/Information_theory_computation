import numpy as np
import os


sizes='matinfo.txt'
results='results.txt'

#removing the previous results if they are still there
if os.path.exists(sizes):
    os.remove(sizes)
if os.path.exists(results):
    os.remove(results)
    
#recreating the sizes file
f=open(sizes,"w+")

#filling the file with the sizes
while True:
    val = int(input("Enter the number of rows in the first matrix: "))
    if (val>0):
        break
    else:
        print("Negative values are not accepted")
f.write(str(val)+'\n')
while True:
    val = int(input("Enter the number of columns in the first matrix: "))
    if (val>0):
        break
    else:
        print("Negative values are not accepted")
f.write(str(val)+'\n')
while True:
    val = int(input("Enter the number of rows in the second matrix: "))
    if (val>0):
        break
    else:
        print("Negative values are not accepted")
f.write(str(val)+'\n')
while True:
    val = int(input("Enter the number of columns in the second matrix: "))
    if (val>0):
        break
    else:
        print("Negative values are not accepted")
f.write(str(val)+'\n')
f.close()
