import subprocess as sub
import os
import matplotlib.pyplot as plt
import numpy as np


#asking the user to insert the dimensions
N_min = 0
while (N_min<=0):
    # catching eventual non-integer values.
    try:
        N_min = int(input("Please enter the minimum size: "))
    except ValueError:
        print("Invalid value: integer needed.")
    # if the integer is non valid
    if (N_min<=0):
        print("Invalid dimension: less or equal to 0.")

N_max = 0
while (N_max<=0 or N_max<N_min):
    # catching eventual non-integer values.
    try:
        N_max = int(input("Please enter the maximum size: "))
    except ValueError:
        print("Invalid value: integer needed.")
    # if the integer is non valid
    if (N_max<=0 or N_max<N_min):
        print("Invalid dimension: N_max should be positive and greater than N_min=",N_min)


# the number of steps from N_min to N_man
num_intermediate = 30


# creating the sizes; logarithmic
dims = np.logspace(np.log10(N_min), np.log10(N_max), num_intermediate, dtype=np.int16)


# file names, modules
source='matmul_performance.f90'
sizes='matinfo.txt'
modules=['debug_module.f90']
gnuplot_script = 'plot_results.gnu'
results=['row.txt','col1.txt','col2.txt','matmul.txt']
execs = ['mm_no_opt.x','mm_opt1.x','mm_opt2.x','mm_opt3.x']
optimizations = ['-O0','-O1','-O2','-O3']


#removing the previous results and execs if they are still there
if os.path.exists(sizes):
    os.remove(sizes)

for name in results:
    if os.path.exists(name):
        os.remove(name)
    for opt in optimizations:
        if os.path.exists(name[:-4]+opt+name[-4:]):
            os.remove(name[:-4]+opt+name[-4:])

for name in execs:
    if os.path.exists(name):
        os.remove(name)


# creating the executables : takes the needed modules and creates an exec named exe
for opt,exe in zip(optimizations,execs):
    sub.run(['gfortran',source,' '.join(modules),opt,'-o',exe])


#creating the results files
for x,opt in zip(execs,optimizations):
    for i in dims:
        with open(sizes,"w+") as filename:
            filename.write(str(i)+'\n'+str(i)+'\n'+str(i)+'\n'+str(i))
        if os.path.exists(x):
            sub.run(['./'+x])
        os.remove(sizes)
    # changing names of the results 
    for r in results:
        os.rename(r,r[:-4]+opt+r[-4:])

# calling the gnuplot script
if os.path.exists(gnuplot_script):
    sub.run(['gnuplot',gnuplot_script])
else:
    print("\nNo gnuplot script detected; no plots were produced!\n")
