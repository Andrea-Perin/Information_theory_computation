#%%
import sys
import os
import subprocess as sub
import numpy as np

# parameters for the simulations
N_LAMBDA = 20
MAX_LAMBDA = 3

# lists of params to be used
lmbd = [str(MAX_LAMBDA*(i/N_LAMBDA)) for i in range(N_LAMBDA+1)]
number = 8

# names of executables
exe = 'state_comp.x'    

# launching stuff
states = np.empty((20,2**number))
with open('probs.dat', "w+") as f:   
    for idx,lamb in enumerate(lmbd):
        sub.run(['./'+exe,str(number),lamb], stdout=f)
states=np.loadtxt('probs.dat')[:,1:]

#drawing the 5 most likely states
k=15 
for i in range(states.shape[0]):
    ind = (-states[i,:]).argsort()[:k]
    print("")
    print("LAMBDA: ",lmbd[i])
    for j in ind:
        print(str(bin(j))[2:].zfill(number), '\t', '{:.4e}'.format(states[i,j]))

