#%%
import sys
import os
import subprocess as sub

# parameters for the simulations
N_MAX = 11
N_LAMBDA = 20
MAX_LAMBDA = 3

# lists of params to be used
NN = [str(i) for i in range(3,N_MAX+1)]
lmbd = [str(MAX_LAMBDA*(i/N_LAMBDA)) for i in range(N_LAMBDA+1)]

# names of executables
exe = 'ex9.x'    
gnuscript='gnuscript.gnu'

# launching stuff
for number in NN:
    with open('res_'+number+'.dat', "w+") as f:
        for lamb in lmbd:
            sub.run(['./'+exe,number,lamb], stdout=f)
    sub.run(['gnuplot -e "num='+number+'" '+gnuscript], shell=True)    
    print("Done number ", number) 

# creating the plots folder if it does not exist
cwd = os.getcwd()
plt_folder='plots'
if not os.path.exists(os.path.join(cwd,plt_folder)):
    os.makedirs(os.path.join(cwd,plt_folder))
# creating the numerical results folder if it does not exist
res_folder='num_res'
if not os.path.exists(os.path.join(cwd,res_folder)):
    os.makedirs(os.path.join(cwd,res_folder))

# listing files
plots = [f for f in os.listdir(cwd) if os.path.isfile(os.path.join(cwd,f)) and f[-4:]=='.pdf']
res = [f for f in os.listdir(cwd) if os.path.isfile(os.path.join(cwd,f)) and f[-4:]=='.dat']

#moving the results in the appropriate folder
for f in plots:
    os.rename(os.path.join(cwd,f),os.path.join(cwd,plt_folder,f))
for f in res:
    os.rename(os.path.join(cwd,f),os.path.join(cwd,res_folder,f))

