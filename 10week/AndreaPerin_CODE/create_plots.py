import sys
import os
import subprocess as sub

# parameters for the simulations
N_LAMBDA = 100
MAX_LAMBDA = 3
NN_max = 4
NN = [str(i) for i in range(2,NN_max+1)]

# lists of params to be used
lmbd = [str(MAX_LAMBDA*(i/N_LAMBDA)) for i in range(0, N_LAMBDA+1)]

# names of executables
exe = 'ex10_RG.x'    
gnuscript='gnuscript.gnu'
gnuscript2='gnuscript2.gnu'

# launching stuff
for num in NN:
    with open('res_RG_'+num+'.dat', "w+") as f:
        for lamb in lmbd:
            sub.run(['./'+exe,lamb,num], stdout=f)
        sub.run(['gnuplot -e "number='+num+'" '+gnuscript], shell=True)    
        print("Done with ", num) 
sub.run(['gnuplot '+gnuscript2], shell=True)    
        
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

