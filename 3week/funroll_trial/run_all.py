import subprocess as sub
import os
import matplotlib.pyplot as plt
import numpy as np

sizes='matinfo.txt'
modules=['debug_module.f90']
results=['res_noopt.txt','res_opt1.txt','res_opt2.txt','res_opt3.txt']
execs = ['matmul_no_opt.x','matmul_opt1.x','matmul_opt2.x','matmul_opt3.x']
dims = [2,10,100,200,500,1000,2000]
optimizations = ['-O0','-O1','-O2','-O3']

#removing the previous results and execs if they are still there
if os.path.exists(sizes):
    os.remove(sizes)
for name in results:
    if os.path.exists(name):
        os.remove(name)
    if os.path.exists(name[:-4]+'.pdf'):
        os.remove(name[:-4]+'.pdf')
    
for name in execs:
    if os.path.exists(name):
        os.remove(name)

#creating the execs
for opt,exe in zip(optimizations,execs):
    sub.run(['gfortran','my_matmul_proper.f90',' '.join(modules),opt,'-o',exe])

#creating the results files
for x,r in zip(execs,results):
    for i in dims:
        with open(sizes,"w+") as filee:
            filee.write(str(i)+'\n'+str(i)+'\n'+str(i)+'\n'+str(i))
        if os.path.exists(x):
            res = sub.run(['./'+x], stdout=sub.PIPE)
            res = res.stdout.decode('utf-8').split()
            with open(r,"a+") as filee:
                for number in res:
                    filee.write(number+'\t')
                filee.write('\n')
        os.remove(sizes)

#plotting the results
plt.rc('xtick',labelsize=15)
plt.rc('ytick',labelsize=15)
algs = ['Horizontal', 'Vertical, few skips', 'Vertical, many skips', 'MATMUL']
for r,opt in zip(results,optimizations):
    plt.figure(figsize=(10,8))
    plt.title("Comparison between algorithms\nOptimization level: "+opt, fontsize=30)
    plt.xlabel("Dimension of one side", fontsize=25)
    plt.ylabel("log(Time (s))", fontsize=25)
    plt.yscale("log")
    plt.grid()
    data = np.loadtxt(r)
    for idx,alg in enumerate(algs):
        plt.plot(dims,data[:,idx], 'o-', label=alg)
    plt.legend(fontsize=20)
    plt.savefig(r[:-4]+'.pdf')





