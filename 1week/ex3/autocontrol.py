import numpy as np
import os
import subprocess as sub

sizes='matinfo.txt'
results=['res.txt','res_opt1.txt','res_opt2.txt','res_opt3.txt']
execs = ['ex3.x','ex3_opt1.x','ex3_opt2.x','ex3_opt3.x']
dims = [2,10,100,200,500,1000]#,2000]

#removing the previous results if they are still there
if os.path.exists(sizes):
    os.remove(sizes)
for name in results:
    if os.path.exists(name):
        os.remove(name)

for (r,x) in zip(results,execs):
    for i in dims:
        with open(sizes,"w+") as file:
            file.write(str(i)+'\n'+str(i)+'\n'+str(i)+'\n'+str(i))
        if os.path.exists(x):
            sub.run(['./'+x])
        os.remove(sizes)
    sub.run(['gnuplot', 'performance.gnuplot'])
    os.rename('performance.svg', 'perf_'+r[:-4]+'.svg')
    os.rename('results.txt',r)
