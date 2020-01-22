#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Nov  8 11:48:17 2019

@author: andrea
"""

import numpy as np
import os
import argparse

# accepting command line args
parser = argparse.ArgumentParser(description='Insert the name of the file to'+ 
                                 'split into its components.')
parser.add_argument("--name", type=str, help="The name of the file to split.")
args = parser.parse_args()

# checking whether the file exists
mat_size=args.name.split("_")[-1][:-4]
windows= [str(int(mat_size)//i) for i in [100,50,10,5,1]] 
cwd=os.getcwd()
if not (os.path.exists(os.path.join(cwd,args.name))):
    print("ERROR: no such file in the directory!")
else:
    res=np.loadtxt(os.path.join(cwd,args.name))
    name_file=os.path.join(cwd,args.name).split("/")[-1]
    for n in range(res.shape[1]):
        with open('Results/'+name_file[:-4]+'_'+windows[-1-n]+name_file[-4:], "w+") as f:
            np.savetxt(f,res[:,n])