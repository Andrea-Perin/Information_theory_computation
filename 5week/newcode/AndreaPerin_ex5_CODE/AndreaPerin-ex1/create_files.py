#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Nov  9 10:39:03 2019

@author: andrea
"""
#%%
import os
import numpy as np
import subprocess as sub

results_folder ='Results'
cwd=os.getcwd()
filenames=['herm_results.dat','diag_results.dat']
specifics=['H','D']
exec_name='spacing_2000_60_5.x'

# creating results folder if it does not exist
if not (os.path.exists(os.path.join(cwd,results_folder))):
    os.makedirs(os.path.join(cwd,results_folder))
    
# launching the exec, splitting files
for name,spec in zip(filenames,specifics):
    with open(os.path.join(cwd,results_folder,name), "w+") as f:
        sub.run(['./'+exec_name,spec], stdout=f)

    res = np.loadtxt(os.path.join(cwd,results_folder,name))
    np.savetxt(os.path.join(cwd,results_folder,name[:-4]+'_s'+'.dat'),res[:,0])    
    np.savetxt(os.path.join(cwd,results_folder,name[:-4]+'_delta'+'.dat'),res[:,2:])
    np.savetxt(os.path.join(cwd,results_folder,name[:-4]+'_s_loc'+'.dat'),res[:,1])    
    os.remove(os.path.join(cwd,results_folder,name))
    
    