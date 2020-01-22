#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Jan  5 18:22:47 2020

@author: andrea
"""

import os
import subprocess as sub

# parameters for the simulations
N_LAMBDA = 100
MAX_LAMBDA = 3

# lists of params to be used
lmbd = [str(MAX_LAMBDA*(i/N_LAMBDA)) for i in range(0, N_LAMBDA+1)]

# names of executables
exe = 'ex10_RG_101.x'    
gnuscript = 'comp.gnu'

# launching stuff
with open('res_RG_101.dat', "w+") as f:
    for lamb in lmbd:
        sub.run(['./'+exe,lamb,str(2)], stdout=f)
sub.run(['gnuplot '+gnuscript], shell=True)        
