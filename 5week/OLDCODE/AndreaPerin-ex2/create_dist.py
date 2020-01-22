#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Nov  8 12:52:24 2019

@author: andrea
"""

import subprocess as sub
import os
from os.path import exists,join,isfile
import argparse

# accepting command line args
parser = argparse.ArgumentParser(description='Insert the params of the file of'+ 
                                 ' which to compute the distribution.')
parser.add_argument("-name", type=str, default='auto', help="The name of the file.")
parser.add_argument("-nbins", type=int, default=0, help="The number of bins.")
parser.add_argument("-low", type=float, default='', help="The lower extreme.")
parser.add_argument("-up", type=float, default='', help="The upper extreme.")
args = parser.parse_args()

# checking whether the file exists
cwd=os.getcwd()
ex = 'generate_dist.x'
if (args.name=='auto'):
    all_files=[f for f in os.listdir('Results') if (isfile('Results/'+f))]
    for fil in all_files:
        with open('Results/dist_'+fil, "w+") as f:
            sub.run(['./'+ex,'Results/'+fil,str(args.nbins),str(args.low),str(args.up)], stdout=f)
else:
    if not (exists(join(cwd,args.name))):
        print("ERROR: no such file in the directory!")
    else:
        fname = args.name.split("/")
        with open(fname[0]+'/dist_'+fname[-1], "w+") as f:
            sub.run(['./'+ex,args.name,str(args.nbins),str(args.low),str(args.up)], stdout=f)
        