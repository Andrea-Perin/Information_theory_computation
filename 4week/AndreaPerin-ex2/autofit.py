#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Nov  3 20:59:25 2019

@author: andrea
"""

import subprocess as sub
import os
from os.path import join,isfile 

# setting the name of the settings file and of the gnuplot script
settings='settings.txt'
cwd = os.getcwd()
gnuscript = 'fit_general.gnu'

# eliminating any previous instance of the file 'settings.txt'
if (os.path.exists(join(cwd,settings))):
    os.remove(join(cwd,settings))

# creating a list of the DATA files in the current directory
files_to_fit = [f for f in os.listdir(cwd) if (isfile(join(cwd,f)) and
                                      (f[-4:]=='.txt' or f[-4:]=='.dat'))]

    
# creating the settings file, which will be used by the gnuplot script to
#set all the info
for file in files_to_fit:
    #creating the settings file
    with open(settings, 'w+') as opts:
        opts.write('\''+file+'\'\n') #the input file for data
        opts.write('\''+file[:-4]+'\'\n') #the title
    # calling the gnuplot script, then removing the settings file
    sub.run(['gnuplot',join(cwd,gnuscript)])
    os.remove(join(cwd,settings))
