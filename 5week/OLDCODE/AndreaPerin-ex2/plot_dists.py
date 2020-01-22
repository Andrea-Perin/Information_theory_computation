#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Nov  8 14:03:27 2019

@author: andrea
"""

import subprocess as sub
import os
from os.path import join,isfile 

# setting the name of the settings file and of the gnuplot script
settings='settings.txt'
cwd = os.getcwd()
gnuscript = 'fit_general.gnu'
fld = 'Results'

# eliminating any previous instance of the file 'settings.txt'
if (os.path.exists(join(cwd,settings))):
    os.remove(join(cwd,settings))

# creating a list of the DATA files in the current directory
files_to_fit = [f for f in os.listdir(join(cwd,fld)) if 
                (isfile(join(cwd,fld,f)) and (f[:4]=='dist'))]

    
# creating the settings file, which will be used by the gnuplot script to
#set all the info
for file in files_to_fit:
    #creating the settings file
    with open(join(cwd,settings), 'w+') as opts:
        opts.write('\''+join(fld,file)+'\'\n') #the input file for data
        opts.write('\''+file[:-4]+'\'\n') #the title of the log
        title=file.split("_")
        alph_val=(title[2]!='diag')*1+1e-10
        title=("Diagonal, "*(title[2]=='diag')+"Hermitian, "*(title[2]=='herm'))+'size '+title[3]+','+'local average '+title[4][:-4]
        opts.write('\''+title+'\'\n') #the title of the plot
        opts.write(str(alph_val))
    # calling the gnuplot script, then removing the settings file
    sub.run(['gnuplot',join(cwd,gnuscript)])
    os.remove(join(cwd,settings))
