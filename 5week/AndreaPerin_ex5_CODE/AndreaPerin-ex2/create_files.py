#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Nov  9 16:36:35 2019

@author: andrea
"""
#%%

#%%
import os
import subprocess as sub
import argparse

# accepting command line args
parser = argparse.ArgumentParser(description='Insert the params of the files of'+ 
                                 ' which to compute the distribution.')
parser.add_argument("-nbins", type=int, default=60, help="The number of bins.")
parser.add_argument("-low", type=float, default=0., help="The lower extreme.")
parser.add_argument("-up", type=float, default=5.0, help="The upper extreme.")
args = parser.parse_args()


# input data 
input_folder='Data'

# results folders
results_folder ='Numerical_results'
plots_folder='Plots'
fit_folder='Fit_results'

# input file names
cwd=os.getcwd()
filenames_delta=['herm_results_delta.dat','diag_results_delta.dat']

filenames_s=['herm_results_s.dat','diag_results_s.dat']

exec_dist='generate_dist.x'
exec_dist_kde='generate_dist_kde.x'
exec_avg_r='generate_avg_r.x'
gnuscript='fit_general.gnu'
gnuscript_kde='fit_general_kde.gnu'

# result file names
result_dist = ['herm_dist.dat','diag_dist.dat']
result_dist_kde = ['herm_dist_kde.dat','diag_dist_kde.dat']
result_r = ['herm_avg_r.dat','diag_avg_r.dat']

# distributions params
nbins=str(args.nbins)
low=str(args.low)
up=str(args.up)

# creating results folders if they do not exist
if not (os.path.exists(os.path.join(cwd,results_folder))):
    os.makedirs(os.path.join(cwd,results_folder))
if not (os.path.exists(os.path.join(cwd,plots_folder))):
    os.makedirs(os.path.join(cwd,plots_folder))
if not (os.path.exists(os.path.join(cwd,fit_folder))):
    os.makedirs(os.path.join(cwd,fit_folder))
    
# launching the exec to calc the distribution
for name,eman in zip(filenames_s,result_dist):
    with open(os.path.join(cwd,results_folder,eman), "w+") as f:
        sub.run(['./'+exec_dist,os.path.join(cwd,input_folder,name),
                 nbins,low,up],stdout=f)
    

# launching the exec to calc the distribution with kde method    
for name,eman in zip(filenames_s,result_dist_kde):
    with open(os.path.join(cwd,results_folder,eman), "w+") as f:
        sub.run(['./'+exec_dist_kde,os.path.join(cwd,input_folder,name),
                 str(1000),low,up], stdout=f)
    
# launching the exec to calc the average r
for name,eman in zip(filenames_delta,result_r):
    with open(os.path.join(cwd,results_folder,eman), "w+") as f:
        sub.run(['./'+exec_avg_r,os.path.join(cwd,input_folder,name)], 
                 stdout=f)
    
# setting the name of the settings file and of the gnuplot script
settings='settings.txt'

# eliminating any previous instance of the file 'settings.txt'
if (os.path.exists(os.path.join(cwd,settings))):
    os.remove(os.path.join(cwd,settings))

# creating the settings file, which will be used by the gnuplot script to
#set all the info
for file in result_dist:
    #creating the settings file
    with open(os.path.join(cwd,settings), 'w+') as opts:
        #the input file for data
        opts.write('\''+os.path.join(cwd,results_folder,file)+'\'\n')
        if (file=='diag_dist.dat'):
            opts.write('\'Diagonal\'\n') #the title of the plot
        else:
            opts.write('\'Hermitian\'\n') #the title of the plot
    # calling the gnuplot script, then removing the settings file
    sub.run(['gnuplot',os.path.join(cwd,gnuscript)])
    os.remove(os.path.join(cwd,settings))

for file in result_dist_kde:
    #creating the settings file
    with open(os.path.join(cwd,settings), 'w+') as opts:
        #the input file for data
        opts.write('\''+os.path.join(cwd,results_folder,file)+'\'\n') 
        if (file=='diag_dist_kde.dat'):
            opts.write('\'Diagonal\'\n') #the title of the plot
        else:
            opts.write('\'Hermitian\'\n') #the title of the plot
    # calling the gnuplot script, then removing the settings file
    sub.run(['gnuplot',os.path.join(cwd,gnuscript_kde)])
    os.remove(os.path.join(cwd,settings))
    

# now with local averaged stuff
filename_s_loc = ['herm_results_s_loc.dat','diag_results_s_loc.dat']    
result_dist_loc = ['herm_dist_loc.dat','diag_dist_loc.dat']
result_dist_kde_loc = ['herm_dist_kde_loc.dat','diag_dist_kde_loc.dat']

# launching the exec to calc the distribution locally
# with histogram
for name,eman in zip(filename_s_loc,result_dist_loc):
    with open(os.path.join(cwd,results_folder,eman), "w+") as f:
        sub.run(['./'+exec_dist,os.path.join(cwd,input_folder,name),
                 nbins,low,up],stdout=f)
# with kde
for name,eman in zip(filename_s_loc,result_dist_kde_loc):
    with open(os.path.join(cwd,results_folder,eman), "w+") as f:
        sub.run(['./'+exec_dist,os.path.join(cwd,input_folder,name),
                 nbins,low,up],stdout=f)

sub.run(['gnuplot','fit_local.gnu'])    
    
    
    
    
    
    
    
    
    