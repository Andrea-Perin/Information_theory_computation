#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Nov 14 13:56:34 2019

@author: andrea
"""

#%%
import sys
import os
import subprocess as sub
import argparse

# accepting command line args
parser = argparse.ArgumentParser(description='Insert the parameters of the 1D '+ 
                                 'Schrodinger equation with harmonic potential.')
parser.add_argument("-k", type=int, default=5, help="The number of eigenvalues "+
                    "and eigenvectors to return.")
parser.add_argument("-N", type=int, default=100, help="The number of points for the"+
                    "discretization.")
parser.add_argument("-omega", type=float, default=2**(.5), help="The harmonic "+
                    "potential multiplicative constant.")
parser.add_argument("-x_low", type=float, default=-1.0, help="The lower extreme.")
parser.add_argument("-x_up", type=float, default=1.0, help="The upper extreme.")
parser.add_argument("-mass", type=float, default=None, help="The mass of the particle.")

args = parser.parse_args()

if (args.k >= args.N):
    print("Inusfficient number of discretization points.")
    sys.exit()

# some parameter names
N=str(args.N)
k=str(args.k)
omega=str(args.omega)
x_low=str(args.x_low)
x_up=str(args.x_up)
mass=str(args.mass)
    
# names of executables
solver='solver.x'    
hsolver='hsolver.x'    
gnuscript='gnuscript.gnu'
gscript2 = 'gnuscript_compare.gnu'

# running the solvers
sub.run(['./'+solver,N,omega,x_low,x_up])
sub.run(['./'+hsolver,N,k,omega,x_low,x_up])

#names of result files
cwd=os.getcwd()
egval = 'eigvals.dat'
egvec = 'eigvecs.dat'
probs = 'probs.dat'
egvect = 'hermite.dat'
probst = 'exact_probs.dat'
files = [egval,egvec,probs,egvect,probst]


# creating the plots folder if it does not exist
plt_folder='Plots'
os.mkdir(os.path.join(cwd,plt_folder))

# running the gnuplot script
sub.run(['gnuplot -e "num='+k+'" '+gnuscript], shell=True)


# names of the folder where to put the results; contains info about them
# FORMAT: number of points, value of omega, lower extreme, upper extreme
res_folder = 'Numerical_results_'+'_'.join([N,omega[:5],x_low,x_up])
if not os.path.exists(os.path.join(cwd,res_folder)):
    os.mkdir(os.path.join(cwd,res_folder))
    
#moving the results in the appropriate folder
for f in files:
    os.rename(os.path.join(cwd,f),os.path.join(cwd,res_folder,f))

plt_folder = 'Plots_'+'_'.join([N,omega[:5],x_low,x_up])
os.rename(os.path.join(cwd,'Plots'),os.path.join(cwd,plt_folder))
