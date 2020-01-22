import sys
import os
import subprocess as sub
import argparse

# accepting command line args
parser = argparse.ArgumentParser(description='Insert the parameters of the 1D '+ 
                                 'time dependent Schrodinger equation with ' +
 				'harmonic potential.')
parser.add_argument("-k", type=int, default=1, help="The number of eigenvectors to return.")
parser.add_argument("-N", type=int, default=1000, help="The number of points for the"+
                    "discretization.")
parser.add_argument("-num_tsteps", type=int, default=250, help="The number of time steps "+
							"used in the evolution of the state.")
parser.add_argument("-omega", type=float, default=1.0, help="The harmonic "+
                    "potential multiplicative constant (homogenized version).")
parser.add_argument("-x_low", type=float, default=-10.0, help="The lower extreme.")
parser.add_argument("-x_up", type=float, default=10.0, help="The upper extreme.")
parser.add_argument("-tot_time", type=float, default=0.001, help="The total time for which to "+
								"evolve the system.")

args = parser.parse_args()

if (args.k >= args.N):
    print("Insufficient number of discretization points.")
    sys.exit()

# some parameter names
N=str(args.N)
k=str(args.k)
num_tsteps=str(args.num_tsteps)
omega=str(args.omega)
x_low=str(args.x_low)
x_up=str(args.x_up)
tot_time=str(args.tot_time)

#creating the results folders
cwd=os.getcwd()
if not os.path.exists(os.path.join(cwd,'Gifs')):
    os.makedirs(os.path.join(cwd,'Gifs'))

#running the execs
#sub.run('sh compile.sh', shell=True)
sub.run(['./try.x',N,k,num_tsteps,x_low,x_up,omega,tot_time])
sub.run(['gnuplot -e "xl='+x_low+'" -e "xu='+x_up+'" -e "numframes='+num_tsteps+'" '+'makegif.gnu'], shell=True)

#moving gifs
gifs = [f for f in os.listdir(cwd) if (os.path.isfile(os.path.join(cwd,f)) and f[-4:]=='.gif')]
for f in gifs:
    os.rename(os.path.join(cwd,f),os.path.join(cwd,'Gifs',f[:-4]+'_'+N+'_'+omega+'_'+tot_time+'_'+num_tsteps+f[-4:]))

# the other gifs too
sub.run(['gnuplot -e "xl='+x_low+'" -e "xu='+x_up+'" -e "numframes='+num_tsteps+'" '+'makegif2.gnu'], shell=True)

#moving gifs
gifs = [f for f in os.listdir(cwd) if (os.path.isfile(os.path.join(cwd,f)) and f[-4:]=='.gif')]
for f in gifs:
    os.rename(os.path.join(cwd,f),os.path.join(cwd,'Gifs',f[:-4]+'_'+N+'_'+omega+'_'+tot_time+'_'+num_tsteps+'_2'+f[-4:]))

#moving .dat files in the results
files = [f for f in os.listdir(cwd) if (os.path.isfile(os.path.join(cwd,f)) and f[-4:]=='.dat')]
if not os.path.exists(os.path.join(cwd,'Numerical_results'+'_'+N+'_'+omega+'_'+tot_time+'_'+num_tsteps)):
    os.makedirs(os.path.join(cwd,'Numerical_results'+'_'+N+'_'+omega+'_'+tot_time+'_'+num_tsteps))
for f in files:
    os.rename(os.path.join(cwd,f),os.path.join(cwd,'Numerical_results'+'_'+N+'_'+omega+'_'+tot_time+'_'+num_tsteps,f))



