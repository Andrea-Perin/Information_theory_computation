
#%%
import subprocess as sub
import os
import argparse

# accepting command line args
parser = argparse.ArgumentParser(description='Insert the arguments for running the')
parser.add_argument("--size", default=2000, type=int, help="The size of each matrix.")
parser.add_argument("--num", default=50, type=int, help="The number of matrices.")
parser.add_argument("--en_sep", type=bool, default=True, 
                    help="Whether to store the results in a separate folder.")
parser.add_argument("--en_window", type=bool, default=True, 
                    help="Enabling the computation of the local average.")
parser.add_argument("--en_diag", type=bool, default=False, 
                    help="Whether to also generate spacings for a diagonal random matrix.")
args = parser.parse_args()




# setting some folder specific information
cwd = os.getcwd()
diag = args.en_diag
exec_name = 'spacing_generator.x'
if (diag):
    result_file_name = 'spacings_diag_'+str(args.size)+'.txt'
else:
    result_file_name = 'spacings_herm_'+str(args.size)+'.txt'

# if results are to be stored in an external folder
separate_folder = args.en_sep
separate_folder_name = 'Results'
if (separate_folder):
    if not (os.path.exists(os.path.join(cwd,separate_folder_name))):
        os.mkdir(os.path.join(cwd,separate_folder_name))
    result_file_name=os.path.join(separate_folder_name,result_file_name)

# preparing the list of wanted parameters
num_executions = args.num
matrix_size = args.size
local_average = args.en_window # whether to calculate the local average
local_windows = [str(int(matrix_size/i)) for i in [100,50,10,5,1]] # the windows for the local average

# creating the results files
if (diag):
    if (local_average):
        print("Diag on, local on")
        with open(result_file_name, "a+") as f:
            for n in range(num_executions):
                sub.run(['./'+exec_name,str(matrix_size),'D',local_windows[0],
                         local_windows[1],local_windows[2],local_windows[3],
                         local_windows[4]],universal_newlines=True,stdout=f)


    else:
        print("Diag on, local off")
        with open(result_file_name, "a+") as f:
            for n in range(num_executions):
                sub.run(['./'+exec_name,str(matrix_size),'D',local_windows[4]],
                         universal_newlines=True,stdout=f)

else:
    if(local_average):
        print("Diag off, local on")
        with open(result_file_name, "a+") as f:
            for n in range(num_executions):
                sub.run(['./'+exec_name,str(matrix_size),'N',local_windows[0],
                         local_windows[1],local_windows[2],local_windows[3],
                         local_windows[4]],universal_newlines=True,stdout=f)

    else:
        print("Diag off, local off")        
        with open(result_file_name, "a+") as f:
            for n in range(num_executions):
                sub.run(['./'+exec_name,str(matrix_size),'N',local_windows[4]],
                         universal_newlines=True,stdout=f)
