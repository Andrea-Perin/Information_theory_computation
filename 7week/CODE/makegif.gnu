##################
# CREATING A GIF #
##################

reset
set term gif animate font "TeX Gyre Pagella, 15" size 960,690
set output "animate.gif"

#setting the number of frames
tot_time = numframes
time=0

idx=1

#setting filenames from command line
prob_filename = 'probs_001.dat'
real_filename = 'real_001.dat'
imag_filename = 'img_001.dat'
pot_filename = 'potential.dat'

#setting range from command line
x_low = xl
x_up = xu
set xrange [x_low:x_up]

#creating the gif 
load "gnuscript.gnu"
set output
