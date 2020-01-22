#########################
# PLOTTING WITH PALETTE #
#########################


# general, common settings
reset
set terminal pdfcairo enhanced color dashed font "TeX Gyre Pagella, 30" rounded size 16 cm, 10 cm
set encoding utf8
set grid


#setting the output and the input from command line
filename = 'Numerical_results_1024_5.0_5.0_100/movement.dat'
set output 'motion_5_5_100.pdf'


#setting ranges and labels
set xlabel 'Time index' offset 0,0.7,0
set ylabel 'Position of the maximum' offset 2.5,0,0


#setting the color range
set key top left


#fitting

omega=5.
potspeed=5./100.
f(x)=(potspeed)*x+(1./omega)*sin(omega*(potspeed)*x-3.1415)
fit f(x) filename via omega, potspeed

plot filename with lines lc rgb '#0000FF' lw 3 title 'Simulation', filename u 1:(f($1)) dashtype 2 lw 3 lc rgb '#FF0000' w l title 'Ansatz'


#difference, too
set output 'diff_5_5_100.pdf'
set key bottom left

plot filename u 1:($2-(f($1)))  lw 2 w l title 'Difference'
