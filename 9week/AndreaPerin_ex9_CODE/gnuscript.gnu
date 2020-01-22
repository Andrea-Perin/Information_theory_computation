#########################
# PLOTTING WITH PALETTE #
#########################


# general, common settings
reset
set terminal pdfcairo enhanced color dashed font "TeX Gyre Pagella, 20" rounded size 16 cm, 10 cm
set encoding utf8
set grid

#getting command line params and setting the output and the input filenames
number=num
inputfile = 'res_'.number.'.dat'
outputfile = 'plot_'.number.'.pdf'

#setting the output file
set output outputfile

#setting ranges and labels
set xlabel '{/Symbol l}' offset 0,0.7,0
set ylabel 'e' offset 2,0,0

#setting the key
set key bottom left

#plotting sims and mean field
f(x) = x<2 ? -1-(x/2.0)**2 : -abs(x)
plot for [k=1:5] inputfile u 1:(column(k+1)) lw 3 w l title 'e_{'.k. '}({/Symbol l})', f(x) lc rgb '#000000' lw 3 title 'MF'

