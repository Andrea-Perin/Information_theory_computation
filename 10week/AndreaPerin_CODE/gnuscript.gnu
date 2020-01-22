#########################
# PLOTTING RG VERSUS MF #
#########################


# general, common settings
reset
set terminal pdfcairo enhanced color dashed font "TeX Gyre Pagella, 20" rounded size 16 cm, 10 cm
set encoding utf8
set grid

#getting command line params and setting the output and the input filenames
num = number
inputfile = 'res_RG_'.num.'.dat'
outputfile = 'plot_RG_'.num.'.pdf'

#setting the output file
set output outputfile

#setting ranges and labels
set title 'N='.num.', number of repetitions: 100'
set xlabel '{/Symbol l}' offset 0,0.5,0
set ylabel 'e' offset 2,0,0
set yrange [-4:0]

#setting the key and linestyles
set key top right
set border linewidth 1.5
set style line 1 lc rgb '#5d76cb' lt 1 lw 2 pt 7 pi -1 ps 0.5
set pointintervalbox 0.1

#plotting sims and mean field
f(x) = abs(x)<2 ? -1-(x/2.0)**2 : -abs(x)
plot f(x) lc rgb '#000000' lw 3 title 'MF', inputfile u 1:2 with linespoints ls 1 title 'e_{RG}({/Symbol l})'

