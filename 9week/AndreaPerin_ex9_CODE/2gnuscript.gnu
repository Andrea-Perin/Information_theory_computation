#########################
# PLOTTING WITH PALETTE #
#########################


# general, common settings
reset
set terminal pdfcairo enhanced color dashed font "TeX Gyre Pagella, 20" rounded size 16 cm, 10 cm
set encoding utf8
set grid
set palette defined (0 '#220000',  1 '#ff0000')

#getting command line params and setting the output and the input filenames
outputfile = 'all.pdf'

#setting the output file
set output outputfile

#setting ranges and labels
set xlabel '{/Symbol l}' offset 0,0.7,0
set ylabel 'e' offset 2,0,0
set cblabel 'N'

#setting the key
set key bottom left

#plotting sims and mean field
f(x) = x<2 ? -1-(x/2.0)**2 : -abs(x)
set cbrange [3:12]
plot for [k=3:12] 'res_'.k.'.dat' u 1:2 lw 3 lc palette cb (k) w l notitle, f(x) lc rgb '#0000ff' lw 3 title 'MF'

