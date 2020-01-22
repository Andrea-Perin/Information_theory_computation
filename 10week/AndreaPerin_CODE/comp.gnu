#######################
# PLOTTING COMPARISON #
#######################

# general, common settings
reset
set terminal pdfcairo enhanced color dashed font "TeX Gyre Pagella, 20" rounded size 16 cm, 10 cm
set encoding utf8
set grid

#setting the output file
set output 'comp.pdf'

#setting ranges and labels
set title "N=2, number of repetitions: 101 \nvs. N=4, number of repetitions: 100"
set xlabel '{/Symbol l}' offset 0,0.5,0
set ylabel 'e' offset 2,0,0

#setting the key and linestyles
set key top right
set border linewidth 1.5

#plotting sims and mean field
mf(x) = abs(x)<2 ? -1-(x/2.0)**2 : -abs(x)
plot 'num_res/res_RG_4.dat' u 1:(column(2)-mf(column(1))) with lines lw 1 title 'e^{N=4}_{RG}({/Symbol l})', 'res_RG_101.dat' u 1:(column(2)-mf(column(1))) with lines lw 1 title 'e^{N=2}_{RG}({/Symbol l})' 


