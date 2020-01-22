################
# PLOTTING ALL #
################


# general, common settings
reset
set terminal pdfcairo enhanced color dashed font "TeX Gyre Pagella, 20" rounded size 16 cm, 10 cm
set encoding utf8
set grid

#filenames
firstname = 'num_res/res_RG_'
ext = '.dat'

#setting the output file
set output 'plots/plot_all.pdf'

#setting ranges and labels
set title 'Comparison of N'
set xlabel '{/Symbol l}' offset 0,0.5,0
set ylabel 'e' offset 2,0,0
set yrange [-4:0]

#setting the key and linestyles
set key top right
set border linewidth 1.5

#plotting sims and mean field
mf(x) = abs(x)<2 ? -1-(x/2.0)**2 : -abs(x)
plot mf(x) lc rgb '#000000' lw 1 title 'MF', for [i=2:4] firstname.i.ext u 1:2 with lines lw 1 title 'e^{'.i.'}_{RG}({/Symbol l})'


########################
# PLOTTING RG MINUS MF #
########################
# general, common settings
reset
set terminal pdfcairo enhanced color dashed font "TeX Gyre Pagella, 20" rounded size 16 cm, 10 cm
set encoding utf8
set grid

#filenames
firstname = 'num_res/res_RG_'
ext = '.dat'

#setting the output file
set output 'plots/plot_diffs.pdf'

#setting ranges and labels
set title 'Difference between MF and RSRG'
set xlabel '{/Symbol l}' offset 0,0.5,0
set ylabel 'e' offset 2,0,0

#setting the key and linestyles
set key top right
set border linewidth 1.5

#plotting sims and mean field
mf(x) = abs(x)<2 ? -1-(x/2.0)**2 : -abs(x)
plot for [i=2:4] firstname.i.ext u 1:(column(2)-mf(column(1))) with lines lw 1 title '[e_{MF}-e_{RG}^{(N='.i.')}]({/Symbol l})'

