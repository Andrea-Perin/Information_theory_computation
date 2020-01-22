reset

# general, common settings
set terminal pdfcairo enhanced color dashed font "TeX Gyre Pagella, 20" rounded size 16 cm, 10 cm
set encoding utf8

resfolder='./Plots/'

k=num+1

###########################
# FIRST PLOT: EIGENVALUES #
###########################


# setting the output pdf
set output resfolder.'eigvals.pdf'


#setting some aesthetic options
unset key
set xlabel 'n'
set ylabel 'E_n'
set autoscale xy
set tics out
set style line 12 lc rgb'#808080' lt 0 lw 1
set grid back ls 12
set title 'Eigenvalues'


#plotting
pl 'eigvals.dat' 


###########################
# SECOND PLOT: EIGENVECTS #
###########################


# setting the output pdf
set output resfolder.'eigvecs.pdf'


#setting some aesthetic options
set key outside
set xlabel 'x'
set ylabel '{/Symbol Y}(x)'
set autoscale xy
set tics out
set style line 12 lc rgb'#808080' lt 0 lw 1
set grid back ls 12
set title 'First '.(k-1).' (discretized) eigenfunctions'


#om=15
#hbar=2#6.62607015e-34
#c=(om*hbar/(2*3.14))**.25
#f(x)=c*exp(-(hbar*om/4)*x**2)



#plotting
pl for [ii=2:k] 'eigvecs.dat' u 1:ii w l title '{/Symbol Y}_{'.(ii-1).'}'

#############################
# THIRD PLOT: PROBABILITIES #
#############################


# setting the output pdf
set output resfolder.'probs.pdf'


#setting some aesthetic options
set key outside
set xlabel 'x'
set ylabel 'P(x)'
set autoscale xy
set tics out
set style line 12 lc rgb'#808080' lt 0 lw 1
set grid back ls 12
set title 'First '.(k-1).' probability densities'


#plotting
pl for [ii=0:k] 'probs.dat' u 1:ii w l title 'P_{'.ii.'}' 


##############################
# FOURTH PLOT: COMP. EIGVECS #
##############################


# setting the output pdf
set output resfolder.'eigvecscompare.pdf'


#setting some aesthetic options
set key outside
set xlabel 'x'
set ylabel '{/Symbol Y}(x)'
set autoscale xy
set tics out
set style line 12 lc rgb'#808080' lt 0 lw 1
set grid back ls 12
set title 'First '.(k-1).' (discretized) eigenfunctions, comparison'


#plotting
pl for [ii=2:k] 'eigvecs.dat' u 1:ii w l title '{/Symbol Y}_{'.(ii-1).'}', for [ii=2:k] 'hermite.dat' u 1:ii lc 0*ii dashtype ii w l title '{/Symbol Y}^{th.}_{'.(ii-1).'}'
 

###########################
# FIFTH PLOT: COMP. PROBS #
###########################


# setting the output pdf
set output resfolder.'probscompare.pdf'


#setting some aesthetic options
set key outside
set xlabel 'x'
set ylabel 'P(x)'
set autoscale xy
set tics out
set style line 12 lc rgb'#808080' lt 0 lw 1
set grid back ls 12
set title 'First '.(k-1).' probability densities, comparison'


#plotting
pl for [ii=2:k] 'probs.dat' u 1:ii w l title 'P_{'.(ii-1).'}', for [ii=2:k] 'exact_probs.dat' u 1:ii lc 0*ii dashtype ii w l title 'P^{th.}_{'.(ii-1).'}' 
