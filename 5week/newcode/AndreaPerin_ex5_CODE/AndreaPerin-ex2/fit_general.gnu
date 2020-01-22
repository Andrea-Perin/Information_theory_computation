reset

# getting the name of the input file from the settings.txt file
input_file = `sed -n 1p settings.txt`

# getting the title for the files and the plot
file_tit = `sed -n 2p settings.txt`

# setting the name of the log file 
logfilename = 'Fit_results/'.file_tit.'_FIT.log'

# setting pdf as output
set terminal pdfcairo enhanced color dashed font "TeX Gyre Pagella, 20" rounded size 16 cm, 9.8 cm
set encoding utf8
set title 'Distribution of the spacings: '.file_tit

# setting the output file for the log; no on screen output either.
set fit logfile logfilename quiet errorvariables

# setting the output pdf
set output 'Plots/'.file_tit.'.pdf'


# setting some aesthetic options
unset key
set xlabel 's'
set ylabel 'P(s)'
set key top center
set autoscale xy
set tics out
set style line 12 lc rgb'#808080' lt 0 lw 1
set grid back ls 12

# defining the fitting function and fitting, depending on input file
if (file_tit eq 'Diagonal') {
	a=1
	alpha=0
	alpha_err=0
	b=1
	beta=1
	# fitting the function
	f(x)=a*(exp(-b*x**beta))
	fit f(x) input_file using 1:2 via a,b,beta
} else {
	a=5
	alpha=2
	b=1
	beta=2
	# fitting the function
	f(x)=(a*x**alpha)*(exp(-b*x**beta))
	fit f(x) input_file using 1:2 via a,alpha,b,beta
	surm(x)=(32/3.14**2)*x**2*exp(-(4/3.14)*x**2)
}

# getting a nice vertical placement for the following label
stats input_file using 2 nooutput
ylab=STATS_max
stats input_file using 1 nooutput
xlab=STATS_max

# plotting the parameters and their errors on screen
set label sprintf("a=%1.2f \261 %1.2f \n{/Symbol a}=%1.2f \261 %1.2f \nb=%1.2f \261 %1.2f \n{/Symbol b}=%1.2f \261 %1.2f", a,a_err,alpha,alpha_err,b,b_err,beta,beta_err) at 0.78*xlab,0.95*ylab

# plotting everything
if (file_tit eq 'Diagonal') {
	plot input_file using 1:2 title "Distribution" w l, f(x) title 'Fit'
} else {
	plot input_file using 1:2 title "Distribution" w l, f(x) title 'Fit', surm(x) title 'Theoretical value' lt 2 lc rgb "red"
}

# setting the output pdf
set output 'Plots/'.file_tit.'_diff.pdf'


# plotting the difference between the two
set title 'Difference between fit and distribution, '.file_tit
set xlabel 's'
set ylabel 'P(s)-f(s)'

if (file_tit eq 'Diagonal') {
	plot input_file using 1:($2-f($1)) w l title 'Data-fit difference'
} else {
	plot input_file using 1:($2-f($1)) w l title 'Data-fit difference', input_file using 1:($2-surm($1)) w l title 'Data-surmise difference' lt 2 lc rgb "red"
}


