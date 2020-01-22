reset

# getting the name of the input file from the settings.txt file
input_file = `sed -n 1p settings.txt`

# getting the title of the plot from settings.txt
tit =  `sed -n 3p settings.txt`

# getting the title for the files
fit_tit = `sed -n 2p settings.txt`

# setting the name of the log file 
logfilename = 'Fit_results/'.fit_tit.'_FIT.log'

# setting pdf as output
set terminal pdfcairo enhanced color dashed font "TeX Gyre Pagella, 14" rounded size 16 cm, 9.6 cm
set encoding utf8
set title 'Distribution of the spacings:'.tit

# setting the output file for the log; no on screen output either.
set fit logfile logfilename quiet errorvariables

# setting the output pdf
set output 'Plots/'.fit_tit.'.pdf'

# setting some aesthetic options
unset key
set xlabel 's'
set ylabel 'P(s)'
set key top center
set autoscale xy

# defining the fitting function and fitting
a=1
b=1
alpha=`sed -n 4p settings.txt`
beta=1

# fitting the function
f(x)=(a*x**alpha)*(exp(-b*x**beta))
fit f(x) input_file using 1:2 via a,alpha,b,beta

# plotting the parameters and their errors on screen
set label sprintf("a=%1.2f \261 %1.2f \n{/Symbol a}=%1.2f \261 %1.2f \nb=%1.2f \261 %1.2f \n{/Symbol b}=%1.2f \261 %1.2f", a,a_err,alpha,alpha_err,b,b_err,beta,beta_err) at 2.5,0.6

# plotting everything
plot input_file using 1:2 title "Times" w l, f(x) title 'f(x)'
