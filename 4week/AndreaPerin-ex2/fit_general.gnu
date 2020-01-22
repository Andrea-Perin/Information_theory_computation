reset

# getting the name of the input file from the settings.txt file
input_file = `sed -n 1p settings.txt`

# getting the title of the plot and of the fit from settings.txt
tit =  `sed -n 2p settings.txt`

# setting the name of the log file
logfilename = tit.'_FIT.log'

# to create some margins
stats input_file u 2 nooutput
y_min = STATS_min
y_max = STATS_max

stats input_file u 1 nooutput
x_min = STATS_min
x_max = STATS_max


# setting pdf as output
set terminal pdfcairo
set title tit

# setting the output file for the log; no on screen output either.
set fit logfile logfilename quiet

# setting the output pdf
set output sprintf("%s%s", tit, '.pdf')

# setting some aesthetic options
set grid
set xlabel 'Size of the matrix'
set ylabel 'Time (s)'
set format y "%2.0t{/Symbol \264}10^{%L}"
set key top left title 'Legend' box
set autoscale xy

# defining the fitting function and fitting
a=0.1
b=1e-10
c=3
f(x)=a+b*x**c
fit f(x) input_file using 1:2 via a,b,c
b_exp = floor(log10(b))
b_mant = b/(10**b_exp) 
fnct = sprintf("%.2f+(%1.2f{/Symbol \264}10^{%2.f})x^{%.2f}", a,b_mant,b_exp,c)


set yrange [y_min-(y_max-y_min)*0.05:\
    	   y_max+(y_max-y_min)*0.05]
set xrange [x_min-(x_max-x_min)*0.05:\
    	   x_max+(x_max-x_min)*0.05]

# plotting everything
plot input_file using 1:2 title "Times", f(x) title 'f(x)='.fnct
