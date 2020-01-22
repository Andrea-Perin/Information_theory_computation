#########################
# PLOTTING WITH PALETTE #
#########################


# general, common settings
reset
set terminal pdfcairo enhanced color dashed font "TeX Gyre Pagella, 30" rounded size 16 cm, 10 cm
set encoding utf8
set palette defined (0 '#220000',  1 '#ff0000')
#load 'moreland.pal'
set pm3d
set grid


#setting the output and the input from command line
fname=outname
prob_filename = inname
set output fname


#setting the number of lines
numframes = numframes
evr = every


#setting range from command line
x_low = xl
x_up = xu
set xrange [x_low:x_up]


#setting ranges and labels
stats prob_filename using 2 nooutput
ylab=STATS_max
set yrange[0:ylab+0.2]
set xlabel 'x' offset 0,1,0
set ylabel '|{/Symbol Y}(x)|^2' offset 2.5,0,0
set cblabel 'Time index'


#setting the color range
set cbrange [0:numframes]
plot for [i=1:numframes:evr] prob_filename u 1:(column(i+1)) with lines ls 1 lc palette cb (i) notitle

