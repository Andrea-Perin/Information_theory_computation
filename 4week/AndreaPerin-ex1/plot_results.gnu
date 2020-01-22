do for [i=0:3] {
   set term png
   set output 'results_opt_O'.i.'.png'
   set title 'Comparison of algorithms, O'.i.' optimization'
   set xlabel 'Size of the matrix'
   set ylabel 'Time (s)'
   set format y "%2.0t{/Symbol \264}10^{%L}"
   set logscale y
   set grid
   set key right bottom title 'Legend' box
   plot 'row-O'.i.'.txt' u 1:2 with linespoints lw 2 title 'Horizontal', \
   	'col1-O'.i.'.txt' u 1:2 with linespoints lw 2 title 'Vertical, few skips', \
   	'col2-O'.i.'.txt' u 1:2 with linespoints lw 2 title 'Vertical, many skips', \
   	'matmul-O'.i.'.txt' u 1:2 with linespoints lw 2 title 'MATMUL'
}
