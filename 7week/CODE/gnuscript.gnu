### Start multiplot (2x2 layout)
set multiplot layout 2,1 rowsfirst title sprintf("t=%i",time)
set grid

# --- GRAPH a
set yrange[-0.1-(0.5*(2*idx-1)):2]
unset xlabel 
set ylabel '|{/Symbol Y}(x)|^2'
plot prob_filename u 1:(column(time+2)) with lines ls 1 title '|{/Symbol Y}(x)|^2', pot_filename u 1:(column(time+2))-(0.5*(2*idx-1)) with lines ls 1 dashtype 1 lc rgb '#00FF00' title 'V(x)'

# --- GRAPH b
set yrange[-1-(0.5*(2*idx-1)):2]
set xlabel 'x'
set ylabel '{/Symbol Y}(x)'
plot real_filename u 1:(column(time+2)) with lines ls 1 lc rgb '#FF0000' title 'Re[{/Symbol Y}(x)]', imag_filename u 1:(column(time+2)) with lines ls 2 lc rgb '#0000FF' title 'Im[{/Symbol Y}(x)]', pot_filename u 1:(column(time+2))-(0.5*(2*idx-1)) with lines ls 1 dashtype 1 lc rgb '#00FF00' title 'V(x)'


unset multiplot
### End multiplot

time = time + 1

if (time < tot_time) reread
