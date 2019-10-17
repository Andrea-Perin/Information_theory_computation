reset
set title "Performance of various matrix-matrix multiplication algorithms vs. size"
set terminal svg
set output "performance.svg"
set xlabel "Size of matrix"
set ylabel "log-time (s)"
set logscale y
set key inside bottom right title "Algorithms" box 3
array A[4]
A[1]="Dot product"
A[2]="Horizontal, indexed"
A[3]="Vertical, vectorized"
A[4]="MATMUL()"
plot for [col=2:5] 'results.txt' using 1:col with linespoints title A[col-1]
