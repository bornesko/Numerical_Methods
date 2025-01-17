 set title "Convergence"
 set logscale x
 set xlabel "Number of Points [n]"
 set ylabel "Integration [U^3]"
 plot "task_2/convergence.txt" using 1:2:(sprintf("(%g)",$2)) with labels offset 1,1 point pt 2, "task_2/convergence.txt" with line lt rgb "#00008b"
 pause -1 "Hit return to continue"
