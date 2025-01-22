 set title "Convergence"
 set logscale x 2
 set xlabel "Number of Points [n]"
 set ylabel "Integration [U^3]"
 set style line 1 lt rgb "#DC143C" lw 2
 set style line 2 lt rgb "#00008b" dt 3 lw 2
 plot "task_2/convergence.txt" using 1:2:(sprintf("(%g)",$2)) with labels offset 1,1 point pt 2 notitle, "task_2/convergence.txt" with lines ls 2 title "Convergence Behaviour ", "task_2/reference.txt" using 1:2:(sprintf("(%g)",$2)) with labels offset -1,-1 notitle, "task_2/reference.txt" with lines ls 1 title "Reference "
 pause -1 "Hit return to continue"
